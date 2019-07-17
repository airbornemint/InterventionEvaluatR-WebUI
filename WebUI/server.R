#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(future)
library(promises)
library(shiny)
library(InterventionEvaluatR)
library(uuid)
library(dplyr)
library(ggplot2)

source("analysis.R")
source("common.R")
source("worker.R")
source("results.R")

import::from(magrittr, "%>%")
import::from(shiny, validate)
import::from(plotly, ggplotly, renderPlotly)
import::from(shinyBS, updateButton)
import::from(shinyjs, hidden, toggleElement, toggleClass)
import::from(shinyWidgets, airMonthpickerInput)
import::from(lubridate, "%m+%", "%m-%", days, "day<-")
import::from(ggplot2, geom_blank, geom_errorbarh)
import::from(InterventionEvaluatR, evaluatr.init, evaluatr.univariate, evaluatr.univariate.plot)
import::from(plyr, llply)
import::from(dplyr, filter)

plan(multisession)

check.call = function(args) {
  status = system2(args[1], args[2:length(args)], stdout="", stderr="")
  if (is.null(status) || (is.numeric(status) && status == 0)) {
    return()
  } else {
    stop(sprintf("status = %d", status))
  }
}

check.output = function(args) {
  res = system2(args[1], args[2:length(args)], stdout=TRUE, stderr="")
  status = attr(res, "status", exact=TRUE)
  if (is.null(status) || (is.numeric(status) && status == 0)) {
    return(res)
  } else {
    stop(sprintf("status = %d", status))
  }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  ############################################################
  # Set up reactive data inputs
  ############################################################
  
  userInput = reactiveVal() # Will be list(name, input, params, results)
  
  observe({
    validate(need(input$stockDataset, FALSE))
    md_update_spinner(session, "loadSpinner", visible=TRUE)
    userInput(list(
      name=names(which(stockDatasets == input$stockDataset)),
      input=switch(
        input$stockDataset,
        pnas_brazil = {
          data("pnas_brazil", package="InterventionEvaluatR")
          pnas_brazil
        }
      )
    ))
  })
  
  observe({
    validate(need(input$userDataset, FALSE))
    md_update_spinner(session, "loadSpinner", visible=TRUE)
    updateSelectInput(session, "stockDataset", selected="")
    upload = input$userDataset[1,]
    # We accept rds and csv input. Try rds first.
    tryCatch(
      userInput(c(
        readRDS(upload$datapath),
        name=upload$name)
      ),
      error = function(e) {
        userInput(list(
          input=read.csv(upload$datapath),
          name=upload$name
        ))
      }
    )
  })
  
  # This is the input data for analysis  
  inputData = reactive({
    validate(need(userInput(), FALSE))
    userInput()$input
  })
  
  # Pre-computed results, if uploaded by the user
  inputResults = reactive({
    validate(need(userInput(), FALSE))
    userInput()$results
  })
  
  dataDateColumns = reactive({
    dateColumns(inputData())
  })
  
  dataTime = reactive({
    validate(need(input$dateCol, FALSE))
    validate(need(input$dateFormat, FALSE))
    
    as.Date(inputData()[[input$dateCol]], format=input$dateFormat) 
  })
  
  dataOutcome = reactive({
    validate(need(input$outcomeCol, FALSE))

    if (checkNeed(input$denomCol)) {
      inputData()[[input$outcomeCol]] / inputData()[[input$denomCol]]
    } else {
      inputData()[[input$outcomeCol]]
    }
  })
  
  dataGroup = reactive({
    if (!is.null(input$groupCol)) {
      inputData()[[input$groupCol]]
    }
  })
  
  dataNeedsGroup = reactive({
    length(unique(dataTime())) < length(dataTime())
  })
  
  dataGroupValues = reactive({
    validate(need(dataGroup, FALSE))
    factor(dataGroup())
  })

  dataPostStart = reactive({
    validate(need(input$postStart, FALSE))
    date = as.Date(input$postStart, "%Y-%m-%d")
    day(date) = 1
    date
  })

  dataEvalStart = reactive({
    validate(need(input$postDuration, FALSE))
    date = dataPostStart() %m+% months(as.numeric(input$postDuration))
    day(date) = 1
    date
  })
  
  ############################################################
  # Set up reactive data display
  ############################################################
  
  plotlyOptions = function(plot) {
    plot %>% plotly::config(
      staticPlot=TRUE,
      editable=FALSE,
      scrollZoom=FALSE,
      doubleClick=FALSE,
      showAxisDragHandles=FALSE,
      showLink=FALSE,
      displayModeBar=FALSE,
      showSendToCloud=FALSE,
      displaylogo=FALSE
    )
  }
  
  output$previewPlot = renderPlotly({
    periods = function() {
      if (checkNeed(input$postStart) && checkNeed(input$postDuration)) {
        df = data.frame(
          xmin=c(min(dataTime()), dataEvalStart()),
          xmax=c(dataPostStart(), max(dataTime())),
          y=rep(max(dataOutcome()) * 1.1, 2)
        )
        c(
          geom_segment(data=df, aes(x=xmin, xend=xmax, y=y, yend=y)),
          geom_point(data=df, aes(x=xmin, y=y)),
          geom_point(data=df, aes(x=xmax, y=y))
        )
      } else {
        df = data.frame(
          xmin=min(dataTime()),
          xmax=max(dataTime()),
          y=max(dataOutcome()) * 1.1
        )
        c(
          geom_segment(data=df, aes(x=xmin, xend=xmax, y=y, yend=y), color="#FFFFFF00"),
          geom_point(data=df, aes(x=xmin, y=y), color="#FFFFFF00"),
          geom_point(data=df, aes(x=xmax, y=y), color="#FFFFFF00")
        )
      }
    }

    if (!is.null(dataGroup())) {
      ggplotly(ggplot(
          data.frame(y=dataOutcome(), t=dataTime(), g=dataGroup()) %>% arrange(t)
        ) +
          geom_line(aes(x=t, y=y, group=g), size=0.1) +
          periods() + 
          labs(x=NULL, y=NULL) +
          theme_minimal()
      ) %>% plotlyOptions()
    } else if (dataNeedsGroup()) {
      data = data.frame(y=dataOutcome(), t=dataTime()) %>% arrange(t)
      data %<>% group_by(t) %>% summarize(ymin=min(y), ymax=max(y))
      ggplotly(
        ggplot(data) +
        geom_ribbon(aes(x=t, ymin=ymin, ymax=ymax), size=0.1, fill="grey75") +
        periods() + 
        labs(x=NULL, y=NULL) +
        theme_minimal()
      ) %>% plotlyOptions()
    } else {
      data = data.frame(y=dataOutcome(), t=dataTime()) %>% arrange(t)
      ggplotly(
        ggplot(data) +
        geom_line(aes(x=t, y=y), size=0.1) +
        periods() + 
        labs(x=NULL, y=NULL) +
        theme_minimal()
      ) %>% plotlyOptions()
    }
  })
  outputOptions(output, 'previewPlot', suspendWhenHidden=FALSE)
  
  output$showPreviewPlot = reactive({
    show = checkNeed(input$dateCol) && checkNeed(input$dateFormat) && checkNeed(input$outcomeCol)
    toggleClass(id="page", class="plot-on", condition=show)
    show
  })
  outputOptions(output, 'showPreviewPlot', suspendWhenHidden=FALSE)
  
  ############################################################
  # Set up reactive input controls
  ############################################################
  
  output$dateColUI <- renderUI({
    choices = names(dateColumns(inputData()))
    if (length(choices) > 1) {
      choices = c("", choices)
    }
    
    selectInput(
      inputId = "dateCol",
      label = "Which variable in your data represents time?",
      choices = choices
    )
  })
  outputOptions(output, 'dateColUI', suspendWhenHidden=FALSE)
  
  output$dateFormatUI <- renderUI({
    validate(need(input$dateCol, FALSE))

    choices = (inputData() %>% dateColumns())[[input$dateCol]]
    select = selectInput(
      inputId = "dateFormat",
      label = "Date Format:",
      choices = choices
    )
    
    if (length(choices) == 1) {
      select = hidden(select)
    }
    
    select
  })
  outputOptions(output, 'dateFormatUI', suspendWhenHidden=FALSE)
  
  output$outcomeColUI <- renderUI({
    selectInput(
      inputId = "outcomeCol",
      label = "Outcome:",
      choices = c("", setdiff(names(inputData()), names(dateColumns(inputData()))))
    )
  })
  outputOptions(output, 'outcomeColUI', suspendWhenHidden=FALSE)
  
  output$denomColUI <- renderUI({
    selectInput(
      inputId = "denomCol",
      label = "Denominator:",
      choices = c(`No denominator`="", setdiff(names(inputData()), names(dateColumns(inputData()))))
    )
  })
  outputOptions(output, 'denomColUI', suspendWhenHidden=FALSE)
  
  output$groupColUI <- renderUI({
    if (dataNeedsGroup()) {
      selectInput(
        inputId = "groupCol",
        label = "Group:",
        choices = c(`No grouping`="", setdiff(names(inputData()), names(dateColumns(inputData()))))
      )
    }
  })
  outputOptions(output, 'groupColUI', suspendWhenHidden=FALSE)
  
  output$introDateUI <- renderUI({
    # oldValue = {
    #   if (checkNeed(input$postStart)) {
    #     # There is a weird bug in airMonthpickerInput with minView=months that causes the date picker to set itself to one month earlier than value if value is the first day of the month.
    #     # dataPostStart() %m+% days(15)
    #   }
    # }
    airMonthpickerInput(
      inputId = "postStart",
      label = "When was the vaccine introduced?",
      view="months",
      minView="months",
      minDate=min(dataTime()),
      maxDate=max(dataTime()),
#      maxDate=max(dataTime()) %m-% months(as.numeric(input$postDuration)),
      addon="none",
      autoClose=TRUE#,
      #value=oldValue 
    )
  })
  outputOptions(output, 'introDateUI', suspendWhenHidden=FALSE)
  
  output$analysisGroupsUI = renderUI({
    validate(need(dataGroupValues(), FALSE), need(input$groupCol, FALSE))
    groupValues = levels(dataGroupValues())
    groupNames = sprintf("%s %s", input$groupCol, groupValues)
    
    checkboxGroupInput(
      "analysisGroups",
      "Which groups do you want to include in analysis?",
      choiceNames = groupNames,
      choiceValues = groupValues,
      selected = groupValues
    )
  })
  outputOptions(output, 'analysisGroupsUI', suspendWhenHidden=FALSE)
  
  ############################################################
  # Set up step enabled / disabled state and next buttons
  ############################################################
  
  dateCols = reactive({
    if (!is.null(inputData())) {
      dateColumns(inputData())
    }
  })
  
  observe({
    with(list(dateAvailable=checkNeed(dateCols())), {
      updateButton(session, "nextDate", disabled=!dateAvailable)
      md_update_stepper_step(session, "steps", "date", enabled=dateAvailable)
    })
  })
  
  observeEvent(input$nextDate, {
    md_update_stepper(session, "steps", value="date")
  })
  
  observe({
    with(list(outcomeAvailable=!is.null(dataTime())), {
      updateButton(session, "nextOutcome", disabled=!outcomeAvailable)
      md_update_stepper_step(session, "steps", "outcome", enabled=outcomeAvailable)
    })
  })
  
  observeEvent(input$nextOutcome, {
    md_update_stepper(session, "steps", value="outcome")
  })
  
  observe({
    with(list(periodsAvailable=checkNeed(dataOutcome()) && (!dataNeedsGroup() || checkNeed(dataGroup()))), {
      updateButton(session, "nextPeriods", disabled=!periodsAvailable)
      md_update_stepper_step(session, "steps", "periods", enabled=periodsAvailable)
    })
  })
  
  observeEvent(input$nextPeriods, {
    md_update_stepper(session, "steps", value="periods")
  })

  observe({
    with(list(analysisAvailable=checkNeed(dataPostStart()) && checkNeed(dataEvalStart())), {
      updateButton(session, "nextAnalysis", disabled=!analysisAvailable)
      md_update_stepper_step(session, "steps", "analysis", enabled=analysisAvailable)
    })
  })
  
  observeEvent(input$nextAnalysis, {
    md_update_stepper(session, "steps", value="analysis")
  })
  
  observe({
    with(list(analyzeAvailable=checkNeed(input$analysisTypes)), {
      updateButton(session, "analyze", disabled=!analyzeAvailable)
    })
  })
  
  ############################################################
  # Set up step summaries
  ############################################################
  
  output$loadSummary = reactive({
    validate(need(userInput()$name, FALSE))
    md_update_spinner(session, "loadSpinner", hidden=checkNeed(userInput()$name))
    userInput()$name
  })
  
  output$dateSummary = renderUI({
    validate(need(input$dateCol, FALSE))
    tags$code(input$dateCol)
  })
  
  output$outcomeSummary = renderUI({
    validate(need(dataOutcome(), FALSE))
    if (checkNeed(input$denomCol) && checkNeed(input$groupCol)) {
      span(
        tags$code(input$outcomeCol),
        " / ",
        tags$code(input$denomCol),
        " by ",
        tags$code(input$groupCol)
      )
    } else if (checkNeed(input$denomCol)) {
      span(
        tags$code(input$outcomeCol),
        " / ",
        tags$code(input$denomCol)
      )
    } else if (checkNeed(input$groupCol)) {
      span(
        tags$code(input$outcomeCol),
        " by ",
        tags$code(input$groupCol)
      )
    } else {
      tags$code(input$outcomeCol)
    }
  })
  
  output$periodsSummary = renderUI({
    validate(need(dataPostStart(), FALSE), need(dataEvalStart(), FALSE))
    span(
      span(
        class="pre-period",
        strftime(min(dataTime()), "%b %Y"), 
        "—",
        strftime(dataPostStart(), "%b %Y")
      ),
      "vs.",
      span(
        class="post-period",
        strftime(dataEvalStart(), "%b %Y"), 
        "—",
        strftime(max(dataTime()), "%b %Y")
      )
    )
  })
  
  ############################################################
  # Download analysis results
  ############################################################
  
  output$download <- downloadHandler(
    filename = function() {
      sprintf("InterventionEvaluatR Analysis %s.rds", Sys.Date())
    },
    content = function(file) {
      saveRDS(list(
        input = inputData(),
        params = analysisParams(),
        results = analysisResults()
      ), file)
    }
  )
  outputOptions(output, 'download', suspendWhenHidden=FALSE)
  
  ############################################################
  # Analysis
  ############################################################
  
  ANALYSIS_READY = "ready"
  ANALYSIS_RUNNING = "running"
  ANALYSIS_DONE = "done"
  ANALYSIS_FAILED = "failed"
  ANALYSIS_CANCELED = "canceled"
  
  analysisStatus = reactiveVal(ANALYSIS_READY)
  analysisResults = reactiveVal(NULL)
  analysisParams = reactiveVal(NULL)
  
  output$analysisStatus = renderText({
    analysisStatus()
  }) 
  outputOptions(output, 'analysisStatus', suspendWhenHidden=FALSE)
  
  output$resultsUnivariate = reactive({})
  outputOptions(output, 'resultsUnivariate', suspendWhenHidden=FALSE)

  observeEvent(input$analyze, {
    withLogErrors({
      if (analysisStatus() == ANALYSIS_RUNNING) {
        return()
      } else {
        analysisStatus(ANALYSIS_RUNNING)
        
        # Detect whether we are using monthly or quarterly observations by looking at the average interval between observations
        obsPerYear = 365 / as.numeric(diff(range(dataTime()))) * length(unique(dataTime()))
        obsPerYear = ifelse(obsPerYear > 8, 12, 4)
        
        analysisData = inputData()
        analysisData[[input$dateCol]] = dataTime()
        
        if (checkNeed(input$analysisGroups)) {
          analysisData %<>% filter_at(input$groupCol, function(group) group %in% input$analysisGroups)
        }
  
        params = list(
          post_period_start=dataPostStart(),
          eval_period_start=dataEvalStart(),
          eval_period_end=max(dataTime()),
          n_seasons=obsPerYear,
          year_def="cal_year",
          group_name=input$groupCol,
          date_name=input$dateCol,
          outcome_name=input$outcomeCol,
          denom_name=input$denomCol
        )
        
        print("Analysis setup:")
        print(params)
        
        params = c(
          params,
          list(
            country="Placeholder",
            data=analysisData
          )
        )
        
        analysisParams(params)
        
        premadeResults = inputResults()
  
        future({
          withLogErrors({
            # If the user uploaded precomputed results, use them
            # TODO: only do this if params are unchanged
            if (checkNeed(premadeResults)) {
              premadeResults
            } 
            # Otherwise set up the computation worker and run the analysis
            else {
              worker = setupWorker()
              oplan = workerPlan(worker)
              on.exit(plan(oplan), add=TRUE)
              
              on.exit({
                dismissWorker(worker)
              }, add=TRUE)
              
              future({
                withLogErrors({
                  app.analyze(params)
                })
              }) %>% value()
            }
          })
        }) %...>% (function(results) {
          print("Analysis done")
          analysisStatus(ANALYSIS_DONE)
          analysisResults(results)

          plots = app.plot(results)
          
          output$resultsUI = renderUI({
            # One stepper step for each analysis group
            steps = llply(seq_along(plots), function(idx) {
              groupName = names(plots)[idx]
              md_stepper_step(
                title=sprintf("Group %s", groupName),
                value=sprintf("result-group-%s", idx),
                enabled=TRUE,
                md_carousel(
                  sprintf("carousel-results-group-%s", idx), 
                  list(
                    plotlyOutput(sprintf("results.group%d.univariate", idx), width="800px"),
                    plotlyOutput(sprintf("results.group%d.univariate2", idx), width="800px")
                  )
                )
              )
            })
            
            do.call(md_stepper_vertical, c(
              steps, list(
                md_stepper_step(
                  title="Save results",
                  value="save",
                  downloadButton('download', "Download analysis results"),
                  enabled=TRUE
                ),
                id="results",
                selected="univariate"
              )
            ))
          })
          
          for(idx in seq_along(plots)) {
            output[[sprintf("results.group%d.univariate", idx)]] = renderPlotly({
              plots[[idx]]$univariate %>% plotlyOptions()
            })
            outputOptions(output, sprintf("results.group%d.univariate", idx), suspendWhenHidden=FALSE)
            output[[sprintf("results.group%d.univariate2", idx)]] = renderPlotly({
              plots[[idx]]$univariate %>% plotlyOptions()
            })
            outputOptions(output, sprintf("results.group%d.univariate2", idx), suspendWhenHidden=FALSE)
          }
        }) %...!% (function(error) {
          print("Analysis failed")
          analysisStatus(ANALYSIS_FAILED)
          print(error$message)
          showNotification(error$message)
        })
  
        # By constructing a future but returning a NULL, shiny server will continue updating the UI while the future is being computed, which allows us to give progress updates
        NULL
      }
    })
  })
})

analysisStatusDetail <- function(text) {
  print(text)
}

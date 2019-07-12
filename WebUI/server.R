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

import::from(magrittr, "%>%")
import::from(plotly, ggplotly, renderPlotly)
import::from(shinyBS, updateButton)
import::from(shinyjs, hidden, toggleElement, toggleClass)
import::from(shinyWidgets, airMonthpickerInput)
import::from(lubridate, "%m+%", "%m-%", days)
import::from(ggplot2, geom_blank, geom_errorbarh)

source("common.R")

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
  
  inputData = reactive({
    if (!is.null(input$stockDataset) && input$stockDataset != "") {
      md_update_spinner(session, "loadSpinner", visible=TRUE)
    }
    switch(
      input$stockDataset,
      pnas_brazil = {
        data("pnas_brazil", package="InterventionEvaluatR")
        pnas_brazil
      }
    )
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
  
  dataPostStart = reactive({
    validate(need(input$postStart, FALSE))
    as.Date(input$postStart, "%Y-%m-%d")
  })

  dataEvalStart = reactive({
    validate(need(input$postDuration, FALSE))
    dataPostStart() %m+% months(as.numeric(input$postDuration))
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
        geom_blank()
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
    oldValue = {
      if (checkNeed(input$postStart)) {
        # There is a weird bug in airMonthpickerInput with minView=months that causes the date picker to set itself to one month earlier than value if value is the first day of the month.
        dataPostStart() %m+% days(15)
      }
    }
    airMonthpickerInput(
      inputId = "postStart",
      label = "When was the vaccine introduced?",
      view="months",
      minView="months",
      minDate=min(dataTime()),
      maxDate=max(dataTime()) %m-% months(as.numeric(input$postDuration)),
      addon="none",
      autoClose=TRUE,
      value=oldValue 
    )
  })
  outputOptions(output, 'introDateUI', suspendWhenHidden=FALSE)
  
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
    null2empty(unspin(session, "loadSpinner", 
      invert.list(stockDatasets)[[input$stockDataset]]
    ))
  })
  
  output$dateSummary = renderUI({
    null2empty(
      tags$code(input$dateCol)
    )
  })
  
  output$outcomeSummary = renderUI({
    null2empty({
      if (is.null(inputData())) {
        NULL
      } else if (is.null(input$outcomeCol) || !(input$outcomeCol %in% names(inputData()))) {
        NULL
      } else if (is.null(input$denomCol) || !(input$denomCol %in% names(inputData()))) {
        if (is.null(input$groupCol) || !(input$groupCol %in% names(inputData()))) {
          tags$code(input$outcomeCol)
        } else {
          span(
            tags$code(input$outcomeCol),
            " by ",
            tags$code(input$groupCol)
          )
        }
      } else {
        if (is.null(input$groupCol) || !(input$groupCol %in% names(inputData()))) {
          span(
            tags$code(input$outcomeCol),
            " / ",
            tags$code(input$denomCol)
          )
        } else {
          span(
            tags$code(input$outcomeCol),
            " / ",
            tags$code(input$denomCol),
            " by ",
            tags$code(input$groupCol)
          )
        }
      }
    })
  })
  
  output$periodsSummary = renderUI({
    validate(need(dataPostStart(), FALSE), need(dataEvalStart, FALSE))
    
    sprintf(
      "%s — %s vs. %s — %s", 
      strftime(min(dataTime()), "%b %Y"), 
      strftime(dataPostStart(), "%b %Y"), 
      strftime(dataEvalStart(), "%b %Y"), 
      strftime(max(dataTime()), "%b %Y")
    )
  })
  
  ############################################################
  # Analysis
  ############################################################
  
  ANALYSIS_READY = "ready"
  ANALYSIS_RUNNING = "running"
  ANALYSIS_DONE = "done"
  ANALYSIS_FAILED = "failed"
  ANALYSIS_CANCELED = "canceled"
  
  analysisStatus <- reactiveVal(ANALYSIS_READY)
  analysisResults <- reactiveVal(NULL)
  
  output$analysisStatus = renderText({
    analysisStatus()
  }) 
  outputOptions(output, 'analysisStatus', suspendWhenHidden=FALSE)
  
  output$analysisResults = renderTable({
    analysisResults()
  })
  outputOptions(output, 'analysisResults', suspendWhenHidden=FALSE)
  
  observeEvent(input$analyze, {
    if (analysisStatus() == ANALYSIS_RUNNING) {
      return()
    } else {
      analysisStatus(ANALYSIS_RUNNING)
      
      result = future({
        worker = setupWorker()
        oplan = workerPlan(worker)
        on.exit(plan(oplan), add=TRUE)
        
        on.exit({
          dismissWorker(worker)
        }, add=TRUE)
        
        future({
          k = names(Sys.getenv())
          v = Sys.getenv(k)
          data.frame(name=k, value=v)
        }) %>% value()
      }) %...>% (function(result) {
        analysisStatus(ANALYSIS_DONE)
        analysisResults(result)
      }) %...!% (function(error) {
        analysisStatus(ANALYSIS_FAILED)
        analysisResults(NULL)
        print(error$message)
        showNotification(error$message)
      })
      
      NULL
    }
  })
})

# Set up a worker for evaluation of InterventionEvaluatR
setupWorker = function() {
  if(getOption("ie.worker.local", TRUE)) {
    setupLocalWorker()
  } else {
    setupRemoteWorker()
  }
}

setupLocalWorker = function() {
  list(local=TRUE)
}

setupRemoteWorker = function() {
  # TODO generate ephemeral SSH key
  
  machineName = sprintf("iew-%s", UUIDgenerate())
  
  # First provision a DigitalOcean droplet
  analysisStatusDetail("Provisioning DO droplet")
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "create",
      "--driver", "digitalocean",
      "--digitalocean-access-token", getOption("ie.digitalocean.access.token"),
      "--digitalocean-size", getOption("ie.worker.digitalocean-droplet-size", "s-2vcpu-4gb"),
      "--digitalocean-userdata", "worker/cloud-config.yml",
      machineName
    )
  )
  
  workerConfig = check.output(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "config",
      machineName
    )
  )
  
  workerIp = check.output(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "ip",
      machineName
    )
  ) %>% trimws()
  
  # Copy the worker image
  analysisStatusDetail("Copying worker image")
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "scp",
      "worker/image.tar.xz", sprintf("%s:/tmp/worker-image.tar.xz", machineName)
    )
  )
  
  # Load the worker image into docker
  analysisStatusDetail("Unarchiving worker image")
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "ssh",
      machineName, "/usr/bin/unxz", "/tmp/worker-image.tar.xz"
    )
  )
  
  analysisStatusDetail("Loading worker image")
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "ssh",
      machineName, sprintf("%s/docker", getOption("ie.worker.docker.bindir")), "load", "--input", "/tmp/worker-image.tar"
    )
  )
  
  # Make a single-worker cluster 
  workerCluster = makeClusterPSOCK(
    workers=workerIp,
    rshopts=c("-i", "worker/id_rsa", "-o", "StrictHostKeyChecking=no", "-o", "UserKnownHostsFile=/dev/null"),
    user="evaluatr",
    rscript="/usr/local/bin/Rscript-docker"
  )
  
  list(local=FALSE, cluster=workerCluster, machineName=machineName)
}

# Generate a future evaluation plan for our worker
workerPlan = function(worker) {
  if(worker$local) {
    localWorkerPlan(worker)
  } else {
    remoteWorkerPlan(worker)
  }
}

localWorkerPlan = function(worker) {
  plan()
}

remoteWorkerPlan = function(worker) {
  plan(cluster, workers=worker$cluster)
}

# Shut down the worker
dismissWorker = function(worker) {
  if(worker$local) {
    dismissLocalWorker(worker)
  } else {
    dismissRemoteWorker(worker)
  }
}

dismissLocalWorker = function(worker) {
}

dismissRemoteWorker = function(worker) {
  check.call(
    c(
      sprintf("%s/docker-machine", getOption("ie.webui.docker.bindir")), "rm", "--force",
      worker$machineName
    )
  )
}

analysisStatusDetail <- function(text) {
  print(text)
}

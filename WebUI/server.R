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
import::from(shinyjs, hidden)

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
  
  dataTime = reactive({
    if (!validCol(input$timeCol, inputData(), . %>% timeColumns() %>% names())) {
      NULL
    } else if (is.null(input$timeFormat) || !input$timeFormat %in% dateFormats) {
      NULL
    } else {
      time = as.Date(inputData()[[input$timeCol]], format=input$timeFormat) 
      if (any(is.na(time))) {
        NULL
      } else {
        time
      }
    }
  })
  
  dataOutcome = reactive({
    if (!validCol(input$outcomeCol, inputData(), . %>% names())) {
      NULL
    } else if (is.null(input$denomCol) || !(input$denomCol %in% names(inputData()))) {
      inputData()[[input$outcomeCol]]
    } else {
      inputData()[[input$outcomeCol]] / inputData()[[input$denomCol]]
    }
  })
  
  dataGroup = reactive({
    if (!validCol(input$groupCol, inputData(), . %>% names())) {
      NULL
    } else {
      inputData()[[input$groupCol]]
    }
  })
  
  dataPeriods = reactive({
    dataGroup()
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
    if(is.null(dataOutcome()) || is.null(dataTime())) {
      NULL
    } else if (!is.null(dataGroup())) {
      ggplotly(ggplot(
          data.frame(y=dataOutcome(), t=dataTime(), g=dataGroup()) %>% arrange(t)
        ) +
          geom_line(aes(x=t, y=y, group=g), size=0.1) +
          labs(x=NULL, y=NULL) +
          theme_minimal()
      ) %>% plotlyOptions()
    } else {
      ggplotly(ggplot(
          data.frame(y=dataOutcome(), t=dataTime()) %>% arrange(t)
        ) +
          geom_line(aes(x=t, y=y), size=0.1) +
          labs(x=NULL, y=NULL) +
          theme_minimal()
      ) %>% plotlyOptions()
    }
  })
  outputOptions(output, 'previewPlot', suspendWhenHidden=FALSE)
  
  output$showPreviewPlot = reactive({
    !is.null(dataOutcome()) && !is.null(dataTime())
  })
  outputOptions(output, 'showPreviewPlot', suspendWhenHidden=FALSE)
  
  ############################################################
  # Set up reactive input controls
  ############################################################
  
  output$timeColUI <- renderUI({
    choices = names(timeColumns(inputData()))
    if (length(choices) > 1) {
      choices = c("", choices)
    }
    
    selectInput(
      inputId = "timeCol",
      label = "Which variable in your data represents time?",
      choices = choices
    )
  })
  outputOptions(output, 'timeColUI', suspendWhenHidden=FALSE)
  
  output$timeFormatUI <- renderUI({
    if (!validCol(input$timeCol, inputData(), . %>% timeColumns() %>% names())) {
      tagList()
    } else {
      choices = (inputData() %>% timeColumns())[[input$timeCol]]
      select = selectInput(
        inputId = "timeFormat",
        label = "Time Format:",
        choices = choices
      )
      
      if (length(choices) == 1) {
        select = hidden(select)
      }
      
      select
    }
  })
  outputOptions(output, 'timeFormatUI', suspendWhenHidden=FALSE)
  
  output$outcomeColUI <- renderUI({
    selectInput(
      inputId = "outcomeCol",
      label = "Outcome:",
      choices = c("", names(inputData()))
    )
  })
  outputOptions(output, 'outcomeColUI', suspendWhenHidden=FALSE)
  
  output$denomColUI <- renderUI({
    selectInput(
      inputId = "denomCol",
      label = "Denominator:",
      choices = c(`No denominator`="", names(inputData()))
    )
  })
  outputOptions(output, 'denomColUI', suspendWhenHidden=FALSE)
  
  output$groupColUI <- renderUI({
    selectInput(
      inputId = "groupCol",
      label = "Group:",
      choices = c(`No grouping`="", names(inputData()))
    )
  })
  outputOptions(output, 'groupColUI', suspendWhenHidden=FALSE)
  
  ############################################################
  # Set up step enabled / disabled state and next buttons
  ############################################################
  
  timeCols = reactive({
    if (!is.null(inputData())) {
      timeColumns(inputData())
    }
  })
  
  observe({
    with(list(timeAvailable=!is.null(timeCols())), {
      updateButton(session, "nextTime", disabled=!timeAvailable)
      md_update_stepper_step(session, "steps", "time", enabled=timeAvailable)
    })
  })
  
  observeEvent(input$nextTime, {
    md_update_stepper(session, "steps", value="time")
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
    with(list(periodsAvailable=!is.null(dataOutcome())), {
      updateButton(session, "nextPeriods", disabled=!periodsAvailable)
      md_update_stepper_step(session, "steps", "periods", enabled=periodsAvailable)
    })
  })
  
  observeEvent(input$nextPeriods, {
    md_update_stepper(session, "steps", value="periods")
  })

  observe({
    with(list(analysisAvailable=!is.null(dataPeriods())), {
      updateButton(session, "nextAnalysis", disabled=!analysisAvailable)
      md_update_stepper_step(session, "steps", "analysis", enabled=analysisAvailable)
      updateButton(session, "analyze", disabled=!analysisAvailable)
    })
  })
  
  observeEvent(input$nextAnalysis, {
    md_update_stepper(session, "steps", value="analysis")
  })
  
  ############################################################
  # Set up step summaries
  ############################################################
  
  output$loadSummary = reactive({
    null2empty(unspin(session, "loadSpinner", 
      invert.list(stockDatasets)[[input$stockDataset]]
    ))
  })
  
  output$timeSummary = renderUI({
    null2empty(
      tags$code(input$timeCol)
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
    ""
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
  
  output$analysisResults = renderTable({
    analysisResults()
  })
  
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

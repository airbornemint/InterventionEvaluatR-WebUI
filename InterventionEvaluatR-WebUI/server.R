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
library(shinyjs)
library(InterventionEvaluatR)
library(uuid)
library(magrittr)
library(dplyr)
library(ggplot2)

source("common.R")

plan(multisession)
future(1) # This forces the multisession workers to be set up right away, rather than randomly stalling the app later

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

timeFormats = list(
  `YYYY-MM-DD`="%Y-%m-%d",
  `YYYY-DD-MM`="%Y-%d-%m",
  `MM-DD-YYYY`="%m-%d-%Y",
  `DD-MM-YYYY`="%d-%m-%Y"
)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  ############################################################
  # Set up reactive data inputs
  ############################################################
  
  inputData = reactive({
    switch(
      input$stockDataset,
      pnas_brazil = {
        data("pnas_brazil", package="InterventionEvaluatR")
        pnas_brazil
      }
    )
  })
  
  dataOutcome = reactive({
    if (is.null(inputData())) {
      NULL
    } else if (is.null(input$outcomeCol) || !(input$outcomeCol %in% names(inputData()))) {
      NULL
    } else if (is.null(input$denomCol) || !(input$denomCol %in% names(inputData()))) {
      inputData()[[input$outcomeCol]]
    } else {
      inputData()[[input$outcomeCol]] / inputData()[[input$denomCol]]
    }
  })
  
  dataTime = reactive({
    if (is.null(inputData())) {
      NULL
    } else if (is.null(input$timeCol) || !(input$timeCol %in% names(inputData()))) {
      NULL
    } else {
      time = as.Date(inputData()[[input$timeCol]], format=timeFormats[[input$timeFormat]]) 
      if (any(is.na(time))) {
        NULL
      } else {
        time
      }
    }
  })
  
  dataGroup = reactive({
    if (is.null(inputData())) {
      NULL
    } else if (is.null(input$groupCol) || !(input$groupCol %in% names(inputData()))) {
      NULL
    } else {
      inputData()[[input$groupCol]]
    }
  })
  
  ############################################################
  # Set up reactive data display
  ############################################################
  
  output$previewPlot = renderPlot({
    if(is.null(dataOutcome()) || is.null(dataTime())) {
      NULL
    } else if (!is.null(dataGroup())) {
      ggplot(
        data.frame(y=dataOutcome(), t=dataTime(), g=dataGroup()) %>% arrange(t)
      ) +
        geom_line(aes(x=t, y=y, group=g)) +
        theme_light()
    } else {
      ggplot(
        data.frame(y=dataOutcome(), t=dataTime()) %>% arrange(t)
      ) +
        geom_line(aes(x=t, y=y)) +
        theme_light()
    }
  }, height=200)
  
  output$showPreviewPlot = reactive({
    !is.null(dataOutcome()) && !is.null(dataTime())
  })
  outputOptions(output, 'showPreviewPlot', suspendWhenHidden=FALSE)
  
  ############################################################
  # Set up reactive input controls
  ############################################################
  
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
  
  output$timeColUI <- renderUI({
    selectInput(
      inputId = "timeCol",
      label = "Which variable in your data represents time?",
      choices = c("", names(inputData()))
    )
  })
  outputOptions(output, 'timeColUI', suspendWhenHidden=FALSE)
  
  output$timeFormatUI <- renderUI({
    selectInput(
      inputId = "timeFormat",
      label = "Time Format:",
      choices = names(timeFormats)
    )
  })
  outputOptions(output, 'timeFormatUI', suspendWhenHidden=FALSE)
  
  output$groupColUI <- renderUI({
    selectInput(
      inputId = "groupCol",
      label = "Group:",
      choices = c(`No grouping`="", names(inputData()))
    )
  })
  outputOptions(output, 'groupColUI', suspendWhenHidden=FALSE)
  
  ############################################################
  # Set up reactive buttons
  ############################################################
  
  loadDone = reactive({
    !is.null(inputData())
  })
  
  observe({
    if (loadDone()) {
      enable("next.outcome")
    } else {
      disable("next.outcome")
    }
  })
  
  ############################################################
  # Set up step summaries
  ############################################################
  
  output$loadSummary = reactive({
    null2empty(invert.list(stockDatasets)[[input$stockDataset]])
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
  
  observeEvent(input$prev.load, {
    updateTabsetPanel(session, "stepNav", selected="load")
  })
  
  observeEvent(input$next.outcome, {
    updateTabsetPanel(session, "stepNav", selected="outcome")
  })
  
  observeEvent(input$prev.outcome, {
    updateTabsetPanel(session, "stepNav", selected="outcome")
  })
  
  observeEvent(input$next.time, {
    updateTabsetPanel(session, "stepNav", selected="time")
  })
  
  observeEvent(input$prev.time, {
    updateTabsetPanel(session, "stepNav", selected="time")
  })
  
  observeEvent(input$next.grouping, {
    updateTabsetPanel(session, "stepNav", selected="grouping")
  })
  
  observeEvent(input$prev.grouping, {
    updateTabsetPanel(session, "stepNav", selected="grouping")
  })
  
  observeEvent(input$next.analysis, {
    updateTabsetPanel(session, "stepNav", selected="analysis")
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

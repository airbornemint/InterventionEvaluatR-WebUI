# Create results UI
results.ui = function() {
  div(
    class="tab-pane fade show",
    id="nav-results",
    role="tabpanel",
    "aria-labelledby"="nav-results-tab",
    div(
      class="container",
      md_row(
        md_column(
          tagList(
            uiOutput("resultsPendingUI"),
            uiOutput("resultsUI")
          )
        )
      )
    )
  )
}

# Create results UI help
results.help = function() {
  renderHTML("markdown/help-results.md")
}

# Server-side handling of results UI
results.server = function(input, output, session, setup) {
  ############################################################
  # Analysis
  ############################################################
  
  ANALYSIS_READY = "ready"
  ANALYSIS_RUNNING = "running"
  ANALYSIS_DONE = "done"
  ANALYSIS_FAILED = "failed"
  ANALYSIS_CANCELED = "canceled"

  analysisStatus = reactiveVal(ANALYSIS_READY)
  completedAnalysis = reactiveVal(NULL)
  reformattedAnalysis = reactiveVal(NULL)
  
  output$analysisStatus = renderText({
    analysisStatus()
  }) 
  outputOptions(output, 'analysisStatus', suspendWhenHidden=FALSE)
  
  output$resultsUnivariate = reactive({})
  outputOptions(output, 'resultsUnivariate', suspendWhenHidden=FALSE)
  
  output$resultsPendingUI = renderUI({
    tags$section(
      id="results-pending",
      div(
        class="navbar results-heading mb-3 mt-3 justify-content-center primary-color",
        p(class="h3 p-2 m-0 text-white", "Analysis in progress…"),
        md_spinner("spinner-results") %>% tagAppendAttributes(class="text-white")
      )
    )
  })
  outputOptions(output, 'resultsPendingUI', suspendWhenHidden=FALSE)
  
  observeEvent(input$analyze, {
    # Loading progress UI
    shinyjs::show("results-pending")
    
    withLogErrors({
      session$sendCustomMessage("activate_tab", list(tab="nav-results-tab"))
      if (analysisStatus() == ANALYSIS_RUNNING) {
        return()
      } else {
        analysisStatus(ANALYSIS_RUNNING)
        
        analysisData = setup$preparedData()
        
        if (checkNeed(input$analysisGroups)) {
          analysisData %<>% filter_at(input$groupCol, function(group) group %in% input$analysisGroups)
          groups = input$analysisGroups
        } else {
          groups = NULL
        }
        
        params = setup$analysisParams()
        
        print("Analysis setup:")
        print(params)
        
        params = c(
          params,
          list(
            data=analysisData
          )
        )
        if (checkNeed(setup$precomputedAnalysis())) {
          precomputedAnalysis = setup$precomputedAnalysis()
        } else {
          precomputedAnalysis = NULL
        }
        
        analysisTypes = input$analysisTypes
        
        future({
          withLogErrors({
            # If the user uploaded precomputed results, and their current analysis settings are compatible with them, use them
            if (checkNeed(precomputedAnalysis)) {
              precomputedAnalysis
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
                  performAnalysis(params, analysisTypes)
                })
              }) %>% value()
            }
          })
        }) %...>% (function(analysis) {
          print("Analysis done")
          analysisStatus(ANALYSIS_DONE)
          completedAnalysis(analysis)
          
          reformatted = reformatAnalysis(analysis, analysisTypes)
          reformattedAnalysis(reformatted)
          results.server.show(input, output, session, reformatted)
        }) %...!% (function(error) {
          print("Analysis failed")
          analysisStatus(ANALYSIS_FAILED)
          print(error$message)
          print(error$call)
          showNotification(error$message)
        })
        
        # By constructing a future but returning a NULL, shiny server will continue updating the UI while the future is being computed, which allows us to give progress updates
        NULL
      }
    })
  })

  ############################################################
  # Download analysis results
  ############################################################
  
  output$downloadResults <- downloadHandler(
    filename = function() {
      sprintf("InterventionEvaluatR Report %s.zip", Sys.Date())
    },
    content = function(file) {
      # Output files to a temporary directory
      tempDir = sprintf("%s/%s", tempdir(), UUIDgenerate())
      dir.create(tempDir, recursive=TRUE)

      # Save data in RDS
      # Note: save all data even if only a subset of groups was analyzed, so that we can come back later and analyze other groups
      analysis = completedAnalysis()
      saveRDS(list(
        version = SAVE_VERSION_CURRENT,
        data = setup$preparedData(),
        params = setup$analysisParams(),
        analysis = analysis
      ), sprintf("%s/Results.rds", tempDir))
      
      # Render each plot to a PDF file
      analysis = reformattedAnalysis()
      for (idx in seq_along(analysis$results$groups)) {
        group = analysis$results$groups[[idx]]
        groupName = group$name
        groupFileName = gsub("[^a-zA-Z0-9_.-]", "-", groupName)
        dir.create(sprintf("%s/Plots/%s", tempDir, groupFileName), recursive=TRUE)
        ggsave(
          sprintf("%s/Plots/%s/prevented-cases.pdf", tempDir, groupFileName), 
          group$plots$prevented,
          width = 4, height = 3
        )
        ggsave(
          sprintf("%s/Plots/%s/cases-yearly.pdf", tempDir, groupFileName), 
          group$plots$tsYearly,
          width = 4, height = 3
        )
        ggsave(
          sprintf("%s/Plots/%s/cases-monthly.pdf", tempDir, groupFileName), 
          group$plots$tsMonthly,
          width = 4, height = 3
        )
        ggsave(
          sprintf("%s/Plots/%s/covariate-comparison.pdf", tempDir, groupFileName), 
          group$plots$univariate,
          width = 4, height = 3
        )
      }
      
      message(sprintf("Running brew in %s", tempDir))

      # Using brew -> latex because rmarkdown is currently unable to output code blocks from a loop, and we need to loop over groups
      brew(
        file="Report.template.tex",
        output=sprintf("%s/Report.tex", tempDir),
        envir=new_environment(data=list(
          # LaTeX template needs analysis data
          analysis=analysis,
          # and some helpers
          renderLaTeX=renderLaTeX,
          new_environment=new_environment
        ), parent=baseenv())
      )

      # Change workdir when running LaTeX so its temporary files can be deleted
      oldWD <- getwd()
      # Clean up temporary directory on exit
      on.exit({
        setwd(oldWD)
        unlink(tempDir, recursive = TRUE, force = TRUE)
      })
      setwd(tempDir)
      
      message(sprintf("Running texi2pdf in %s", tempDir))

      Sys.setenv(PDFLATEX="xelatex")
      texi2pdf(
        file="Report.tex",
        quiet=FALSE
      )
      
      # Zip what we want (the rest is LaTeX garbage)
      zip(file, c("Plots", "Report.pdf", "Results.rds"))
    }
  )
  outputOptions(output, 'downloadResults', suspendWhenHidden=FALSE)
}

# Server-side update of results when analysis is complete
results.server.show = function(input, output, session, analysis) {
  setup = analysis$setup
  results = analysis$results
  withLogErrors({
    output$resultsUI = renderUI({
      # One section for each analysis group
      tagList(
        tagList(llply(seq_along(results$groups), function(idx) {
          group = results$groups[[idx]]
          groupName = group$name
          
          plots = group$plots
          prevented = group$prevented
          
          # item=tableOutput(visId("rateRatios", idx)) %>% tagAppendAttributes(class="table-wrap"),
          
          tags$section(
            div(
              class="navbar results-heading mt-3 mb-3 justify-content-center primary-color",
              p(class="h3 p-2 m-0 text-white", groupName)
            ),
            md_accordion(
              id=sprintf("acc-results-group-%s", idx),
              md_accordion_card(
                visId("summary", idx),
                "Summary",
                renderHTML(
                  "markdown/results-group-summary.Rmd", envir=new_environment(data=list(
                    setup=setup,
                    group=group
                  ), parent=baseenv())
                ),
                expanded=TRUE
              ),
              md_accordion_card(
                visId("prevented", idx),
                "Prevented cases",
                div(
                  class="d-flex justify-content-center", 
                  plotlyOutput(visId("prevented", idx), width="800px")
                )
              ),
              md_accordion_card(
                visId("tsYearly", idx),
                "Total cases (yearly)",
                div(
                  class="d-flex justify-content-center", 
                  plotlyOutput(visId("tsYearly", idx), width="800px")
                )
              ),
              md_accordion_card(
                visId("tsMonthly", idx),
                "Total cases (monthly)",
                div(
                  class="d-flex justify-content-center", 
                  plotlyOutput(visId("tsMonthly", idx), width="800px")
                )
              ),
              md_accordion_card(
                visId("card-univariate", idx),
                "Covariate comparison",
                div(
                  class="d-flex justify-content-center", 
                  plotlyOutput(visId("univariate", idx), width="800px")
                )
              )
            ) %>% tagAppendAttributes(class="mb-3 mt-3 col-12")
          )
        })),
        tags$section(
          div(
            class="navbar results-heading justify-content-center primary-color",
            p(class="h3 p-2 m-0 text-white", "Download results")
          ),
          div(
            class="col-12 mb-3 mt-3",
            downloadButton('downloadResults', "Download results"),
            p("Includes:"),
            tags$ul(
              tags$li("Report with analysis results (PDF). It contains the same information you see on this page, in a form that you can easily share with others."),
              tags$li("Individual plots (PDF). You can use these in your own reports and presentations."),
              tags$li("Data file with analysis results (RDS). Advanced users can import this into RStudio for additional analysis or to generate additional plots.")
            )
          )
        )
      )
    })
    
    for(idx in seq_along(results$groups)) {
      # Need separate environment because renderPlotly is lazy and therefore without a separate environment all plots end up being evaluated in the last group
      plotlyEnv = env(
        plots=results$groups[[idx]]$plots
      )
      
      if ("univariate" %in% analysis$setup$analysisTypes) {
        output[[visId("univariate", idx)]] = renderPlotly(
          ggplotly(plots$univariate) %>% plotlyOptions(staticPlot=TRUE),
          env=plotlyEnv
        )
        outputOptions(output, visId("univariate", idx), suspendWhenHidden=FALSE)
      }
      
      if ("impact" %in% analysis$setup$analysisTypes) {
        output[[visId("rateRatios", idx)]] = renderTable(
          results$rateRatios[[idx]]
        )
        outputOptions(output, visId("rateRatios", idx), suspendWhenHidden=FALSE)
        
        output[[visId("tsMonthly", idx)]] = renderPlotly(
          ggplotly(plots$tsMonthly) %>% plotlyOptions(),
          env=plotlyEnv
        )
        outputOptions(output, visId("tsMonthly", idx), suspendWhenHidden=FALSE)
        
        output[[visId("tsYearly", idx)]] = renderPlotly(
          ggplotly(plots$tsYearly) %>% plotlyOptions(),
          env=plotlyEnv
        )
        outputOptions(output, visId("tsYearly", idx), suspendWhenHidden=FALSE)
        
        output[[visId("prevented", idx)]] = renderPlotly(
          ggplotly(plots$prevented) %>% plotlyOptions(),
          env=plotlyEnv
        )
        outputOptions(output, visId("prevented", idx), suspendWhenHidden=FALSE)
      }
    }
    output$resultsPendingUI = renderUI({
    })
  })
}

visId = function(type, idx) {
  sprintf("%sResults%d", type, idx)
}

analysisStatusDetail <- function(text) {
  print(text)
}

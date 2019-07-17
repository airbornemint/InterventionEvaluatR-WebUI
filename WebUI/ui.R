#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

import::from(magrittr, "%>%")
import::from(plotly, plotlyOutput)
import::from(fontawesome, fa)
import::from(shinyjs, useShinyjs, hidden)

source("common.R")
source("mdbootstrap.R")

md_page(
    id="page",
    useShinyjs(),
    singleton(tags$head(
        tags$script(src = "js/app.js")
    )),
    div(
        class="fixed-top",
        id="header",
        md_navbar(
            title="InterventionEvaluatR",
            tags$ul(
                class="navbar-nav ml-auto",
                tags$li(
                    class="nav-item", id="help-button",
                    tags$a(class="nav-link", fa("question-circle", height="1.5em", fill="white"))
                )
            )
        ) %>% tagAppendAttributes(class="navbar-expand-sm"),
        div(
            id="plot-container",
            class="container-fluid",
            div(
                md_row(
                    md_column(
                        id="plotColumn",
                        plotlyOutput("previewPlot", height="200px"),
                        md_spinner("plotSpinner")
                    )
                )
            )
        )
    ),
    div(
        class="main-content",
        div(
            class="container",
            md_row(
                md_column(
                    md_stepper_vertical(
                        id="steps",
                        selected="load",
                        md_stepper_step(
                            title="Load Data",
                            value="load",
                            div(
                                class="file-input",
                                fileInput(
                                    inputId = "userDataset",
                                    label = "Load your data:",
                                    buttonLabel = "Choose a file"
                                )
                            ),
                            selectInput(
                                inputId = "stockDataset",
                                label = "Or load stock data:",
                                choices = c("", stockDatasets)
                            ),
                            nextButton("nextDate", "loadSpinner"),
                            summary=textOutput("loadSummary"),
                            enabled=TRUE
                        ),
                        md_stepper_step(
                            title="Select Time Variable",
                            value="date",
                            uiOutput("dateColUI"),
                            uiOutput("dateFormatUI"),
                            # selectInput(
                            #     inputId = "obsFreq",
                            #     label = "Observation frequency:",
                            #     choices = c("Monthly", "Quarterly")
                            # ),
                            # p("Lorem ipsum year"),
                            # selectInput(
                            #     inputId = "yearStart",
                            #     label = "Year:",
                            #     choices = c("January", "June")
                            # ),
                            nextButton("nextOutcome", "dateSpinner"),
                            summary=uiOutput("dateSummary")
                        ),
                        md_stepper_step(
                            title="Select Outcome Variable",
                            value="outcome",
                            uiOutput("outcomeColUI"),
                            uiOutput("groupColUI"),
                            uiOutput("denomColUI"),
                            nextButton("nextPeriods", "outcomeSpinner"),
                            summary=uiOutput("outcomeSummary")
                        ),
                        md_stepper_step(
                            title="Select Analysis Periods",
                            value="periods",
                            uiOutput("introDateUI"),
                            selectInput(
                                inputId = "postDuration",
                                label = "How long after its introduction did the vaccine become established in the population?",
                                choices=postDurations,
                                selected=12
                            ),
                            nextButton("nextAnalysis", "periodsSpinner"),
                            summary=uiOutput("periodsSummary")
                        ),
                        md_stepper_step(
                            title="Analyze",
                            value="analysis",
                            checkboxGroupInput(
                                "analysisTypes",
                                "Which types of analysis do you want to perform?",
                                c(
                                    "Univariate Poisson regression"="univariate"
                                )
                            ),
                            uiOutput("analysisGroupsUI"),
                            uiOutput("analyzeButtonUI"),
                            textOutput("analysisStatus")
                        )
                    ), tags$hr(),
                    uiOutput("resultsUI")
                )
            )
        )
    ),
    div(
        id="help-toggle", 
        div(
            id="help-container",
            div(
                id="help",
                class="card",
                div(
                    class="help-section card-body",
                    id="help-load",
                    h1("Loading data"),
                    p("Lorem ipsum")
                ),
                div(
                    class="help-section card-body",
                    id="help-date",
                    h1("Selecting time variable"),
                    p("Lorem ipsum")
                ),
                div(
                    class="help-section card-body",
                    id="help-outcome",
                    h1("Selecting outcome"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum"),
                    p("Lorem ipsum")
                ),
                div(
                    class="help-section card-body",
                    id="help-periods",
                    h1("Selecting analysis periods"),
                    p("Lorem ipsum")
                ),
                div(
                    class="help-section card-body",
                    id="help-analysis",
                    h1("Running the analysis"),
                    p("Lorem ipsum")
                )
            )
        )
    )
)
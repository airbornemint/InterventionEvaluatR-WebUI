#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(magrittr)
library(plotly)
library(fontawesome)
library(shinyjs)

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
                class="navbar-nav nav mx-auto justify-content-center",
                role="tablist",
                id="nav-main",
                tags$li(
                    tags$a(
                        class="nav-link active", 
                        id="nav-setup-tab",
                        "data-toggle"="tab",
                        href="#nav-setup",
                        role="tab",
                        "aria-controls"="nav-setup",
                        "aria-selected"="true",
                        "Setup"
                    )
                ),
                tags$li(
                    tags$a(
                        class="nav-link", 
                        id="nav-results-tab",
                        "data-toggle"="tab",
                        href="#nav-results",
                        role="tab",
                        "aria-controls"="nav-results",
                        "aria-selected"="true",
                        "Results"
                    )
                )
            ),
            tags$ul(
                class="navbar-nav justify-content-end",
                tags$li(
                    class="nav-item", id="help-button",
                    tags$a(class="nav-link", fa("question-circle", height="1.5em", fill="white"))
                )
            )
        ) %>% tagAppendAttributes(class="navbar-expand-sm")
    ),
    div(
        class="main-content tab-content",
        id="nav-main-content",
        div(
            class="tab-pane fade show active",
            id="nav-setup",
            role="tabpanel",
            "aria-labelledby"="nav-setup-tab",
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
            ),
            div(
                class="container",
                md_row(
                    class="",
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
                                hidden(checkboxGroupInput(
                                    "analysisTypes",
                                    "Which types of analysis do you want to perform?",
                                    c(
                                        "Univariate Poisson regression"="univariate",
                                        "Synthetic control impact analysis"="impact"
                                    ),
                                    selected = c("univariate", "impact"),
                                    inline=TRUE
                                )),
                                uiOutput("analysisGroupsUI"),
                                uiOutput("analyzeButtonUI")
                            )
                        )
                    )
                )
            )
        ),
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
                            uiOutput("resultsUI"),
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
                    )
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
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
                                    selected=24
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
                            uiOutput("resultsPendingUI"),
                            uiOutput("resultsUI")
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
                    h2("Stock data"),
                    p("If you don't have your own data, you can explore InterventionEvaluatR by using a stock dataset."),
                    h2("Your data"),
                    p("You can also load your own data into InterventionEvaluatR."),
                    h3("Data Format"),
                    p("Your data must be a CSV file with the following columns:"),
                    tags$ul(
                        tags$li("One column containing the date of each observation. The date must be in one of the following formats: YYYY-MM-DD, YYYY-DD-MM, MM-DD-YYYY, or DD-MM-YYYY"),
                        tags$li("One column containing the outcome variable (usually incidence or incidence rate"),
                        tags$li("If your data contains multiple observations for each date (often, these are age groups), then each observation has to be assigned to a group; the group assigment is in its own column"),
                        tags$li("If the outcome variable is incidence, then a separate column containing the denominator should be included."),
                        tags$li("Any other relevant variables you want to include as predictors in the analysis.")
                    )
                ),
                div(
                    class="help-section card-body",
                    id="help-date",
                    h1("Selecting time variable"),
                    p("Choose which variable in your data contains the date of the observations. Only variables that contain valid dates are shown here.")
                ),
                div(
                    class="help-section card-body",
                    id="help-outcome",
                    h1("Selecting outcome"),
                    p("Choose which variable in your data contains the outcome variable — usually, incidence or incidence rate."),
                    p("If your data is grouped (for example, by age), then you must specify which variable lists the group for each observation."),
                    p("If the outcome variable you selected above is incidence (rather than incidence rate), then you also need to select the denominator variable.")
                ),
                div(
                    class="help-section card-body",
                    id="help-periods",
                    h1("Selecting analysis periods"),
                    p("Specify the month in which the vaccine was introduced."),
                    p("Also specify how long it took the vaccine to become established in the population. If you are unsure, 24 months is a good initial choice.")
                ),
                div(
                    class="help-section card-body",
                    id="help-analysis",
                    h1("Running the analysis"),
                    p("If you specified a group variable above, then you can choose to exclude some groups from the analysis. The analysis takes several minutes for each group, so excluding unneeded groups is definitely a good idea.")
                ),
                div(
                    class="help-section card-body",
                    id="help-results",
                    h1("Interpreting the results"),
                    p("Lorem ipsum.")
                )
            )
        )
    )
)
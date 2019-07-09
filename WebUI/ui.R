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

source("common.R")
source("mdbootstrap.R")

nextButton = function(buttonId, spinnerId, title="Next") {
    div(
        class="button-next",
        md_button(
            buttonId,
            span(class="title", title), 
            md_button_spinner(spinnerId), 
            style="primary", disabled=TRUE
        )
    )
}

# Define UI for application that draws a histogram
md_page(
    singleton(tags$head(
        tags$script(src = "js/app.js")
    )),
    div(
        class="sticky-top",
        id="header",
        md_navbar(
            title="InterventionEvaluatR",
            tags$ul(
                class="navbar-nav ml-auto",
                tags$li(
                    class="nav-item", id="help-toggle",
                    tags$a(class="nav-link", fa("question-circle", height="1.5em", fill="white"))
                )
            )
        ) %>% tagAppendAttributes(class="navbar-expand-sm"),
        div(
            class="container-fluid",
            conditionalPanel(
                "output.showPreviewPlot", 
                md_row(
                    md_column(
                        id="plotColumn",
                        plotlyOutput("previewPlot", height="200px"),
                        md_spinner("plotSpinner")
                        # conditionalPanel("output.showPreviewTable", dataTableOutput("previewTable"))
                    )
                )
            )
        )
    ),
    div(
        class="container-with-help",
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
                            fileInput(
                                inputId = "userDataset",
                                label = "Load your data:",
                                buttonLabel = "Choose a file…"
                            ),
                            selectInput(
                                inputId = "stockDataset",
                                label = "Or load stock data:",
                                choices = c("", stockDatasets)
                            ),
                            nextButton("nextTime", "loadSpinner"),
                            summary=textOutput("loadSummary"),
                            enabled=TRUE
                        ),
                        md_stepper_step(
                            title="Select Time Variable",
                            value="time",
                            uiOutput("timeColUI"),
                            uiOutput("timeFormatUI"),
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
                            nextButton("nextOutcome", "timeSpinner"),
                            summary=uiOutput("timeSummary")
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
                            p("Lorem ipsum post start"),
                            dateInput(
                                inputId = "postStart",
                                label = "Post Start:"
                            ),
                            p("Lorem ipsum eval start"),
                            dateInput(
                                inputId = "evalStart",
                                label = "Eval Start:"
                            ),
                            nextButton("nextAnalysis", "periodsSpinner"),
                            summary=uiOutput("periodsSummary")
                        ),
                        md_stepper_step(
                            title="Run Analysis",
                            value="analysis",
                            nextButton("analyze", "analyzeSpinner", title="Run Analysis"),
                            textOutput("analysisStatus"),
                            tableOutput("analysisResults")
                        )
                    )
                )
            )
        ),
        conditionalPanel("true", div(
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
                id="help-time",
                h1("Selecting time variable"),
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
        ))
    )
)
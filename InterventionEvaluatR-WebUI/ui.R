#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
source("common.R")
source("mdbootstrap.R")

# Define UI for application that draws a histogram
md_page(
    div(
        class="sticky-top",
        md_navbar(title="InterventionEvaluatR"),
        conditionalPanel(
            "output.showPreviewPlot", 
            md_row(
                md_column(
                    plotOutput("previewPlot", height="200px")
                    # conditionalPanel("output.showPreviewTable", dataTableOutput("previewTable"))
                )
            )
        )
    ),
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
                        p("Let's begin by loading some data to analyze. You can load your own data, or you can choose a stock dataset below."),
                        selectInput(
                            inputId = "stockDataset",
                            label = "Load stock data:",
                            choices = c("", stockDatasets)
                        ),
                        bsButton("nextTime", "Next", style="primary", disabled=TRUE),
                        summary=textOutput("loadSummary")
                    ),
                    md_stepper_step(
                        title="Select Time Variable",
                        value="time",
                        uiOutput("timeColUI"),
                        uiOutput("timeFormatUI"),
                        selectInput(
                            inputId = "obsFreq",
                            label = "Observation frequency:",
                            choices = c("Monthly", "Quarterly")
                        ),
                        p("Lorem ipsum year"),
                        selectInput(
                            inputId = "yearStart",
                            label = "Year:",
                            choices = c("January", "June")
                        ),
                        p("When was the vaccine introduced?"),
                        dateInput(
                            inputId = "postStart",
                            label = "Post Start:"
                        ),
                        p("Lorem ipsum eval start"),
                        dateInput(
                            inputId = "evalStart",
                            label = "Eval Start:"
                        ),
                        bsButton("nextOutcome", "Next", style="primary", disabled=TRUE),
                        summary=uiOutput("timeSummary")
                    ),
                    md_stepper_step(
                        title="Select Outcome Variable",
                        value="outcome",
                        uiOutput("outcomeColUI"),
                        uiOutput("denomColUI"),
                        uiOutput("groupColUI"),
                        bsButton("nextAnalysis", "Next", style="primary", disabled=TRUE),
                        summary=uiOutput("outcomeSummary")
                    ),
                    md_stepper_step(
                        title="Run Analysis",
                        value="analysis",
                        bsButton("analyze", "Run Analysis", style="primary", disabled=TRUE),
                        textOutput("analysisStatus"),
                        tableOutput("analysisResults")
                    )
                )
            )
        )
    )
)

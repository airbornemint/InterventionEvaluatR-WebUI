#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("mdbootstrap.R")

# Define UI for application that draws a histogram
md_page(
    div(
        md_navbar(title="InterventionEvaluatR"),
        class="fixed-top"
    ),
    div(
        conditionalPanel(
            "output.showPreviewPlot", 
            md_row(
                md_column(
                    plotOutput("previewPlot", height="200px"),
                    div(style="background: linear-gradient(to bottom, rgba(255,255,255,1) 0%,rgba(255,255,255,0) 100%);", height="1rem")
                    # conditionalPanel("output.showPreviewTable", dataTableOutput("previewTable"))
                )
            )
        ),
        class="sticky-top",
        id=""
    ),
    md_row(
        md_column(
            md_stepper_vertical(
                md_stepper_step(
                    "Load Data",
                    p("Let's begin by loading some data to analyze. You can load your own data, or you can choose a stock dataset below."),
                    selectInput(
                        inputId = "stockDataset",
                        label = "Load stock data:",
                        choices = c("", `Preumococcal pneumonia, Brazil`="pnas_brazil")
                    )
                ),
                md_stepper_step(
                    "Identify Outcome Data",
                    uiOutput("outcomeColUI"),
                    uiOutput("denomColUI")
                ),
                md_stepper_step(
                    "Identify Time Data",
                    p("Which is the time variable in your data?"),
                    uiOutput("timeColUI"),
                    p("What is the date format of the time variable?"),
                    uiOutput("timeFormatUI"),
                    p("Does the time variable contain monthly or quarterly observations?"),
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
                    )
                ),
                md_stepper_step(
                    "Identify Grouping",
                    uiOutput("groupColUI")
                ),
                md_stepper_step(
                    "Run Analysis",
                    actionButton("analyze", "Run Analysis"),
                    textOutput("analysisStatus"),
                    tableOutput("analysisResults")
                )
            )
        )
    )
)

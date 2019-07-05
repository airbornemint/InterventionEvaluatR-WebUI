#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("InterventionEvaluatR",
    tabPanel(
        "Load Data",
        h1("Welcome to InterventionEvaluatR"),
        p("Let's begin by loading some data to analyze. You can load your own data, or you can choose a stock dataset below."),
        selectInput(
            inputId = "stockDatasetName",
            label = "Load stock data:",
            choices = c("Preumococcal pneumonia, Brazil")
        ),
        actionButton("next.outcome", "Next: Specify outcome variable"),
        value="load"
    ),
    tabPanel(
        "Outcome Variables",
        h1("Specify Outcome Variables"),
        uiOutput("outcomeColUI"),
        uiOutput("denomColUI"),
        actionButton("next.time", "Next: Specify time variable"),
        value="outcome"
    ),
    tabPanel(
        "Time Variable",
        h1("Specify Time Variable"),
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
        ),
        actionButton("next.grouping", "Next: Specify grouping"),
        value="time"
    ),
    tabPanel(
        "Grouping",
        h1("Specify Grouping"),
        uiOutput("groupColUI"),
        actionButton("next.analysis", "Next: Begin analysis"),
        value="grouping"
    ),
    tabPanel(
        "Analysis",
        h1("Analysis"),
        actionButton("analyze", "Begin Analysis"),
        textOutput("analysisStatus"),
        tableOutput("analysisResults"),
        value="analysis"
    ),
    header=conditionalPanel("output.showPlot", plotOutput(
        outputId = "previewPlot"
    )),
    fluid=FALSE,
    id="mainNav"
))

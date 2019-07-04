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
shinyUI(fluidPage(

    # Application title
    titlePanel("InterventionEvaluatR"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(
                outputId = "previewPlot"
            ),
            
            h3("Load data"),
            
            h3("Data description"),
            
            h3("Outcome"),
            uiOutput("outcomeColUI"),
            uiOutput("denomColUI"),
            
            h3("Time"),
            uiOutput("timeColUI"),
            textInput(
                inputId = "timeFormat",
                label = "Time Format:",
                value = "YYYY-MM-DD"
            ),
            selectInput(
                inputId = "obsFreq",
                label = "Observation frequency:",
                choices = c("Monthly", "Quarterly")
            ),
            selectInput(
                inputId = "yearStart",
                label = "Year:",
                choices = c("January", "June")
            ),
            dateInput(
                inputId = "postStart",
                label = "Post Start:"
            ),
            dateInput(
                inputId = "evalStart",
                label = "Eval Start:"
            ),
            
            h3("Stratification"),
            uiOutput("groupColUI"),
            
            h3("Fine-tuning"),
        
            h3("Summary"),
            actionButton("analyze", "Begin Analysis"),
            textOutput("analysisStatus"),
            tableOutput("analysisResults")
        )
    )
))

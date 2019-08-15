library(InterventionEvaluatR.Web)
library(shiny)

shinyApp(ui = evaluatr.web.ui(), server = evaluatr.web.server)


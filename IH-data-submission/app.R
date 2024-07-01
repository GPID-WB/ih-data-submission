#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fastverse)
library(shinyjs)





ui <- fluidPage(

    titlePanel("PIP Innovation Hub data submission")
)

server <- function(input, output, session) {



}

shinyApp(ui     = ui,
         server = server)




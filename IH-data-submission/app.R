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






# define objects
authorized_emails <- c("dmahler@worldbank.org",
                       "stettehbaah@worldbank.org",
                       "espen.prydz@gmail.com",
                       "snakamura2@worldbank.org",
                       "zprinsloo@worldbank.org")


ui <- fluidPage(

    titlePanel("PIP Innovation Hub data submission"),

    shinyFeedback::useShinyFeedback(),

    textInput("user", "Please supply your email:")
)

server <- function(input, output, session) {

    # Debounce the email input to delay evaluation
    debounced_email <- debounce(reactive(input$user), 1000)

    # Validate user
    observe({
        email <- debounced_email()

        if (email != "") {
            # Check if email has a valid domain
            valid_domain <- grepl("@.*\\.(com|org)$", email)

            if (valid_domain) {
                if (email %in% authorized_emails) {
                    shinyFeedback::showFeedbackSuccess("user", "Email authorized")
                } else {
                    shinyFeedback::showFeedbackDanger("user", "Email not authorized")
                }
            } else {
                shinyFeedback::hideFeedback("user")
            }
        } else {
            shinyFeedback::hideFeedback("user")
        }
    })

}

shinyApp(ui     = ui,
         server = server)




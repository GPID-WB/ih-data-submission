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
essential_vars <- c("code",
                    "region_code",
                    "reporting_level",
                    "welfare_type",
                    "poverty_line",
                    "headcount")

ui <- fluidPage(

    titlePanel(title = "PIP Innovation Hub data submission"),

    shinyFeedback::useShinyFeedback(),

    textInput(inputId = "user",
              label   = "Please supply your email:"),

    fileInput(inputId     = "upload",
              label       = "Upload dta file:",
              buttonLabel = "Upload",
              accept      = c(".dta")),
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
                    shinyFeedback::showFeedbackSuccess("user",
                                                       "Email authorized")
                } else {
                    shinyFeedback::showFeedbackDanger("user",
                                                      "Email not authorized")
                }
            } else {
                shinyFeedback::hideFeedback("user")
            }
        } else {
            shinyFeedback::hideFeedback("user")
        }
    })


    # get data() reactive
    dta <- reactive({

        req(input$upload_dta)

        ext <- tools::file_ext(input$upload$name)
        cols <- switch(ext,
                       csv = vroom::vroom(input$upload_dta$datapath, delim = ";"),
                       tsv = vroom::vroom(input$upload_dta$datapath, delim = "\t"),
                       dta = haven::read_dta(input$upload_dta$datapath),
                       validate("Invalid file: Please upload a .dta file")
        ) |>
            colnames()
        if (!all(essential_vars %in% cols)) {
            paste0(c("Not all required variables are present. The data should at least contain: ",
                     paste0(essential_vars,
                            collapse = ", ")),
                   collapse = "")
        }
    })

}

shinyApp(ui     = ui,
         server = server)




#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#-------------------------------------------------------------------------------
# Load packages
library(shiny)
library(fastverse)
library(shinyjs)


#-------------------------------------------------------------------------------
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
dir <- "P:/04.GPID_team/PIP-innovation-hub"

#-------------------------------------------------------------------------------
# Some data
#dt_sim_a <- haven::read_dta(file = fs::path(dir, "data-test"))

#-------------------------------------------------------------------------------
# UI

ui <- fluidPage(


    # 1) Title
    titlePanel(title = "PIP Innovation Hub data submission"),

    # 2) Init feedback
    shinyFeedback::useShinyFeedback(),

    # 3) User
    textInput(inputId = "user",
              label   = "Please supply your email:"),

    # 4) Upload dta file
    fileInput(inputId     = "upload_dta",
              label       = "Upload dta data submission file:",
              buttonLabel = "Upload",
              accept      = c(".dta")),

    # 5) Display dta file
    tableOutput("head")


    # numericInput("n",
    #              "Rows of dta to display",
    #              value = 5,
    #              min   =  1,
    #              step  = 1),





)

#-------------------------------------------------------------------------------
# Server
server <- function(input, output, session) {

    # ----------------------------------
    # 1) User

    ## Debounce the email input to delay evaluation
    debounced_email <- debounce(reactive(input$user), 1000)

    ##  Validate user
    observe({
        email <- debounced_email()

        if (email != "") {

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


    #------------------------------------
    # 2) Upload dta file
    dta <- reactive({

        req(input$upload_dta)

        ext <- tools::file_ext(input$upload_dta$name)
        df_dta <- switch(ext,
                         csv = vroom::vroom(input$upload_dta$datapath,
                                            delim = ";"),
                         tsv = vroom::vroom(input$upload_dta$datapath,
                                            delim = "\t"),
                         dta = haven::read_dta(input$upload_dta$datapath),
                         validate("Invalid file: Please upload a .dta file")
        )
        cols <- df_dta |>
            colnames()
        if (!all(essential_vars %in% cols)) {
            paste0(c("Not all required variables are present. The data should at least contain: ",
                     paste0(essential_vars,
                            collapse = ", ")),
                   collapse = "")
        } else {
            df_dta
        }
    })

    #------------------------------------
    # 3) Display dta file
    output$head <- renderTable({
        head(dta(), 2) #input$n)
    })

}

shinyApp(ui     = ui,
         server = server)




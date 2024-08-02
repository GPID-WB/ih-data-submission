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
library(RJSONIO)
library(rjson)


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
                    "headcount_default",
                    "headcount_estimate")
dir <- "P:/04.GPID_team/PIP-innovation-hub"
#dir <- "C:/Users/wb612474/OneDrive - WBG/pip_technical_work/ih-data-submission"



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
    tableOutput("dta"),

    # 6) Upload json file
    fileInput(inputId     = "upload_meta",
              label       = "Upload xlsx metadata:",
              buttonLabel = "Upload",
              accept      = c(".xlsx")),



    # 7) Display dta file
    tableOutput("metadata"),

    # method text box
    textAreaInput("upload_method",
                  "Paste at most 3 paragraphs on the methodology:",
                  rows = 3),

    actionButton(inputId = "submit_method",
                 label   = "Submit methodology"),

    #tableOutput("method"),
    #verbatimTextOutput("check")


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

            usr <- gsub("@worldbank.org",
                        replacement = "",
                        x = input$user)
            haven::write_dta(df_dta,
                             fs::path(dir,
                                      paste0(usr,
                                             "_",
                                             Sys.Date(),
                                             ".dta") ))
            df_dta
        }
    })

    #------------------------------------
    # 3) Display dta file
    output$dta <- renderTable({

        head(dta(), 2)
    })

    #------------------------------------
    # 4) Display metadata
    output$metadata <- renderTable({

        req(input$upload_meta)

        ext <- tools::file_ext(input$upload_meta$name)
        df <- switch(ext,
                     xlsx = readxl::read_excel(path = input$upload_meta$datapath),
                     validate("Invalid file: Please upload a xlsx file")
        )

        usr <- gsub("@worldbank.org",
                    replacement = "",
                    x = input$user)
        writexl::write_xlsx(x    = df,
                            path = fs::path(dir,
                                            paste0(usr,
                                                   "_",
                                                   Sys.Date(),
                                                   ".xlsx") ))

        head(df, 1)

    })

    #------------------------------------
    # 4) Method save as md

    rv <- reactiveValues(file_text = NULL)

    observeEvent(input$submit_method, {

        req(input$upload_method)
        req(input$submit_method)


        rmd_content <- paste0(input$upload_method)
        rv$file_text <- as.character(rmd_content)
        usr <- gsub("@worldbank.org",
                    replacement = "",
                    x = input$user)

        rcon <- file(fs::path(dir,
                              paste0(usr,
                                     "_",
                                     Sys.Date(),
                                     ".md")),
                     "w") # Create Rmarkdown file
        cat(rmd_content,
            file = rcon) # Write your content to Rmarkdown file
        close(rcon)


    })

}

shinyApp(ui     = ui,
         server = server)




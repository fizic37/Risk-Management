#' login UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_login_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    div(
      tags$head(tags$style('body {backround-color: #ffffff;}')),
      id = ns("loginpage"),
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      wellPanel(
        #
        tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#c92052; font-weight:600;"),
        textInput(inputId = ns("userName"),  placeholder = "Username",
                  label = tagList(icon("user"), "Username"),value = ""),
        br(),
        passwordInput(inputId = ns("passwd"), placeholder = "Password",value = "",
                      label = tagList(icon("unlock-alt"), "Password")),
        br(),
        div(style = "text-align: center; color: #ffffff",
            actionButton(inputId = ns("login"), "SIGN IN",
                         style = "color: white; background-color:#20c997;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
            shinyjs::hidden(div(id = ns("nomatch"),
                                tags$p("Oops! Incorrect username or password!",
                                       style = "color: #c92052; font-weight: 600;padding-top: 5px;font-size:16px;",
                                       class = "text-center")  )) )  )  )
  )
}
    
#' login Server Functions
#'
#' @noRd 
mod_login_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    credentials <- readRDS("R/credentials/credentials.rds")
    
    observe({if (vals$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- sodium::password_verify(pasmatch, Password)
            if(pasverify) {
              vals$login <- TRUE
              removeUI(selector = "#login_ui_1-loginpage")
              vals$user_type <- dplyr::pull(.data = dplyr::filter(.data = credentials,username_id==Username),permission)
              vals$user_name <- dplyr::pull(.data = dplyr::filter(.data = credentials,username_id==Username),username_id)
              }
            
            else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
      
      
    })
    
  })
}
    
## To be copied in the UI
# mod_login_ui("login_ui_1")
    
## To be copied in the server
# mod_login_server("login_ui_1")

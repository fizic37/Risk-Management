#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    
    tags$ul(
      h4("Provizioane specifice - plati de garantii"),
      tags$li(a("Se uploadeaza fisierul de provizioane din Charisma", href="#shiny-tab-plati", "data-toggle" = "tab") ),
      br(),
      tags$li(a("Se genereaza catre contabilitate adresa Word de regularizare lunara a provizioanelor specifice",
                href="#shiny-tab-plati", "data-toggle" = "tab")),
      
      tags$li(HTML("Coeficientii IFRS9 sunt vizibili ", 
                   as.character(actionLink(inputId = ns("coeficienti_plati"),"aici")) ))
      
      
    )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id, parrent){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    observeEvent(input$coeficienti_plati,{
      bs4Dash::updateTabItems(session = parrent,inputId = "sidebar",selected = 2)
      })
})
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# mod_home_server("home_ui_1")

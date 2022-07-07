#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  
  
  bs4Dash::sidebarMenuOutput(outputId = ns("sidebar")) 
  
  
}
    
#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    risk_user_sidebar <- bs4Dash::sidebarMenu( id = ns("tabs"),
      
      bs4Dash::menuItem( tabName = "home",   text = "Home",icon = icon("home"),selected = FALSE  ),
      
      bs4Dash::menuItem(tabName = "plati", text = "Provizioane plati", 
                               icon = icon("euro-sign"),  selected = FALSE),
      
      bs4Dash::menuItem(tabName = "solduri",text = "Solduri de garantii",
                        icon = icon("book"),selected = FALSE),
      
      bs4Dash::menuItem(tabName = "crc",text = "CRC si CIP", icon = icon("copyright"), selected = F),
      
      bs4Dash::menuItem(tabName = "ifrs",text = "IFRS9", icon = icon("italic"), selected = TRUE),
      
      bs4Dash::menuItem(tabName = "admin",text = "Admin", icon = icon("lock-open"), selected = FALSE)
      
    )
    
    output$sidebar <- bs4Dash::renderMenu(risk_user_sidebar)
    
   
    observeEvent(input$tabs,{ vals$sidebar_selected <- c(vals$sidebar_selected,input$tabs)
    })
    
    
 
  })
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_ui_1")

#' ifrs_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ifrs_database_ui <- function(id){
  ns <- NS(id)
  fluidPage( br(),
  fluidRow( column(width = 6,
    selectInput(ns("baza_ifrs_date"),label = "Selecteaza data bazei de date", choices = c())),
    
    column(width = 6, downloadLink(ns("down_ifrs_database"),"Click to download selected IFRS database",
                                   class = "glyphicon glyphicon-download")) ),
  
  DT::dataTableOutput(ns("baza_ifrs")), br(), br(), 
  
  shinyWidgets::actionBttn( inputId = ns("change_view"),label = "Change Table View", style = "stretch", 
    color = "success", icon = icon("toggle-on")) )
}
    
#' ifrs_database Server Functions
#'
#' @noRd 
mod_ifrs_database_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    portofoliu_ifrs <- readRDS("R/reactivedata/ifrs/portofoliu_ifrs.rds")
    
    vals_ifrs <- reactiveValues(portofoliu_ifrs = portofoliu_ifrs)
    
    updateSelectInput(session = session,inputId = 'baza_ifrs_date',
                choices = vals_ifrs$portofoliu_ifrs$data_raport %>% unique() %>% sort(decreasing = TRUE))
   
    observe({ req(input$baza_ifrs_date != "")
      vals_ifrs$begining_year <- lubridate::make_date(year = lubridate::year(input$baza_ifrs_date)-1,month = 12,day = 31)
      
      if ( input$change_view==0 || input$change_view %% 2 == 0) {
        vals_ifrs$view_table <- vals_ifrs$portofoliu_ifrs %>% 
          dplyr::filter(data_raport == as.Date(input$baza_ifrs_date)) %>%
          dplyr::group_by(categorie_contaminata, stage) %>%
          dplyr::summarise(expunere = sum(expunere_mil_lei) * 1000000) %>%
          dplyr::arrange(desc(categorie_contaminata), stage) %>%
          dplyr::mutate(stage = as.character(stage)) %>%
          janitor::adorn_totals(where = "row", fill = "-", name = "Total")
        vals_ifrs$caption <- paste0("Sinteza IFRS9 la data de ",input$baza_ifrs_date)
        vals_ifrs$perc_col <- NULL
      }
      else if ( input$change_view %% 2 == 1) {
        vals_ifrs$view_table <- vals_ifrs$portofoliu_ifrs %>% 
          dplyr::filter(data_raport %in% c(as.Date(input$baza_ifrs_date), vals_ifrs$begining_year)) %>%
          dplyr::group_by(categorie_contaminata,stage,data_raport) %>% 
          dplyr::summarise(expunere = sum(expunere_mil_lei) * 1000000) %>%
          dplyr::arrange(desc(categorie_contaminata), stage, data_raport) %>% 
          dplyr::mutate(stage = as.character(stage)) %>%
          dplyr::mutate(Variatie_Inceputul_anului = 
                          ifelse(data_raport==vals_ifrs$begining_year, NA, expunere/dplyr::lag(expunere)-1)) %>%
          dplyr::filter(data_raport !=  vals_ifrs$begining_year) %>%
          dplyr::select(-data_raport)
        
        vals_ifrs$caption <- paste0("Evolutia calitatii portofoliului de garantii la data de ", input$baza_ifrs_date)
        vals_ifrs$perc_col <- 4
      }
      
      
      output$baza_ifrs <- DT::renderDataTable( { req( vals_ifrs$view_table,vals_ifrs$caption )
        dt_generate_function( df = vals_ifrs$view_table,show_buttons=TRUE, caption = vals_ifrs$caption,
                              round_col = 3, perc_col = vals_ifrs$perc_col)  } )
      
      output$down_ifrs_database <- downloadHandler(filename = function(){paste0("IFRS_database_",input$baza_ifrs_date,".csv")},
                      content = function(file) {readr::write_csv(x =  vals_ifrs$portofoliu_ifrs %>% 
                                    dplyr::filter(data_raport == input$baza_ifrs_date),file = file) })
      
     })
    
    })
  
}
    
## To be copied in the UI
# mod_ifrs_database_ui("ifrs_database_ui_1")
    
## To be copied in the server
# mod_ifrs_database_server("ifrs_database_ui_1")

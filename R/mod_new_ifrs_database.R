#' new_ifrs_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_new_ifrs_database_ui <- function(id){
  ns <- NS(id)
  
  fluidPage( br(),
            fluidRow(
              column(
                width = 6,
                shinyWidgets::airMonthpickerInput(
                  ns("data_baza_ifrs"),
                  label = "Selecteaza data bazei de date",
                  value = readRDS("R/reactivedata/ifrs/scenarii_folosite.rds") %>%
                    dplyr::pull(data_raport) %>% max(),
                  autoClose = TRUE,
                  language = "ro"
                )
              ),
              
              column(
                width = 6,
                br(),
                shinyWidgets::downloadBttn(
                  ns("down_ifrs_database"),
                  label = "Click to download selected IFRS database",
                  style = "stretch",
                  color = "success",
                  size = "sm"
                )
              ),
              
              column(width = 12, DT::dataTableOutput(ns("baza_ifrs"))),
              
              column(width = 12, br()),
              
              column(width = 6, DT::dataTableOutput(ns(
                "scenarii_folosite"
              ))),
              
              column(width = 6, DT::dataTableOutput(ns(
                "coeficienti_folositi"
              )))
            ))

}
    
#' new_ifrs_database Server Functions
#'
#' @noRd 
mod_new_ifrs_database_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    database_ifrs <- readRDS("R/reactivedata/ifrs/new_database_ifrs.rds")
    
    vals_ifrs_database <- reactiveValues()
    
    
    observeEvent(input$data_baza_ifrs,{
      vals_ifrs_database$data_raport <- input$data_baza_ifrs %>% lubridate::ceiling_date(unit = "months")-1
      
    })
    
    database_selected <- eventReactive( vals_ifrs_database$data_raport,{
      database_ifrs %>% dplyr::filter(data_raport == vals_ifrs_database$data_raport)
    })
    
    coeficienti_folositi <- eventReactive( vals_ifrs_database$data_raport,{
      readRDS("R/reactivedata/ifrs/coeficienti_folositi.rds") %>% dplyr::filter(data_raport == vals_ifrs_database$data_raport) })
    
    scenarii_folosite <- eventReactive( vals_ifrs_database$data_raport,{ 
      readRDS("R/reactivedata/ifrs/scenarii_folosite.rds") %>% dplyr::filter(data_raport == vals_ifrs_database$data_raport) %>%
        dplyr::arrange(desc(Probability))
     
      })
    
    
    output$baza_ifrs <- DT::renderDataTable({ req( database_selected(),scenarii_folosite() )
     dt_generate_function( round_col = 2:5, show_buttons = TRUE,
      caption = paste0("Sinteza provizioanelor IFRS9 la data de ",vals_ifrs_database$data_raport),
      df = database_selected() %>% dplyr::group_by(stage_prob_max) %>% dplyr::summarise(Provizion_prob_max =
            sum(Provizion_prob_max)) %>% dplyr::left_join( database_selected() %>% dplyr::group_by(stage_prob_mean) %>%
        dplyr::summarise(Provizion_prob_mean = sum(Provizion_prob_mean)),
        by = c("stage_prob_max" = "stage_prob_mean") ) %>%
        dplyr::left_join( database_selected() %>% dplyr::group_by(stage_prob_minimum) %>%
            dplyr::summarise(Provizion_prob_minimum = sum(Provizion_prob_minimum)),
          by = c("stage_prob_max" = "stage_prob_minimum")   ) %>%
        dplyr::mutate(Provizion_mediu = Provizion_prob_max * scenarii_folosite()$Probability[1] +
            Provizion_prob_mean * scenarii_folosite()$Probability[2] + Provizion_prob_minimum * 
              scenarii_folosite()$Probability[3]) %>%
        dplyr::rename_at(.vars = 1,  ~ "stage") %>% janitor::adorn_totals(where = "row") 
      )
       
    }  )
    
    output$scenarii_folosite <- DT::renderDataTable( {req(scenarii_folosite())
      dt_generate_function(df = scenarii_folosite(), perc_col=1:3, digits_perc=2,show_buttons=TRUE,
      caption = paste0("Scenariile folosite pentru calculul provizoanelor la data de ", vals_ifrs_database$data_raport)
                           ) })
    
    output$coeficienti_folositi <- DT::renderDataTable( { req(coeficienti_folositi)
      dt_generate_function( df = coeficienti_folositi() %>% dplyr::select(1:5) %>% t() %>% as.data.frame() %>% 
      dplyr::rename_at(.vars = 1, .funs = ~"Valori"), perc_col = 1, digits_perc = 0, rownames = TRUE,show_buttons=TRUE,
      caption = paste0("Coeficientii de provizionare folositi pentru calculul provizioanelor la data de ",
                       vals_ifrs_database$data_raport) )  })
  
    })
}
    
## To be copied in the UI
# mod_new_ifrs_database_ui("new_ifrs_database_ui_1")
    
## To be copied in the server
# mod_new_ifrs_database_server("new_ifrs_database_ui_1")

#' updated_new_database_ifrs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_updated_new_database_ifrs_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
    
    bs4Dash::box( title="Baza de date a provizioanelor IFRS9 calculate",
                  status = "info",width = 12, collapsible = T, collapsed = F,
                  maximizable = TRUE, icon = icon("database"),
  fluidPage( br(),
             fluidRow(
               column(
                 width = 4,
                 tagList(
                 shinyWidgets::airMonthpickerInput(
                   ns("data_baza_ifrs"),
                   label = "Selecteaza data bazei de date",
                   value = readRDS("R/reactivedata/ifrs/scenarii_folosite.rds") %>%
                     dplyr::pull(data_raport) %>% max(),
                   autoClose = TRUE,
                   language = "ro" ),
              
               
                 br(),
                 shinyWidgets::downloadBttn(
                   ns("down_ifrs_database"),
                   label = "Download selected IFRS database",
                   style = "stretch",
                   color = "success",
                   size = "sm"
                 )
               ) ),
               
               column(width = 8, DT::dataTableOutput(ns("baza_ifrs"))),
               
              
               column(width = 12, shinyWidgets::pickerInput(ns("select_scenariu"),
                      label = "Selecteaza scenariul macroeconomic",width = "300px",
                      choices = c("Scenariul de baza", "Scenariul 2", "Worst scenario")), br(),
                      DT::dataTableOutput(ns("baza_detaliata")))
               
             ))  ),
  bs4Dash::box(title = "Scenariile macroeconomice si coeficientii folositi",
               status = "info",width = 12, collapsible = T, collapsed = T,
               maximizable = TRUE, icon = icon("chart-area"),
               
               column(width = 6, DT::dataTableOutput(ns(
                 "scenarii_folosite"
               ))),
               
               column(width = 6, DT::dataTableOutput(ns(
                 "coeficienti_folositi"
               )))
             ))
  
}
    
#' updated_new_database_ifrs Server Functions
#'
#' @noRd 
mod_updated_new_database_ifrs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    database_ifrs <- readRDS("R/reactivedata/ifrs/update_new_database_ifrs.rds")
    
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
    
    database_selected_processed <- reactive({ req(database_selected())
      df = database_selected() %>% dplyr::relocate(CUI, .before = "Beneficiar") %>%
        dplyr::select( -(dplyr::starts_with("woe")),-(dplyr::starts_with("prob")),
                       -data_raport, -contragarantii,-are_restante_peste_30_zile_6M,
                       -incidente_6M,-Este_in_interdictie_6M,-`Cod Partener` ) %>% 
        dplyr::relocate(dplyr::starts_with("stage"), .before = categorie_contaminata) %>% 
        dplyr::relocate(dplyr::starts_with("Provizion"), .after = expunere_mil_lei) %>% 
        dplyr::mutate(expunere_mil_lei =  expunere_mil_lei * 1000000) %>% 
        dplyr::arrange(desc(Provizion_prob_max)) %>% 
        dplyr::mutate(dplyr::across(.cols = c(dplyr::starts_with("stage"),categorie_contaminata),.fns = ~as.factor(.x)))
      })
    
    output$baza_detaliata <- DT::renderDataTable( {req( database_selected_processed() )
      # dt_generate_function does not work when filtering
     DT::datatable( rownames = FALSE, filter = "top",  options = list(dom="tp", pageLength=5,
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
              paste0("Lista detaliata a portofoliului de garantii la data de ", vals_ifrs_database$data_raport,
                     " in ", input$select_scenariu," . Click pe beneficiar pentru mai multe info")),
              selection = list(mode = "single",selected = NULL, target = "row"),
                    data = switch(EXPR = input$select_scenariu,
                                  "Scenariul de baza" = database_selected_processed() %>% dplyr::select(CUI, Beneficiar,
                                                    'Stage scenariul de baza'=stage_prob_max,'Categorie contaminata'=categorie_contaminata,
                                                    'Sold garantii'=expunere_mil_lei, ProvizionContabil,
                                                    Provizion_prob_max) %>%
                                    dplyr::arrange(desc(6)),
                                  "Scenariul 2" = database_selected_processed() %>% dplyr::select(CUI, Beneficiar,
                                            'Stage scenariul 2'=stage_prob_max,'Categorie contaminata'=categorie_contaminata,
                                            'Sold garantii' = expunere_mil_lei, ProvizionContabil,Provizion_prob_mean) %>%
                                    dplyr::arrange(desc(6)),
                                  "Worst scenario" = database_selected_processed() %>% dplyr::select(CUI, Beneficiar,
                                              'Stage worst scenario'=stage_prob_max,'Categorie contaminata'=categorie_contaminata,
                                              'Sold garantii'=expunere_mil_lei, ProvizionContabil,Provizion_prob_minimum) %>%
                                    dplyr::arrange(desc(6))   ) ) %>% DT::formatRound(columns = 5:7,digits = 0)  })
    
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
    
    observeEvent( input$baza_detaliata_rows_selected,
      {
        vals_ifrs_database$beneficiar_selectat <-
          database_selected_processed()  %>%   dplyr::slice(input$baza_detaliata_rows_selected)
     
        showModal(session = session,
                  modalDialog(
                    title = paste(
                      "Beneficiarul ",vals_ifrs_database$beneficiar_selectat$Beneficiar,
                      ", CUI ", vals_ifrs_database$beneficiar_selectat$CUI, ", categorie ", 
                      vals_ifrs_database$beneficiar_selectat$categorie_contaminata ),
                    size = "l",  footer = list( modalButton(label = "Close", icon = icon("times")) ),
                    
                    DT::dataTableOutput(ns("info_beneficiar"))
                  ))
        output$info_beneficiar <- DT::renderDataTable(
          dt_generate_function(df = vals_ifrs_database$beneficiar_selectat %>%
          dplyr::select('Are restante peste 30 zile'=are_restante_peste_30_zile, 
                        'Scor serviciu datorie' = scor_serv_datorie,
                        "Are incidente noi in ultimele 6 luni" = new_incidente_6M,
                        'Total incidente de plati'=total_incidente,
                        "Este in interdictie bancara de a emite cecuri"=Este_in_interdictie,
                        'A intrat in interdictie bancara in ultimele 6 luni'=new_Este_in_interdictie_6M
                        )))
      })
        
  })
}
    
## To be copied in the UI
# mod_updated_new_database_ifrs_ui("updated_new_database_ifrs_ui_1")
    
## To be copied in the server
# mod_updated_new_database_ifrs_server("updated_new_database_ifrs_ui_1")

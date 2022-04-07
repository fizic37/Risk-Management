#' new_ifrs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_new_ifrs_ui <- function(id){
  ns <- NS(id)
 fluidPage( br(),
           shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
           shinyFeedback::useShinyFeedback(feedback = TRUE,toastr = TRUE),  
           
           fluidRow(column(width = 3,
             shinyWidgets::airMonthpickerInput(inputId = ns("data_raport"),label = "Data raportului IFRS9 ",
                                               value=Sys.Date(), autoClose=TRUE, language="ro") ),
             column(width = 5, uiOutput(ns("show_calculate"))),
            
             
             column(width = 4, DT::dataTableOutput(ns("coeficienti_depreciate"))),
             
            
             column(width = 8, DT::dataTableOutput(ns("final_provizioane"))),
             
             column(width = 4, br(), br(), br(),br(), br(),br(), uiOutput(ns("show_save"))),
             
             column(width = 12, br()),
             
             column(width = 4, DT::dataTableOutput(ns("main_scenario_output"))),
             
             column(width = 4, DT::dataTableOutput(ns("mean_scenario_output"))),
             
             column(width = 4, DT::dataTableOutput(ns("minimum_scenario_output"))),
             
             column(width = 4, uiOutput(ns("show_main_scenario"))),
                    
             
             column(width = 4, uiOutput(ns("show_mean_scenario"))),
                   
             column(width = 4, uiOutput(ns("show_minimum_scenario")))
                    
             )
  )
}
    
#' new_ifrs Server Functions
#'
#' @noRd 
mod_new_ifrs_server <- function(id, vals_portofoliu, parrent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    vals_ifrs <- reactiveValues()
    
   coeficienti_depreciate <- reactive({ req(vals_ifrs$data_raport)
     readRDS( 'R/reactivedata/ifrs/coef_non_ifrs.rds' ) %>% 
       dplyr::filter( FromDate <= vals_ifrs$data_raport, ToDate>= vals_ifrs$data_raport)
    })
    
  
    
   output$show_calculate <- renderUI({ tagList( br(),
     shinyWidgets::actionBttn(inputId = ns("calculate"),label = "Calculeaza provizioanele IFRS9",
              style = "stretch",color = "success",icon = icon("calculator") ) 
   )
   })
   
    observeEvent(vals_portofoliu$view1_portofoliu,{ req(vals_portofoliu$view1_portofoliu)
      shinyWidgets::updateAirDateInput(session=session, inputId = 'data_raport',
                        value = max(vals_portofoliu$view1_portofoliu$anul_de_raportare) +1 )
    })
    
    observeEvent(input$data_raport,{
      vals_ifrs$data_raport <- input$data_raport %>% lubridate::ceiling_date(unit = "months")-1
    })
  
    
    observeEvent(input$calculate,{ req(vals_ifrs$data_raport, vals_portofoliu$view1_portofoliu)
      
      view_baza_date_crc <- readRDS(file = "R/reactivedata/crc/view_baza_date_crc.rds")
      view_cip_database <- readRDS(file = "R/reactivedata/cip/view_cip_database.rds")
      
      if ( !vals_ifrs$data_raport %in% vals_portofoliu$view1_portofoliu$anul_de_raportare) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                     text = "Nu detin portofoliul de garantii. Please Upload portofoliu file!")
        bs4Dash::updateTabItems(session = parrent_session,inputId = "sidebar_ui_1-tabs",selected = 'solduri')
      }
      
      else if( !vals_ifrs$data_raport %in%  view_baza_date_crc$`Data situatie risc global` ) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                     text = "Nu detin datele CRC necesare. Please Upload CRC file!")
        bs4Dash::updateTabItems(session = parrent_session,inputId = "sidebar_ui_1-tabs",selected = 'crc')
        
      }
      else if (!vals_ifrs$data_raport %in%  view_cip_database$data_raport) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                     text = "Nu detin datele CIP necesare. Please Upload CIP file!")
        bs4Dash::updateTabItems(session = parrent_session,
                                inputId = "sidebar_ui_1-tabs",selected = 'crc') 
        
      }
      
      else {
        scenarios <-  reactive({req(vals_ifrs$data_raport)
          
          readRDS( "R/reactivedata/ifrs/scenarios.rds") %>% dplyr::filter(From_Date <= vals_ifrs$data_raport,
          To_Date >= vals_ifrs$data_raport ) %>% dplyr::arrange(desc(Probability)) %>% dplyr::select(1:4) })
        
        output$show_calculate <- renderUI({DT::dataTableOutput(ns("scenarios_table")) })
        
        output$scenarios_table <- DT::renderDataTable(dt_generate_function(df = scenarios(),perc_col=1:3, digits_perc=2,
                    caption= "Scenariile de evolutie macroeconomica. Dublu-click pentru a modifica.",
                    editable="cell", show_buttons=TRUE))
        
        scenarios_reactive <<- reactive({
          if (is.null(input[['scenarios_table_cell_edit']])) {
            scenarios()
          } else {
            DT::editData(scenarios(),rownames = FALSE, info=input[['scenarios_table_cell_edit']]) }
        })
        
        Robor_3M_prob_max <- reactive({ req(scenarios_reactive())
          scenarios_reactive() %>% dplyr::slice(1) %>% dplyr::pull(Robor_3M)
        }) 
        
        Robor_3M_prob_mean <- reactive({ req(scenarios_reactive())
          scenarios_reactive() %>% dplyr::slice(2) %>% dplyr::pull(Robor_3M) })
        
        Robor_3M_prob_minimum <- reactive({ req(scenarios_reactive())
          scenarios_reactive() %>% dplyr::slice(3) %>% dplyr::pull(Robor_3M)  })
        
        growth_prob_max <- reactive({ req(scenarios_reactive())
          scenarios_reactive() %>% dplyr::slice(1) %>% dplyr::pull(Economic_Growth) })
        
        growth_prob_mean <- reactive({ req(scenarios_reactive())
          scenarios_reactive() %>% dplyr::slice(2) %>% dplyr::pull(Economic_Growth) })
        
        growth_prob_minimum <- reactive({ req(scenarios_reactive())
          scenarios_reactive() %>% dplyr::slice(3) %>% dplyr::pull(Economic_Growth)  })
        
        
        
        output$coeficienti_depreciate <- DT::renderDataTable(
          dt_generate_function( df = coeficienti_depreciate() %>% dplyr::select(1:5) %>% t() %>% as.data.frame() %>% 
              dplyr::rename_at(.vars = 1, .funs = ~"Valori"), perc_col = 1, digits_perc = 0, rownames = TRUE,
          caption = "Coeficientii de provizionare. Dublu click pentru a modifica", show_buttons=TRUE, editable=TRUE))
        
        coeficienti_depreciate_reactivi <<- reactive({
          if (is.null(input[['coeficienti_depreciate_cell_edit']])) {
            coeficienti_depreciate() %>% dplyr::select(1:5)
          } else {
          DT::editData(coeficienti_depreciate() %>% dplyr::select(1:5) %>% t() %>% as.data.frame() %>% 
            dplyr::rename_at(.vars = 1, .funs = ~"Indicatori"), rownames = TRUE,
            info=input[['coeficienti_depreciate_cell_edit']]) %>% t() %>% as.data.frame() }
        })
        
       
        
        
        portofoliu <<- reactive ({ req(coeficienti_depreciate_reactivi(), scenarios_reactive())
          baza_cui <- readRDS("R/reactivedata/bi_cui.rds")
          crc <- readRDS("R/reactivedata/crc/baza_date_crc.rds") %>% 
            dplyr::filter(`Data situatie risc global` == vals_ifrs$data_raport ) %>%
            dplyr::mutate(`Cod debitor` = as.character(`Cod debitor`)) %>%
            dplyr::left_join(baza_cui, by = c("Cod debitor" = "Unique ID")) %>%
            dplyr::group_by(Code) %>% dplyr::summarise(scor_serv_datorie = mean(scor_serv_datorie),
                are_restante_peste_30_zile = mean(are_restante_peste_30_zile))
          
          
          cip <- readRDS("R/reactivedata/cip/baza_date_cip.rds") %>% 
            dplyr::filter(data_raport == vals_ifrs$data_raport ) %>%
            dplyr::mutate(CUI = as.character(CUI)) %>% 
            dplyr::select(-bilete_majore) %>%
            dplyr::left_join(y = baza_cui, by = c("CUI" = "Unique ID"))
          rm("baza_cui")
          
          portofoliu <- readRDS("R/reactivedata/portofoliu/portof_database.rds") %>%
            dplyr::filter(anul_de_raportare == vals_ifrs$data_raport ) %>%
            dplyr::group_by(`Cod Partener`,Beneficiar, categorie_contaminata) %>%
            dplyr::summarise(expunere_mil_lei = sum(expunere)/1000000, contragarantii = sum(contragarantii)) %>% dplyr::ungroup() %>%
            dplyr::left_join(y = crc %>% dplyr::select(Code,scor_serv_datorie, are_restante_peste_30_zile),
                             by = c("Cod Partener" = "Code")) %>% 
            dplyr::left_join(y = cip %>% dplyr::select(-data_raport, -CUI), by = c("Cod Partener" = "Code")) %>% 
            dplyr::mutate(dplyr::across(.cols = c(total_incidente,Este_in_interdictie,
                                                  total_incidente_majore, A_fost_interdictie),    ~ifelse(is.na(.x),0,.x))) 
          
         
          rm("crc", "cip")
          
          portofoliu <- portofoliu %>% dplyr::mutate(woe_1Y_categorie_contaminata = ifelse(
            categorie_contaminata == "standard",0.373,-2.47),
            woe_3Y_categorie_contaminata = ifelse(categorie_contaminata == "standard",0.188,-0.526) )
            
          
          portofoliu <- portofoliu %>% dplyr::mutate( Scor_1Y_no_robor = ifelse(
            !is.na(are_restante_peste_30_zile),
            -3.13 + 0.00202*total_incidente + 1.1*Este_in_interdictie -2.68*scor_serv_datorie + 
              1.57*are_restante_peste_30_zile + 0.1*expunere_mil_lei -0.333*woe_1Y_categorie_contaminata, 
            -4.42 + 0.344*expunere_mil_lei + - 0.873*woe_1Y_categorie_contaminata ) ) %>%
            dplyr::mutate(prob_1Y_prob_max = 1/(1+exp(-(Scor_1Y_no_robor + ifelse(!is.na(are_restante_peste_30_zile),
                                    6.64*Robor_3M_prob_max(), 15.1*Robor_3M_prob_max())) ) ),
                          prob_1Y_prob_mean = 1/(1+exp(-(Scor_1Y_no_robor + ifelse(!is.na(are_restante_peste_30_zile),
                                      6.64*Robor_3M_prob_mean(), 15.1*Robor_3M_prob_mean())))),
                          prob_1Y_prob_minimum = 1/(1+exp(-(Scor_1Y_no_robor + ifelse( !is.na(are_restante_peste_30_zile),
                                    6.64*Robor_3M_prob_minimum(), 15.1*Robor_3M_prob_minimum()))))
            ) %>% dplyr::mutate( stage_prob_max = ifelse(
              categorie_contaminata %in% c("insolventa", "cerere_plata"), "stage3",
              ifelse(is.na(are_restante_peste_30_zile) & prob_1Y_prob_max < 0.1, "stage1",
              ifelse(!is.na(are_restante_peste_30_zile) & prob_1Y_prob_max < 0.035, "stage1", "stage2"))),
              stage_prob_mean = ifelse(categorie_contaminata %in% c("insolventa", "cerere_plata"), "stage3",
              ifelse(is.na(are_restante_peste_30_zile) & prob_1Y_prob_mean < 0.1, "stage1",
              ifelse(!is.na(are_restante_peste_30_zile) & prob_1Y_prob_mean < 0.035, "stage1", "stage2"))),
              stage_prob_minimum = ifelse(categorie_contaminata %in% c("insolventa", "cerere_plata"), "stage3",
              ifelse(is.na(are_restante_peste_30_zile) & prob_1Y_prob_minimum < 0.1, "stage1",
              ifelse(!is.na(are_restante_peste_30_zile) & prob_1Y_prob_minimum < 0.035, "stage1", "stage2")))
            )
          
          portofoliu <- portofoliu %>% dplyr::mutate(Scor_3Y_no_macro = ifelse(
            !is.na(are_restante_peste_30_zile),
            -1.24 + 0.00237*total_incidente + 0.855*Este_in_interdictie -2.18*scor_serv_datorie + 
              0.652*are_restante_peste_30_zile + 0.0982*expunere_mil_lei -0.947*woe_3Y_categorie_contaminata, 
            -0.944 ) ) %>% dplyr::mutate(
              prob_3Y_prob_max = 1 / (1 + exp(-(
                Scor_3Y_no_macro + ifelse(
                  !is.na(are_restante_peste_30_zile),
                  15.7 *Robor_3M_prob_max() -1.03*growth_prob_max(),
                  4.64 * Robor_3M_prob_max() -19.3*growth_prob_max())))),
              prob_3Y_prob_mean = 1 / (1 + exp(-(
                Scor_3Y_no_macro + ifelse(
                  !is.na(are_restante_peste_30_zile),
                  15.7 *Robor_3M_prob_mean() -1.03*growth_prob_max(),
                  4.64 * Robor_3M_prob_mean() -19.3*growth_prob_max())))),
              prob_3Y_prob_minimum = 1 / (1 + exp(-(
                Scor_3Y_no_macro + ifelse(
                  !is.na(are_restante_peste_30_zile),
                  15.7 *Robor_3M_prob_minimum() -1.03*growth_prob_minimum(),
                  4.64 * Robor_3M_prob_minimum() -19.3*growth_prob_minimum()))))
            )
          
          
          portofoliu <-  portofoliu %>% dplyr::mutate( Provizion_prob_max = ifelse(stage_prob_max=="stage1",
                  prob_1Y_prob_max *(expunere_mil_lei*1000000 - 
                    coeficienti_depreciate_reactivi()$Ajustare_ctg*contragarantii)*
                    coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_plati*
                    coeficienti_depreciate_reactivi()$Coef_provizionare_plati,
                  ifelse (stage_prob_max=="stage2",
                          prob_3Y_prob_max *(expunere_mil_lei*1000000 - 
                            coeficienti_depreciate_reactivi()$Ajustare_ctg*contragarantii)*
                            coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_plati*
                            coeficienti_depreciate_reactivi()$Coef_provizionare_plati,
                  ifelse(categorie_contaminata == "insolventa",
                         coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_insolvente *(expunere_mil_lei*1000000 - 
                          coeficienti_depreciate_reactivi()$Ajustare_ctg*contragarantii)*
                           coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_plati*
                           coeficienti_depreciate_reactivi()$Coef_provizionare_plati,
                         
                         (expunere_mil_lei*1000000 - coeficienti_depreciate_reactivi()$Ajustare_ctg*contragarantii)*
                           coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_plati*
                           coeficienti_depreciate_reactivi()$Coef_provizionare_plati ) ) ) ) %>%
                  
            dplyr::mutate(Provizion_prob_mean = ifelse(stage_prob_mean=="stage1",
                      prob_1Y_prob_mean *(expunere_mil_lei*1000000 - coeficienti_depreciate_reactivi()$Ajustare_ctg*contragarantii)*
                                    coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_plati*
                                    coeficienti_depreciate_reactivi()$Coef_provizionare_plati,
                      ifelse (stage_prob_mean=="stage2",
                          prob_3Y_prob_mean *(expunere_mil_lei*1000000 - coeficienti_depreciate_reactivi()$Ajustare_ctg*contragarantii)*
                                    coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_plati*
                                    coeficienti_depreciate_reactivi()$Coef_provizionare_plati,
                          Provizion_prob_max))) %>%
            
            dplyr::mutate( Provizion_prob_minimum = ifelse( stage_prob_minimum == "stage1",
                prob_1Y_prob_minimum * (expunere_mil_lei * 1000000 - coeficienti_depreciate_reactivi()$Ajustare_ctg * contragarantii ) *
                  coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_plati *
                  coeficienti_depreciate_reactivi()$Coef_provizionare_plati,
                ifelse (  stage_prob_minimum == "stage2",
                  prob_3Y_prob_minimum * ( expunere_mil_lei * 1000000 - coeficienti_depreciate_reactivi()$Ajustare_ctg *contragarantii) *
                    coeficienti_depreciate_reactivi()$Coef_trans_cereri_plata_plati *
                    coeficienti_depreciate_reactivi()$Coef_provizionare_plati,
                  Provizion_prob_max
                )
              )
            )
                         
          
          return(portofoliu)
        })
        
        output$main_scenario_output <- DT::renderDataTable({ req(portofoliu())
          dt_generate_function( portofoliu() %>% dplyr::group_by(stage_prob_max) %>% 
            dplyr::summarise(Sold_Garantii=sum(expunere_mil_lei), Provizion_IFRS9 = sum(Provizion_prob_max)) %>%
              dplyr::mutate( Sold_Garantii = Sold_Garantii*1000000 ) %>% janitor::adorn_totals(where = "row"),
            caption = "Distributia pe stage-uri in scenariul asteptat - probabilitatea maxima de realizare",
            round_col=2:3, show_buttons = TRUE)
          })
      
        output$mean_scenario_output <-  DT::renderDataTable({  req(portofoliu())
            dt_generate_function( portofoliu() %>% dplyr::group_by(stage_prob_mean) %>%
                dplyr::summarise(Sold_Garantii = sum(expunere_mil_lei), Provizion_IFRS9 = sum(Provizion_prob_mean)) %>%
                dplyr::mutate(Sold_Garantii = Sold_Garantii *
                                1000000) %>% janitor::adorn_totals(where = "row"),
              caption = "Distributia pe stage-uri in scenariul median - probabilitatea mediana de realizare",
              round_col = 2:3, show_buttons = TRUE )
          })
        
        output$minimum_scenario_output <- DT::renderDataTable({  req(portofoliu())
          dt_generate_function( portofoliu() %>% dplyr::group_by(stage_prob_minimum) %>%
                dplyr::summarise(Sold_Garantii = sum(expunere_mil_lei), Provizion_IFRS9 = sum(Provizion_prob_minimum)) %>%
                  dplyr::mutate(Sold_Garantii = Sold_Garantii * 1000000) %>% janitor::adorn_totals(where = "row"),
        caption = "Distributia pe stage-uri in scenariul cel mai putin asteptat - probabilitatea cea mai mica de realizare",
                                round_col = 2:3, show_buttons = TRUE )
        })
        
        output$show_save <- renderUI(shinyWidgets::actionBttn(ns("save"), icon = icon("save"),
                      style = "stretch",color = "success",label = "Salveaza provizioanele calculate"))
       
        output$show_main_scenario <- renderUI( shinyWidgets::downloadBttn(ns("down_main_scenario"),
                                   style = "stretch",color = "success",label = "Download main scenario data"))
        
        output$down_main_scenario <- downloadHandler(filename = function() {"expected_scenario.csv"},
                                                     content = function(file) { 
                                                       req(portofoliu())
        readr::write_csv(file = file, x = portofoliu() %>% dplyr::select(-Provizion_prob_minimum, -Provizion_prob_mean,
                -prob_3Y_prob_mean, -prob_3Y_prob_minimum)) })
        
        output$show_mean_scenario <- renderUI( shinyWidgets::downloadBttn(ns("down_mean_scenario"),
                  style = "stretch",color = "success",label = "Download median scenario data"))
        
        
        output$down_mean_scenario <-  downloadHandler( filename = function() {"mean_scenario.csv"},
            content = function(file) {   req(portofoliu())
              readr::write_csv(   file = file,
                x = portofoliu() %>% dplyr::select( -Provizion_prob_minimum,
                  -Provizion_prob_max,-prob_3Y_prob_max, -prob_3Y_prob_minimum) )  }    )
        
        output$show_minimum_scenario <- renderUI(shinyWidgets::downloadBttn(ns("down_minimum_scenario"),
            style = "stretch",color = "success",label = "Download worst scenario data"))
        
        output$down_minimum_scenario <- downloadHandler( filename = function() { "worst_scenario.csv"  },
            content = function(file) { req(portofoliu())
              readr::write_csv( file = file,
                x = portofoliu() %>% dplyr::select(-Provizion_prob_mean,-Provizion_prob_max,
                  -prob_3Y_prob_max, -prob_3Y_prob_mean)  )  } )
        
        output$final_provizioane <- DT::renderDataTable({ req(portofoliu())
          portofoliu() %>% dplyr::group_by(stage_prob_max) %>% dplyr::summarise(Provizion_prob_max =
              sum(Provizion_prob_max)) %>% dplyr::left_join(  portofoliu() %>% dplyr::group_by(stage_prob_mean) %>% 
              dplyr::summarise(Provizion_prob_mean = sum(Provizion_prob_mean)),
              by = c("stage_prob_max" = "stage_prob_mean")) %>%
            dplyr::left_join( portofoliu() %>% dplyr::group_by(stage_prob_minimum) %>% 
            dplyr::summarise(Provizion_prob_minimum = sum(Provizion_prob_minimum)), 
            by = c("stage_prob_max" = "stage_prob_minimum") ) %>% 
            dplyr::mutate( Provizion_mediu = Provizion_prob_max * scenarios_reactive()$Probability[1] + 
                    Provizion_prob_mean * scenarios_reactive()$Probability[2] + Provizion_prob_minimum * 
                      scenarios_reactive()$Probability[3]) %>%
            dplyr::rename_at(.vars = 1,  ~ "stage") %>% janitor::adorn_totals(where = "row") %>% 
            dt_generate_function(round_col = 2:5,caption="Sinteza provizioane IFRS9",show_buttons=TRUE) })
        
      
        }
          
    })
    
    observeEvent(input$save,{
      
      shinyWidgets::ask_confirmation(inputId = ns("confirm_save"),title = "Confirm",
                text = "Esti sigur ca vrei sa salvezi provizioanele calculate?",
      btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#c92052","#20c997"),type = "info")
      
    })
    
    observeEvent(input$confirm_save,{req(input$confirm_save == TRUE)
      
      database_ifrs <- readRDS("R/reactivedata/ifrs/new_database_ifrs.rds")
      
      vals_ifrs$df_new <- portofoliu() %>% dplyr::mutate(data_raport = vals_ifrs$data_raport) %>% 
        dplyr::select(-dplyr::contains("Scor"), -A_fost_interdictie,-total_incidente_majore)
      
      vals_ifrs$df_old <- database_ifrs
      vals_ifrs$element_id <- vals_ifrs$data_raport
      vals_ifrs$column_id = "data_raport"
      vals_ifrs$finalise_process_compare_df = FALSE
      
      callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = vals_ifrs, red="#c92052",green="#20c997")  
      
    })
    
    observeEvent(vals_ifrs$finalise_process_compare_df,{ req(vals_ifrs$finalise_process_compare_df == TRUE )
      
      saveRDS(object = vals_ifrs$df_new_prel, file = "R/reactivedata/ifrs/new_database_ifrs.rds")
      
      
      saveRDS(object = coeficienti_depreciate() %>% dplyr::mutate(data_raport = vals_ifrs$data_raport) %>%
                dplyr::bind_rows( readRDS("R/reactivedata/ifrs/coeficienti_folositi.rds") %>%
                                    dplyr::filter(data_raport != vals_ifrs$data_raport )),
              file = "R/reactivedata/ifrs/coeficienti_folositi.rds")
      
      saveRDS(object = scenarios_reactive() %>% dplyr::mutate(data_raport = vals_ifrs$data_raport) %>%
                dplyr::bind_rows( readRDS("R/reactivedata/ifrs/scenarii_folosite.rds") %>%
                                    dplyr::filter(data_raport != vals_ifrs$data_raport )),
              file = "R/reactivedata/ifrs/scenarii_folosite.rds")
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
            .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE)) 
      
      vals_ifrs$finalise_process_compare_df <- FALSE
    })
    
    
  })
}
    
## To be copied in the UI
# mod_new_ifrs_ui("new_ifrs_ui_1")
    
## To be copied in the server
# mod_new_ifrs_server("new_ifrs_ui_1")

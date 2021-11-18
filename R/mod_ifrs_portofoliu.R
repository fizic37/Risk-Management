#' ifrs_portofoliu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ifrs_portofoliu_ui <- function(id){
  ns <- NS(id)
  bs4Dash::tabsetPanel(id = ns("ifrs_panel"),  selected = T,type = "pills",
  
           
  
        tabPanel(title = "Database IFRS9", icon = icon("database"),value = "database_ifrs",
                 
                 mod_ifrs_database_ui("ifrs_database_ui_1")),
      
         tabPanel(title = "Calcul Provizioane IFRS9", icon = icon("square-root-alt"),
                  
                  tagList( br(),
                    
                           shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
                    shinyFeedback::useShinyFeedback(feedback = TRUE,toastr = TRUE),  
                    
                  fluidRow(column(width = 4,
         selectInput(inputId = ns("data_raport"),label = "Selecteaza data raportului IFRS9",choices = c())),
         
         column(width = 4, br(),  uiOutput(ns("show_button"))),
         
         column(width = 4, br(),shinyWidgets::prettyToggle(inputId = ns("show_info"),label_on = "Hide info below",
                label_off = "Click to show more info",icon_on = icon("eye-slash"),icon_off = icon("info"),
                value = FALSE,status_off = "success",status_on = "info" ) )
         ) ),
         
         textOutput(outputId = ns("info")), br(),
         
         DT::dataTableOutput(outputId = ns("sinteza_ifrs"))
         ),
         
        tabPanel(title = "Dezvoltare Modele IFRS9", icon = icon("layer-group"),
                  tagList( br(),
                  fluidRow( bs4Dash::box(title = "Model 1Y",status = "info",width = 6,
                  footer = "Model 1Y genereaza probabilitatile de default la un an. 
                  Au fost generate 2 modele, unul pentru beneficiarii raportati in CRC
                    si unul pentru restul, avand in vedere ca informatiile din CRC au o pondere semnificativa
                                              in estimarea riscului de default",
                  bs4Dash::accordion(id = ns("accordion_1Y"),
                                                bs4Dash::accordionItem(
                                                  title = "Training and test data dimensions",
                                                  status = "info",collapsed = TRUE,
                                                  DT::dataTableOutput(ns("sumar_dezvoltare_model_1Y"))),
                                                bs4Dash::accordionItem(
                                                  title = "AUC for the 2 models",
                                                  status = "info",collapsed = TRUE,
                                                  DT::dataTableOutput(ns("sumar_rezultate_model_1Y"))),
                                                bs4Dash::accordionItem(
                                                  title = "Model coefficients with CRC",
                                                  status = "info",collapsed = TRUE,
                                                  DT::dataTableOutput(ns("coeficienti_model_1Y_with_CRC"))),
                                                bs4Dash::accordionItem(
                                                  title = "Model coefficients NO CRC",
                                                  status = "info",collapsed = TRUE,
                                                  DT::dataTableOutput(ns("coeficienti_model_1Y_no_crc"))),
                                                bs4Dash::accordionItem(
                                                  title = "Mean encode 1Y",
                                                  status = "info",collapsed = TRUE,
                                                  DT::dataTableOutput(ns("mean_encode_1Y"))))
                  ),
                  bs4Dash::box(title = "Model 3Y",status = "info",width = 6,
                               footer = "Model 3Y genereaza probabilitatile de default la 3 ani. 
                  Au fost generate 2 modele, unul pentru beneficiarii raportati in CRC
                    si unul pentru restul, avand in vedere ca informatiile din CRC au o pondere semnificativa
                                              in estimarea riscului de default",
                               bs4Dash::accordion(id = ns("accordion_3Y"),
                                                  bs4Dash::accordionItem(
                                                    title = "Training and test data dimensions",
                                                    status = "info",collapsed = TRUE,
                                                    DT::dataTableOutput(ns("sumar_dezvoltare_model_3Y"))),
                                                  bs4Dash::accordionItem(
                                                    title = "AUC for the 2 models",
                                                    status = "info",collapsed = TRUE,
                                                    DT::dataTableOutput(ns("sumar_rezultate_model_3Y"))),
                                                  bs4Dash::accordionItem(
                                                    title = "Model coefficients with CRC",
                                                    status = "info",collapsed = TRUE,
                                                    DT::dataTableOutput(ns("coeficienti_model_3Y_with_CRC"))),
                                                  bs4Dash::accordionItem(
                                                    title = "Model coefficients NO CRC",
                                                    status = "info",collapsed = TRUE,
                                                    DT::dataTableOutput(ns("coeficienti_model_3Y_no_crc"))),
                                                  bs4Dash::accordionItem(title = "Mean encode 3Y",
                                                    status = "info",collapsed = TRUE,
                                                    DT::dataTableOutput(ns("mean_encode_3Y")))
                               )
                  )
                  
                  )
                  )
                  )
                   
  )
}
    
#' ifrs_portofoliu Server Function
#'
#' @noRd 
mod_ifrs_portofoliu_server <- function(input, output, session, vals_portofoliu, parrent_session){
  library(glmnet)
  
  ns <- session$ns
  
  updateTabsetPanel(session = session, inputId = 'ifrs_panel',selected = "database_ifrs")
  
  output$info <- renderText({ req(input$show_info == TRUE) 
    "Apasand butonul calculeaza de mai sus se vor genera provizioanele IFRS9 pentru portofoliul de garantii la data selectata.
    In cazul in care nu exista date CRC sau CIP la data selectata, provizioanele nu vor putea fi calculate."
    })
  
  output$show_button <- renderUI(shinyWidgets::actionBttn(inputId = ns("calculate"),style = "stretch",color = "success",
                              label = "Calculeaza provizioanele IFRS9",icon = icon("calculator")))
  
  modelling_results <- readRDS(file = "R/reactivedata/ifrs/modelling_results.rds" )
  
  df_fwd_coef <- readr::read_csv("R/reactivedata/ifrs/fwd_coef.csv")
  
  
  observeEvent(vals_portofoliu$view1_portofoliu,{ req(vals_portofoliu$view1_portofoliu)
  updateSelectInput(inputId = 'data_raport', session = session,
                  choices = vals_portofoliu$view1_portofoliu$anul_de_raportare %>% unique() )
    })
  
  observeEvent(input$calculate,{
    view_baza_date_crc <- readRDS(file = "R/reactivedata/crc/view_baza_date_crc.rds")
    view_cip_database <- readRDS(file = "R/reactivedata/cip/view_cip_database.rds")
    
    if(!as.Date(input$data_raport) %in%  view_baza_date_crc$`Data situatie risc global`) {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                text = "Nu detin datele CRC necesare. Please Upload CRC file!")
     bs4Dash::updateTabItems(session = parrent_session,inputId = "sidebar_ui_1-tabs",selected = 'crc')
      
    }
    else if (!as.Date(input$data_raport) %in%  view_cip_database$data_raport) {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                   text = "Nu detin datele CIP necesare. Please Upload CIP file!")
      bs4Dash::updateTabItems(session = parrent_session,
                  inputId = "sidebar_ui_1-tabs",selected = 'crc') 
      
    }
    
    else {
      
      portofoliu <<- reactive ({
        baza_cui <- readRDS("R/reactivedata/bi_cui.rds")
        crc <- readRDS("R/reactivedata/crc/baza_date_crc.rds") %>% 
          dplyr::filter(`Data situatie risc global` == as.Date(input$data_raport)) %>%
          dplyr::mutate(`Cod debitor` = as.character(`Cod debitor`)) %>%
          dplyr::left_join(baza_cui, by = c("Cod debitor" = "Unique ID")) %>%
          dplyr::group_by(Code) %>% dplyr::summarise(scor_serv_datorie = mean(scor_serv_datorie),
                    are_restante_peste_30_zile = mean(are_restante_peste_30_zile))
        
        
        cip <- readRDS("R/reactivedata/cip/baza_date_cip.rds") %>% 
          dplyr::filter(data_raport == as.Date(input$data_raport)) %>%
          dplyr::mutate(CUI = as.character(CUI)) %>% 
          dplyr::select(-bilete_majore) %>%
          dplyr::left_join(y = baza_cui, by = c("CUI" = "Unique ID"))
        rm("baza_cui")
        
        portofoliu <- readRDS("R/reactivedata/portofoliu/portof_database.rds") %>%
          dplyr::filter(anul_de_raportare == as.Date(input$data_raport)) %>%
          dplyr::group_by(`Cod Partener`,Beneficiar, categorie_contaminata) %>%
          dplyr::summarise(expunere_mil_lei = sum(expunere)/1000000, contragarantii = sum(contragarantii)) %>% dplyr::ungroup() %>%
          dplyr::left_join(y = crc %>% dplyr::select(Code,scor_serv_datorie, are_restante_peste_30_zile),
                           by = c("Cod Partener" = "Code")) %>% 
          dplyr::left_join(y = cip %>% dplyr::select(-data_raport, -CUI), by = c("Cod Partener" = "Code")) %>% 
          dplyr::mutate(dplyr::across(.cols = c(total_incidente,Este_in_interdictie,
                          total_incidente_majore, A_fost_interdictie),    ~ifelse(is.na(.x),0,.x))) %>% 
          dplyr::mutate(scor_serv_prel = ifelse(are_restante_peste_30_zile==0,0.5,scor_serv_datorie))
        
        rm("crc", "cip")
        
        
        portofoliu <- portofoliu %>% dplyr::mutate( mean_encode_categ_contaminata_1Y = ifelse(
          categorie_contaminata == "standard",  0.923187279574391,
          ifelse(   categorie_contaminata == "insolventa",  0.0289517205650865,
                    ifelse(   categorie_contaminata == "instiintare_neplata", 0.0478609998605216,0  )  ) ))
        
        model_1Y_with_CRC <- readRDS("R/reactivedata/ifrs/model_1Y_with_CRC")
        coefficients_1Y_CRC <- predict(object = model_1Y_with_CRC, type = "coefficients",s = model_1Y_with_CRC$lambda[28])
        
        portofoliu <- portofoliu %>% dplyr::mutate(prob_1Y_CRC = predict(
          object = model_1Y_with_CRC,
          s = model_1Y_with_CRC$lambda.min,
          type = "response",
          newx = portofoliu %>% dplyr::select(coefficients_1Y_CRC@Dimnames[[1]][-1]) %>% 
            as.matrix())[, 1] )
        
        model_1Y_no_crc <- readRDS("R/reactivedata/ifrs/model_1Y_no_crc")
        
        
        coefficients_1Y_no_crc <- predict(object = model_1Y_no_crc, type = "coefficients", s = model_1Y_no_crc$lambda[18])
        
        
        portofoliu <- portofoliu %>% dplyr::mutate( prob_1Y_no_crc = predict(
          object = model_1Y_no_crc,
          s = model_1Y_no_crc$lambda.min,
          type = "response",
          newx = portofoliu %>% dplyr::select(coefficients_1Y_no_crc@Dimnames[[1]][-1]) %>% as.matrix())[, 1] )
        
        portofoliu <- portofoliu %>% dplyr::mutate(prob_1Y = ifelse(is.na(scor_serv_prel),prob_1Y_no_crc,prob_1Y_CRC))
        
        portofoliu <- portofoliu %>% dplyr::mutate(
          stage = dplyr::case_when(
            categorie_contaminata %in% c("insolventa", "cerere_plata") ~ 3,
            is.na(are_restante_peste_30_zile) &
              prob_1Y < 0.097 ~ 1,
            is.na(are_restante_peste_30_zile) &
              prob_1Y >= 0.097 ~ 2,
            are_restante_peste_30_zile == 0 &
              prob_1Y < 0.04 ~ 1,
            are_restante_peste_30_zile == 0 &
              prob_1Y >= 0.04 ~ 2,
            are_restante_peste_30_zile == 1 ~ 2  ) )
        
        return(portofoliu)
        
      })
      
      output$sinteza_ifrs <- DT::renderDataTable( { req(portofoliu())
        dt_generate_function( df = portofoliu() %>% 
                        dplyr::group_by(categorie_contaminata,stage) %>% 
                        dplyr::summarise(expunere = sum(expunere_mil_lei) * 1000000) %>%
                        dplyr::arrange(desc(categorie_contaminata), stage) %>% 
                        dplyr::mutate(stage = as.character(stage)) %>%
                        janitor::adorn_totals(where = "row",fill = "-",name = "Total"),
                      caption = "Sinteza IFRS", show_buttons=TRUE,round_col = 3 )  } )
      
      
      output$show_button <- renderUI(  shinyWidgets::actionBttn(inputId = ns("save_ifrs"),
                  label = "Salveaza IFRS9",icon = icon("save"),style = "stretch",color = "success",size = "sm"))
    }
    
  } )
  
  observeEvent(input$save_ifrs,{ req(input$save_ifrs==1)
    ifrs_database <- readRDS("R/reactivedata/ifrs/portofoliu_ifrs.rds")
    
    ifrs_reactive <- reactiveValues( df_old = ifrs_database,
                                     df_new = portofoliu() %>% dplyr::mutate(data_raport = as.Date(input$data_raport) ),
                                     element_id = as.Date(input$data_raport),
                                     column_id = "data_raport",
                                     finalise_process_compare_df = FALSE)
    
    callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = ifrs_reactive)
    
   observe({ req(ifrs_reactive$finalise_process_compare_df == TRUE)
     saveRDS(object = ifrs_reactive$df_new_prel,
             file = "R/reactivedata/ifrs/portofoliu_ifrs.rds")
     
     shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                              .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
     
     })
   
   
    
  })
  
 
 
    
    
 
      
   
  
  output$sumar_dezvoltare_model_1Y <- DT::renderDataTable({ dt_generate_function(round_col = 2,
            df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator,pattern = "nr_observatii")) %>% 
              dplyr::filter(stringr::str_detect(indicator,pattern = "1Y")))})
 
  output$sumar_rezultate_model_1Y <- DT::renderDataTable({
    
      dt_generate_function(digits = 2,  round_col = 2,
        df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator, pattern = "auc")) %>% 
          dplyr::filter(stringr::str_detect(indicator, pattern = "1Y"))
      )
    })
  
  output$coeficienti_model_1Y_with_CRC <- DT::renderDataTable({
    model_1Y_with_CRC <- readRDS(file = "R/reactivedata/ifrs/model_1Y_with_CRC")
      
      dt_generate_function(round_col = 2,  digits = 6,
        df = predict(object = model_1Y_with_CRC, type = "coefficients",
            s = model_1Y_with_CRC$lambda.min) %>% broom::tidy() %>% dplyr::select(-2)
      )
    })
  
  output$coeficienti_model_1Y_no_crc <- DT::renderDataTable({
    model_1Y_no_crc <- readRDS(file = "R/reactivedata/ifrs/model_1Y_no_crc")
    
      dt_generate_function(round_col = 2,  digits = 6,
        df = predict(object = model_1Y_no_crc, type = "coefficients",
            s = model_1Y_no_crc$lambda.min) %>% broom::tidy() %>% dplyr::select(-2) )    })
  
  output$mean_encode_1Y <- DT::renderDataTable({
    categ_contam_1Y <- readRDS(file = "R/reactivedata/ifrs/categ_contaminata_1Y")
    
      dt_generate_function(round_col = 1:3,   digits = 6,
        caption = "Valorile aferente variabilei mean_encode_categ_contaminata_1Y in functie de categoria 
              contaminata a beneficiarului",
        df = categ_contam_1Y$crossFrame %>% dplyr::select(1, 3:5) %>% dplyr::mutate(
          insolventa = categorie_contaminata_lev_x_insolventa * categorie_contaminata_catP,
          instiintare_neplata = categorie_contaminata_lev_x_instiintare_neplata * categorie_contaminata_catP,
          standard = categorie_contaminata_lev_x_standard * categorie_contaminata_catP) %>%
          dplyr::select(insolventa, instiintare_neplata, standard) %>% 
            dplyr::summarise_all( ~max(.))   )   })
  
  output$sumar_dezvoltare_model_3Y <- DT::renderDataTable({
      dt_generate_function(round_col = 2,
        df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator, pattern = "nr_observatii")) %>% 
          dplyr::filter(stringr::str_detect(indicator, pattern = "3Y"))
      )
    })
  
  output$sumar_rezultate_model_3Y <- DT::renderDataTable({
      dt_generate_function(digits = 2, round_col = 2,
        df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator, pattern = "auc")) %>% 
            dplyr::filter(stringr::str_detect(indicator, pattern = "3Y"))
      )
    })
  
  output$coeficienti_model_3Y_with_CRC <- DT::renderDataTable({
    model_3Y_with_CRC <- readRDS(file = "R/reactivedata/ifrs/model_3Y_with_CRC")
    
      dt_generate_function(round_col = 2,  digits = 6,
        df = predict(object = model_3Y_with_CRC, type = "coefficients",
            s = model_3Y_with_CRC$lambda.min) %>% broom::tidy() %>% dplyr::select(-2) )   })
  
  output$coeficienti_model_3Y_no_crc <- DT::renderDataTable({
    model_3Y_no_crc <- readRDS(file = "R/reactivedata/ifrs/model_3Y_no_crc")
    
      dt_generate_function(round_col = 2,  digits = 6,
        df = predict(object = model_3Y_no_crc,type = "coefficients",
            s = model_3Y_no_crc$lambda.min) %>% broom::tidy() %>% dplyr::select(-2)    )  })
  
  output$mean_encode_3Y <- DT::renderDataTable({
    categ_contam_3Y <- readRDS(file = "R/reactivedata/ifrs/categorie_contaminata_3Y")
    
      dt_generate_function(round_col = 1:3,  digits = 6,
        caption = "Valorile aferente variabilei mean_encode_categ_contaminata_3Y in functie de 
        categoria contaminata a beneficiarului",
        df = categ_contam_3Y$crossFrame %>% dplyr::select(1, 3:5) %>% dplyr::mutate(
          insolventa = categorie_contaminata_lev_x_insolventa * categorie_contaminata_catP,
          instiintare_neplata = categorie_contaminata_lev_x_instiintare_neplata * categorie_contaminata_catP,
          standard = categorie_contaminata_lev_x_standard * categorie_contaminata_catP) %>%
            dplyr::select(insolventa, instiintare_neplata, standard) %>% dplyr::summarise_all(~  max(.))  )  })
  
  output$graph1 <- renderPlot({
    
    
    ggplot2::ggplot(data = tidyr::pivot_longer(data = 
                df_fwd_coef[-c(1,nrow(df_fwd_coef)),],cols = 2:3),
        mapping = ggplot2::aes(x = lubridate::year(anul_de_raportare), y=value, colour=name))+ ggplot2::geom_line(size=1.2)+
      ggplot2::scale_color_manual(values = c("#f3d112","#00a65a"))+
      ggplot2::scale_x_continuous(breaks = seq(2010,2018,by=1)) +  
      ggplot2::scale_y_continuous(labels = scales::percent,limits = c(0.02,NA))+
      ggplot2::ylab(label = "") + ggplot2::xlab(label = "Final de an") + 
      ggplot2::labs(title = "Evolutia ratei efective de default la un an si a probabilitatii medii de default 
                     la un an prognozate de model") + 
      ggplot2::geom_text(mapping = ggplot2::aes(y = value,label=paste0(round(value*100,1),"%"))) + 
      ggplot2::theme_set(ggplot2::theme_grey(base_size = 16))+
      ggplot2::theme(panel.grid = ggplot2::element_blank(),panel.background = ggplot2::element_rect(fill = "white"),
            legend.position = c(0.1,0.2),legend.title = ggplot2::element_blank()) })
  
  
  output$graph2 <- renderPlot({ 
    
    ggplot2::ggplot(data = tidyr::pivot_longer(data = df_fwd_coef[-1,],cols = 5:6),
            mapping = ggplot2::aes(x = lubridate::year(anul_de_raportare),y=value,colour=name))+ 
      ggplot2::geom_line(size=1.2)+
      ggplot2::scale_color_manual(values = c("#f3d112","#00a65a"))+
      ggplot2::scale_x_continuous(breaks = seq(2010,2019,by=1)) +  
      ggplot2::ylab(label = "") + ggplot2::xlab(label = "Final de an") + 
      ggplot2::labs(#caption = "In tab-ul urmator sunt prezentate detaliile ecuatiei de regresie care sta la baza estimarii coeficientului fwd looking (predict_coef)",
        title = "Evolutia coeficientului fwd looking precum si a valorii prognozate de ecuatia de regresie") + 
      ggplot2::geom_text(mapping = ggplot2::aes(y = value,label=round(value,2)),check_overlap = TRUE) + 
      ggplot2::theme_set(ggplot2::theme_grey(base_size = 16))+
      ggplot2::theme(panel.grid = ggplot2::element_blank(),panel.background = ggplot2::element_rect(fill = "white"),
            legend.position = c(0.1,0.8),legend.title = ggplot2::element_blank()) })  
  
  
  
  
}
    
## To be copied in the UI
# mod_ifrs_portofoliu_ui("ifrs_portofoliu_ui_1")
    
## To be copied in the server
# callModule(mod_ifrs_portofoliu_server, "ifrs_portofoliu_ui_1")
 

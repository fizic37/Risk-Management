#' database_portofoliu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_portofoliu_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
    
    shinyFeedback::useShinyFeedback(),
    
    bs4Dash::box(title = "Upload utility files - BI, insolventa", id = ns("box_utility"),
      icon = icon("toolbox"), status = "info",  width = 12,  collapsible = TRUE,  
      collapsed = TRUE,maximizable = T, mod_database_util_files_ui("database_util_files_ui_1"), 
      footer = "Aici se uploadeaza date actualizate referitoare la cererile de plata si instiintarile de neplata primite 
      precum si insolventele beneficiarilor."),
    
    bs4Dash::box(title = "Upload portfolio file",  id = ns("box_upload_portofoliu"),
                 icon = icon("file-excel"),  width = 12,
                 collapsible = T,collapsed = TRUE, maximizable = T, status = "info",
                 footer =  "Se uploadeaza intregul fisier de solduri disponibil pe fileserver - Provizioane.",
                 mod_database_portofoliu_upload_ui("database_portofoliu_upload_ui_1") ),
      
    bs4Dash::box( title = "Database portfolio", icon = icon("database"), id = ns("box_plati_database"),
                  maximizable = T,
                        status = "info",width = 12,collapsible = TRUE,collapsed = FALSE,
                       
                       DT::dataTableOutput(ns("sinteza_portofoliu")),
                       tags$script(src = "portofoliu_buttons.js"),
                       tags$script(paste0("portofoliu_module_js('", ns(''), "')")),
                       actionButton(inputId = ns("change_view"),label = "Change Report View",
                                    icon = icon("toggle-on"),width = "250px"),
                       
                       uiOutput(ns("database_portof_output")) ),
                       
    bs4Dash::box(title = "Regularizare provizioane garantii depreciate",status = "info", collapsible = TRUE,collapsed = TRUE,
                 maximizable = T, width = 12, icon = icon("check-double"),id = ns("box_regulariz_portofoliu"),
    mod_regularizare_portofoliu_ui("regularizare_portofoliu_1") ),
      
   bs4Dash::box(title = "Indicatori provizioane non IFRS",id = ns("box_coeficienti_portofoliu"),
                        status = "info", collapsible = TRUE,collapsed = TRUE, maximizable = T,
                width = 12, icon = icon("subscript"),   mod_coeficienti_portofoliu_ui("coeficienti_portofoliu_ui_1") )
  )
  
  
}
    
#' database_portofoliu Server Function
#'
#' @noRd 
mod_database_portofoliu_server <- function(input, output, session,vals,vals_portofoliu) {
  ns <- session$ns
  
  portofoliu_database <- readRDS("R/external_volumes/portofoliu/portof_database.rds")
  
  observeEvent(input$box_utility, {req(any(input$box_utility$collapsed==FALSE,
                                                input$box_utility$maximized==TRUE))
    
    vals$box_selected <- c(vals$box_selected,"box_utility")
  })
  
  observeEvent(input$box_upload_portofoliu, {req(any(input$box_upload_portofoliu$collapsed==FALSE,
                                           input$box_upload_portofoliu$maximized==TRUE))
    
    vals$box_selected <- c(vals$box_selected,"box_upload_portofoliu")
  })
  
  
  observeEvent(input$box_coeficienti_portofoliu, {req(any(input$box_coeficienti_portofoliu$collapsed==FALSE,
                                                     input$box_coeficienti_portofoliu$maximized==TRUE))
    
    vals$box_selected <- c(vals$box_selected,"box_coeficienti_portofoliu")
  })
  
  observeEvent(input$box_regulariz_portofoliu, {  vals$box_selected <- c(vals$box_selected,"box_regulariz_portofoliu")  })
  
  # Observer for change view button state
  
  observe({
    if (input$change_view==0 || input$change_view %% 4 == 0){
      vals_portofoliu$view_portofoliu <- vals_portofoliu$view1_portofoliu
      vals_portofoliu$caption_table <- "Sinteza portofoliu:"
      vals_portofoliu$round_cols <- c(3:6)
      vals_portofoliu$perc_cols <- NULL}
    else if (input$change_view %% 2 == 0) {
      vals_portofoliu$view_portofoliu <- vals_portofoliu$view3_portofoliu
      vals_portofoliu$caption_table <- "Garantii depreciate - distributie procentuala:"
      vals_portofoliu$round_cols <- 0
      vals_portofoliu$perc_cols <- c(3:7)  }
    else if (input$change_view %% 3 == 0) {
      vals_portofoliu$view_portofoliu <- vals_portofoliu$view4_portofoliu
      vals_portofoliu$caption_table <- "Provizioane garantii depreciate:"
      vals_portofoliu$round_cols <- c(3:7)
      vals_portofoliu$perc_cols <- NULL  }
    else {
      vals_portofoliu$view_portofoliu <- vals_portofoliu$view2_portofoliu
      vals_portofoliu$caption_table <- "Garantii depreciate - distributie valorica:"
      vals_portofoliu$round_cols <- c(3:7)
      vals_portofoliu$perc_cols <- NULL  }
  })
  
  # Key observer. Everytime it updates I update actions and render the main table.
  # Note I do not save it, as it will be saved from the beginning. I only save it inside vals_cip$baza_date_cip observer
  
  observeEvent(vals_portofoliu$view_portofoliu,{
    vals_portofoliu$unique_dates <-  vals_portofoliu$view_portofoliu$anul_de_raportare
    
    vals_portofoliu$actions <- purrr::map_chr(vals_portofoliu$unique_dates, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-sm download_btn" data-toggle="tooltip" data-placement="top" title="Download" id = ', 
          id_, ' style="margin: 0; color: #20c997;"><i class="fa fa-download"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_,
          ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )  })
    
    output$sinteza_portofoliu <- DT::renderDataTable({
        dt_generate_function(show_buttons = TRUE,pageLength = 4,
          df = cbind(  tibble::tibble(" " = vals_portofoliu$actions), vals_portofoliu$view_portofoliu),
          escape = FALSE,
          caption = vals_portofoliu$caption_table,
          round_col = vals_portofoliu$round_cols,
          perc_col = vals_portofoliu$perc_cols
        )
    })
    
   })
  
  
  # Key observer. Every time i update vals_portofoliu$portof_database, I save the new database, recalculate and save views
  # and also save the new portof_database
  
  observeEvent(vals_portofoliu$portof_database,{req(!is.na(vals_portofoliu$portof_database))
   
    vals_portofoliu$view1_portofoliu <- vals_portofoliu$portof_database %>% dplyr::group_by(anul_de_raportare) %>%
      dplyr:: summarise(nr_contracte = dplyr::n(),garantii_sold = sum(expunere),contragarantii=sum(contragarantii),
                        provizion_contabil=sum(provizion_contabil)) %>% dplyr::arrange(desc(anul_de_raportare))
    
    view1_portofoliu <- isolate(vals_portofoliu$view1_portofoliu)
    
    saveRDS(object = view1_portofoliu,file = "R/reactivedata/portofoliu/view1_portofoliu.rds")
    
    vals_portofoliu$view2_portofoliu <- vals_portofoliu$portof_database %>% dplyr::group_by(anul_de_raportare,categorie_contaminata) %>% 
      dplyr::summarise(expunere=sum(expunere)) %>%
      tidyr::pivot_wider(names_from = categorie_contaminata,values_from = expunere) %>% dplyr::ungroup()  %>%
      dplyr::mutate(Total_sold=rowSums(x = dplyr::select(.,-1),na.rm = TRUE)) %>% dplyr::arrange(desc(anul_de_raportare))
    
    view2_portofoliu <- isolate(vals_portofoliu$view2_portofoliu)
    
    saveRDS(object = view2_portofoliu,file = "R/reactivedata/portofoliu/view2_portofoliu.rds")
    
    vals_portofoliu$view3_portofoliu <- cbind(view2_portofoliu[,1], view2_portofoliu[,2:6]/view2_portofoliu$Total_sold)
    
    view3_portofoliu <- isolate(vals_portofoliu$view3_portofoliu)
    
    saveRDS(object = view3_portofoliu,file = "R/reactivedata/portofoliu/view3_portofoliu.rds")
    
    vals_portofoliu$view4_portofoliu <- vals_portofoliu$portof_database %>% dplyr::group_by(anul_de_raportare,categorie_contaminata) %>% 
      dplyr::summarise(provizion_contabil=sum(provizion_contabil)) %>% 
        tidyr::pivot_wider(names_from = categorie_contaminata,values_from = provizion_contabil) %>% 
          dplyr::ungroup()  %>%   dplyr::mutate(Total_Provizion_Contabil=rowSums(x = dplyr::select(.,-1),na.rm = TRUE)) %>%
              dplyr::arrange(desc(anul_de_raportare))
    
    view4_portofoliu <- isolate(vals_portofoliu$view4_portofoliu)
    
    saveRDS(object = view4_portofoliu,file = "R/reactivedata/portofoliu/view4_portofoliu.rds")
    
    portof_database <- isolate(vals_portofoliu$portof_database)
    
    saveRDS(object = portof_database,file = "R/external_volumes/portofoliu/portof_database.rds")
    
    
    
    shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                             .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    
  })
  
  
  observeEvent(input$data_raport_to_download,{ req(vals$user_type != "guest")
    
    showModal(modalDialog(title = h3("ATENTIE!",style = "color: #ffa500;"), size = "l",
                          h3(paste0("Esti sigur ca vrei sa downloadez portofoliul la data de ",
                                    input$data_raport_to_download, " ?"), style = "color: #77547a"),  footer = 
                            tagList(shinyWidgets::downloadBttn(outputId = session$ns("confirm_download"),label = "Download",
                                                               color = "success", size = "md"),
                                    shinyWidgets::actionBttn(inputId = session$ns("cancel_download"),label = "Cancel",
                                                             icon = icon("window-close"),color = "danger",size = "md")
                            )))
    
    output$confirm_download <- downloadHandler(filename = function() {
          paste0("portof_database_",  input$data_raport_to_download, ".csv")   },
        content = function(file) {
            readr::write_csv(x = portofoliu_database %>%
                          dplyr::filter(anul_de_raportare ==  as.Date.character(input$data_raport_to_download)),
                      path = file)
          removeModal(session = session)  }   )
    
    
  })
  
  # Remove modal on cancel download button
  observeEvent(input$cancel_download,{ 
    removeModal(session = session)  })
  
  observeEvent(input$data_raport_to_delete,{ req(vals$user_type != "guest")
    
    showModal(modalDialog(title = h3("ATENTIE!",style = "color: #ffa500;"), size = "l",
                          h3(paste0("Esti sigur ca vrei sa stergi portofoliul la data de ",
                                    input$data_raport_to_delete, " ?"), style = "color: #77547a"),  footer = 
                            tagList(shinyWidgets::actionBttn(inputId = session$ns("confirm_delete"),label = "Confirm",
                                                             icon = icon("check"),color = "success",size = "md"),
                                    shinyWidgets::actionBttn(inputId = session$ns("cancel_delete"),label = "Cancel",
                                                             icon = icon("window-close"),color = "danger",size = "md")
                            )))
  }) 
  
  # Remove modal on cancel delete button
  observeEvent(input$cancel_delete,{
    removeModal(session = session)  })
  
  
  observeEvent(input$confirm_delete,{
    
    vals_portofoliu$portof_database <- portofoliu_database %>% 
      dplyr::filter(anul_de_raportare != as.Date.character(input$data_raport_to_delete))
    removeModal(session = session)
    })
  
  
}
  
  # This observer does not do anything. It will only activate when I will finish provision_migration
  #observe({
    #if (input$change_view_migration ==0 || input$change_view_migration %% 2 ==0) {
    #  output$portfolio_migration <- DT::renderDataTable({dt_generate_function(df=vals_portofoliu$tabel_migration_final,
     #                     round_col = 2:6,show_buttons = TRUE,pageLength = 7, dom = "t",
     #           caption = "Migratia portofoliului de garantii in functie de categoria contaminata a beneficiarului:")  })
     # updateActionButton(session = session, inputId = "change_view_migration",label = "Switch to provisions migration")  }
    #else {
     # output$portfolio_migration <- DT::renderDataTable({dt_generate_function(df= vals_portofoliu$regularizare_provizioane_non_ifrs,
      #            show_buttons = TRUE,pageLength = 7, dom = "t",
       #           caption = "Migratia portofoliului de garantii in functie de categoria contaminata a beneficiarului:")  })
      
      #updateActionButton(session = session, inputId = "change_view_migration",label = "Switch to exposures migration")}})
  
  
  
 
  
  

    
## To be copied in the UI
# mod_database_portofoliu_ui("database_portofoliu_ui_1")
    
## To be copied in the server
# callModule(mod_database_portofoliu_server, "database_portofoliu_ui_1")
 

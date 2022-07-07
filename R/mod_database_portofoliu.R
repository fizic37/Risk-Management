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
                 collapsible = T,collapsed = FALSE, maximizable = T, status = "info",
                 footer =  "Se uploadeaza intregul fisier de solduri disponibil pe fileserver - Provizioane.",
                 mod_database_portofoliu_upload_ui("database_portofoliu_upload_ui_1") ),
      
    bs4Dash::box( title = "Database portfolio", icon = icon("database"), id = ns("box_plati_database"),
                  maximizable = T,
                        status = "info",width = 12,collapsible = TRUE,collapsed = TRUE,
                       
                       DT::dataTableOutput(ns("sinteza_portofoliu")),
                       tags$script(src = "portofoliu_buttons.js"),
                       tags$script(paste0("portofoliu_module_js('", ns(''), "')")),
                       actionButton(inputId = ns("change_view"),label = "Change Report View",
                                    icon = icon("toggle-on"),width = "250px"),
                       
                       uiOutput(ns("database_portof_output")),
                       hr(),
                       fluidRow(
                         column(width = 4,
                         selectInput(inputId = ns("migration_from_nonifrs"), label = "Migration from:",width = "300px",
                                                     choices = as.Date("2018-12-31"),
                                                        selected = as.Date("2018-12-31"))),
                         column(width = 4, 
                         selectInput(inputId = ns("migration_to_nonifrs"),    label = "Migration to:", 
                                           choices = as.Date("2019-12-31"), width="300px",
                                           selected = as.Date("2019-12-31"))),
                         column(width = 4, br(), actionButton(inputId = ns("start_migration"),width = "250px",
                                          label = "Perform portfolio migration",icon = icon("play"))) ),
                       DT::dataTableOutput(ns("portfolio_migration")), br(),
                       uiOutput(outputId = ns("show_down_button"))),
    
   bs4Dash::box(title = "Indicatori provizioane non IFRS",id = ns("box_coeficienti_portofoliu"),
                        status = "info", collapsible = TRUE,collapsed = TRUE, maximizable = T,
                width = 12, icon = icon("check-double"),   mod_coeficienti_portofoliu_ui("coeficienti_portofoliu_ui_1") )
  )
  
  
}
    
#' database_portofoliu Server Function
#'
#' @noRd 
mod_database_portofoliu_server <- function(input, output, session,vals,vals_portofoliu){
  ns <- session$ns
  
  portofoliu_database <- readRDS("R/reactivedata/portofoliu/portof_database.rds")
  
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
        dt_generate_function(show_buttons = TRUE,pageLength = 5,
          df = cbind(  tibble::tibble(" " = vals_portofoliu$actions), vals_portofoliu$view_portofoliu),
          escape = FALSE,
          caption = vals_portofoliu$caption_table,
          round_col = vals_portofoliu$round_cols,
          perc_col = vals_portofoliu$perc_cols
        )
    })
    
    updateSelectInput(session = session,inputId ='migration_from_nonifrs',choices = vals_portofoliu$unique_dates ,
                      selected = vals_portofoliu$unique_dates [2])
    
    updateSelectInput(session = session,inputId ='migration_to_nonifrs',choices = vals_portofoliu$unique_dates ,
                      selected = vals_portofoliu$unique_dates [1])
    
    
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
    
    saveRDS(object = portof_database,file = "R/reactivedata/portofoliu/portof_database.rds")
    
    
    
    shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                             .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    
  })
  
  
  observeEvent(input$data_raport_to_download,{
    
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
  
  observeEvent(input$data_raport_to_delete,{
    
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
  
  
  
  #### MIGRATION
  
     
  observeEvent( input$start_migration,{
    vals_portofoliu$lista_provizion_non_ifrs <- portofoliu_database %>% 
      dplyr::group_by(`Cod Partener`,anul_de_raportare,categorie_contaminata) %>% 
      dplyr::summarise(expunere = sum(expunere),provizion_contabil=sum(provizion_contabil)) %>% dplyr::ungroup() %>%
      dplyr::group_split(anul_de_raportare,.keep=TRUE) %>% purrr::map(~rename_col_nonifrs(.x))
    
    vals_portofoliu$portofoliu_perioada_curenta <- vals_portofoliu$lista_provizion_non_ifrs %>% 
      purrr::keep(.p = ~unique(.x$anul_de_raportare) == input$migration_to_nonifrs) %>% purrr::flatten_df()
      
      
    vals_portofoliu$portofoliu_perioada_anterioara <- vals_portofoliu$lista_provizion_non_ifrs %>% 
        purrr::keep(.p = ~unique(.x$anul_de_raportare) == input$migration_from_nonifrs) %>% purrr::flatten_df() %>% 
            dplyr::left_join(y= dplyr::select(vals_portofoliu$portofoliu_perioada_curenta,
                  dplyr::matches("Expunere|Cod Partener|categorie_contaminata|provizion_contabil")), by = "Cod Partener")
    
    # I need to define below column names because they are reactive due to rename_col function applied to the list of dataframes
    # I will need to process with dplyr these column names
    vals_portofoliu$categorie_anterioara <- paste0("categorie_contaminata_",input$migration_from_nonifrs)
    vals_portofoliu$categorie_curenta <- paste0("categorie_contaminata_",input$migration_to_nonifrs)
    vals_portofoliu$expunere_anterioara <- paste0("Expunere_",input$migration_from_nonifrs)
    vals_portofoliu$expunere_curenta <- paste0("Expunere_",input$migration_to_nonifrs)
    vals_portofoliu$provizion_anterior <- paste0("Provizion_contabil_", input$migration_from_nonifrs)
    vals_portofoliu$provizion_curent <- paste0("Provizion_contabil_",input$migration_to_nonifrs)
    
    # I calculate my main indicators here of exposure migration
    vals_portofoliu$tabel_brut_migration_expunere <- vals_portofoliu$portofoliu_perioada_anterioara %>% 
        dplyr::group_by(!!rlang::sym(vals_portofoliu$categorie_anterioara),    !!rlang::sym(vals_portofoliu$categorie_curenta)) %>% 
        dplyr::summarise("vals_portofoliu$expunere_anterioara" = sum(!!rlang::sym(vals_portofoliu$expunere_anterioara)),
                       'vals_portofoliu$expunere_curenta'=sum(!!rlang::sym(vals_portofoliu$expunere_curenta))) %>% dplyr::ungroup() %>%
      dplyr::rename_at(.vars = 3:4,.funs = ~c(vals_portofoliu$expunere_anterioara,vals_portofoliu$expunere_curenta)) %>%
      dplyr::mutate(Derecunoscute_rambursate = ifelse(is.na(.[[2]]),-.[[3]],.[[4]] - .[[3]]),
                    Transferuri_cerere_plata = ifelse(.[[2]] == 'cerere_plata' & .[[1]] != 'cerere_plata',   .[[4]],  0),
                    Transferuri_from_cerere_plata = ifelse(.[[2]] == 'cerere_plata' & .[[1]] != 'cerere_plata', -.[[4]],  0),
                    Transferuri_insolventa = ifelse(.[[2]] == 'insolventa' & .[[1]] != 'insolventa',  .[[4]],    0),
                    Transferuri_from_insolventa = ifelse(.[[2]] == 'insolventa' & .[[1]] != 'insolventa', -.[[4]],  0),
                    Transferuri_instiintare = ifelse(.[[2]] == 'instiintare_neplata' & .[[1]] != 'instiintare_neplata', .[[4]],    0),
                    Transferuri_from_instiintare = ifelse(.[[2]] == 'instiintare_neplata' & .[[1]] != 'instiintare_neplata',  -.[[4]],    0 ),
                    Transferuri_standard = ifelse(.[[2]] == 'standard' & .[[1]] != 'standard', .[[4]],    0),
                    Transferuri_from_standard = ifelse(.[[2]] == 'standard' & .[[1]] != 'standard',  -.[[4]],    0 ))
    
    
    # I get my transfers to a category (from another one)
    vals_portofoliu$tabel1_migration <-  vals_portofoliu$tabel_brut_migration_expunere  %>% 
      dplyr::group_by(!!rlang::sym(vals_portofoliu$categorie_anterioara)) %>% 
        dplyr::summarise(Sold_initial = sum(!!rlang::sym(vals_portofoliu$expunere_anterioara)),
                         Derecunoscute_rambursate=sum(Derecunoscute_rambursate),
                       Transfers_cerere_plata = sum(Transferuri_from_cerere_plata,na.rm=T),   
                       Transfers_insolventa=sum(Transferuri_from_insolventa,na.rm=T),
                       Transfers_instiintare=sum(Transferuri_from_instiintare,na.rm=T),
                       Transfers_standard = sum(Transferuri_from_standard,na.rm = TRUE)) %>% tidyr::pivot_longer(cols = -1)   %>% 
      tidyr::pivot_wider(names_from =  !!rlang::sym(vals_portofoliu$categorie_anterioara),names_prefix = "categorie_contaminata_") 
    
    # I get my transfers from a category to another one (the same table as table1_migration but with minus exposures)
    vals_portofoliu$tabel2_migration <- vals_portofoliu$tabel_brut_migration_expunere  %>% dplyr::filter(!is.na(.[[2]])) %>% 
      dplyr::group_by(!!rlang::sym(vals_portofoliu$categorie_curenta)) %>% 
              dplyr::summarise(Transfers_cerere_plata = sum(Transferuri_cerere_plata,na.rm=T), 
                       Transfers_insolventa = sum(Transferuri_insolventa,na.rm = T),
                       Transfers_instiintare=sum(Transferuri_instiintare,na.rm=T),
                       Transfers_standard = sum(Transferuri_standard,na.rm = TRUE)) %>% 
                 tidyr::pivot_longer(cols = -1)   %>% tidyr::pivot_wider(names_from =  !!rlang::sym(vals_portofoliu$categorie_curenta),
                                                              names_prefix = "categorie_contaminata_")
    
    # I get category of my new exposures categories
    vals_portofoliu$tabel3_new_exposures_migration <- dplyr::left_join(x = vals_portofoliu$portofoliu_perioada_curenta,
              y = dplyr::select(vals_portofoliu$portofoliu_perioada_anterioara,`Cod Partener`,!!rlang::sym(vals_portofoliu$expunere_anterioara),
                      !!rlang::sym(vals_portofoliu$categorie_anterioara)),by="Cod Partener") %>% 
      dplyr::filter(is.na(!!rlang::sym(vals_portofoliu$categorie_anterioara))) %>% 
      dplyr::group_by(!!rlang::sym(vals_portofoliu$categorie_curenta)) %>% 
      dplyr::summarise(Acordate_efectuate_in_timpul_anului=sum(!!rlang::sym(vals_portofoliu$expunere_curenta))) %>% 
      tidyr::pivot_longer(cols = -1) %>% tidyr::pivot_wider(names_from =  !!rlang::sym(vals_portofoliu$categorie_curenta),
                                                            names_prefix = "categorie_contaminata_")
    
    # I produce my final migration_table
    vals_portofoliu$tabel_migration_final <- dplyr::bind_rows(vals_portofoliu$tabel1_migration,
                        vals_portofoliu$tabel2_migration,vals_portofoliu$tabel3_new_exposures_migration) %>% 
      dplyr::group_by(name) %>% 
      dplyr:: summarise_all(.funs = ~sum(.,na.rm=T)) %>% dplyr::slice(c(3,1,2,4:6)) %>% dplyr::mutate(Total=rowSums(.[2:5])) %>% 
      dplyr::bind_rows(apply(X = dplyr::select(.,-1),MARGIN = 2,FUN=sum)) %>% tidyr::replace_na(replace = list(name="Total")) %>% 
      dplyr::rename_at(.vars = 1,~'Miscari_in_timpul_Perioadei')
    
    output$portfolio_migration <- DT::renderDataTable({dt_generate_function(df=vals_portofoliu$tabel_migration_final,
                                                  round_col = 2:6,show_buttons = TRUE,
                                                 caption = "Migratia portofoliului de garantii in functie de categoria contaminata a beneficiarului:")  })
    
    
     ### Provisions Migration - not finished
       # This table produces provision migration for all CUI that are in perioada curenta (it does not matter if they are in perioada anterioara or not)
    vals_portofoliu$tabel_variatie_provizioane <- dplyr::left_join(x = dplyr::select(vals_portofoliu$portofoliu_perioada_curenta,
            `Cod Partener`,   !!rlang::sym(vals_portofoliu$categorie_curenta), !!rlang::sym(vals_portofoliu$provizion_curent)), 
        y = dplyr::select(vals_portofoliu$portofoliu_perioada_anterioara,`Cod Partener`,!!rlang::sym(vals_portofoliu$categorie_anterioara),
                        !!rlang::sym(vals_portofoliu$provizion_anterior)),by="Cod Partener") %>%
      dplyr::mutate_at(.vars = 5,~replace(x = .,list = which(is.na(.)),values = 0)) %>% 
      dplyr::mutate(variatie_provizion = .[[3]] - .[[5]])
    
    # This table produces provisions for all CUI that are in perioada_anterioara and are not in perioada curenta
    vals_portofoliu$tabel_beneficiari_iesiti <- dplyr::left_join(y = dplyr::select(vals_portofoliu$portofoliu_perioada_curenta,`Cod Partener`,
      !!rlang::sym(vals_portofoliu$categorie_curenta),!!rlang::sym(vals_portofoliu$provizion_curent)),
              x = dplyr::select(vals_portofoliu$portofoliu_perioada_anterioara,`Cod Partener`,
                !!rlang::sym(vals_portofoliu$categorie_anterioara), !!rlang::sym(vals_portofoliu$provizion_anterior)),
                        by = "Cod Partener") %>%  dplyr::filter(is.na(!!rlang::sym(vals_portofoliu$categorie_curenta))) %>% 
                     dplyr::mutate(variatie_provizion = .[[3]] * (-1))
    
    # I produce my final regularisation provisions
    vals_portofoliu$regularizare_provizioane_non_ifrs <- dplyr::bind_rows(vals_portofoliu$tabel_beneficiari_iesiti,
                                              vals_portofoliu$tabel_variatie_provizioane)
    
    
    output$show_down_button <- renderUI({req(vals_portofoliu$regularizare_provizioane_non_ifrs)
      fluidRow( column(width = 6,
        downloadLink(outputId = ns("down_regularizare_portof"),label = 
                     "Download regularizarea provizioanelor depreciate", class = "glyphicon glyphicon-download-alt"
                    )),
        
        column(width = 3, downloadLink(outputId =ns("down_intrari_deprec"),label = 
                  "Download intrari depreciate", class = "glyphicon glyphicon-download-alt") ),
        
        column(width = 3, downloadLink(outputId =ns("down_iesiri_deprec"),label = 
                "Download iesiri depreciate", class = "glyphicon glyphicon-download-alt") )
      )
        
        })
    
    output$down_regularizare_portof <- downloadHandler(filename = function() {"regularizare_proviz_deprec.csv"},
                content = function(file) {readr::write_csv(x =  vals_portofoliu$regularizare_provizioane_non_ifrs, file = file)})
    
    # Produce intrari/iesiri depreciate - la nivel de contract
    
    vals_portofoliu$depreciate_curenta <- portofoliu_database %>% 
      dplyr::filter(anul_de_raportare == input$migration_to_nonifrs, categorie_contaminata != "standard")
    
    vals_portofoliu$depreciate_anterioara <- portofoliu_database %>% 
      dplyr::filter(anul_de_raportare == input$migration_from_nonifrs, categorie_contaminata != "standard")
    
    bi_cui_existent <- readRDS("R/reactivedata/bi_cui.rds")
    
    
    output$down_intrari_deprec <- downloadHandler(filename = function() {"intrari_deprec.csv"},
        content = function(file) {readr::write_csv( file = file,
          x =  vals_portofoliu$depreciate_curenta %>% 
            dplyr::filter(!DocumentId %in% vals_portofoliu$depreciate_anterioara$DocumentId) %>%
            dplyr::select(Banca, Beneficiar,DocumentId, DocumentNumber = `Nr contract`, `Cod Partener`) %>%
            dplyr::left_join(bi_cui_existent, by = c("Cod Partener" = "Code")) %>%
            dplyr::select(Banca, Beneficiar,DocumentId,DocumentNumber,CUI = `Unique ID`) ) })
    
    output$down_iesiri_deprec <- downloadHandler(filename = function() {"iesiri_deprec.csv"},
              content = function(file) {readr::write_csv( file = file,
              x =  vals_portofoliu$depreciate_anterioara %>% 
                dplyr::filter(!DocumentId %in% vals_portofoliu$depreciate_curenta$DocumentId) %>% 
                     dplyr::select(Banca, Beneficiar,DocumentId, DocumentNumber = `Nr contract`,`Cod Partener`) %>%
                     dplyr::left_join(bi_cui_existent, by = c("Cod Partener" = "Code")) %>%
                     dplyr::select(Banca, Beneficiar,DocumentId,DocumentNumber,CUI = `Unique ID`) ) })
    
  })
  
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
  
  
  
 
  
  
}
    
## To be copied in the UI
# mod_database_portofoliu_ui("database_portofoliu_ui_1")
    
## To be copied in the server
# callModule(mod_database_portofoliu_server, "database_portofoliu_ui_1")
 

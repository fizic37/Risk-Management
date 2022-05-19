#' crc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_crc_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
    bs4Dash::box(title="Upload new CRC file", status = "info",width = 12, collapsible = T, collapsed = F,
                 maximizable = TRUE, icon = icon("file-csv"),
                 shinyjs::useShinyjs(),
                 
                 fluidRow(column(width=6,
                                 fileInput(inputId = ns("crc_input"),label = "Upload CRC file",accept = ".csv",
                                           buttonLabel = "csv only",placeholder = "no file uploaded",width = "300px")),
                          column(width = 6,br(), uiOutput(ns("error_messages")))
                        ),
                 
                 DT::dataTableOutput(ns("sumar_crc_upload")), br(),
                 DT::dataTableOutput(ns("dobanzi_crc")),
                 br(), verbatimTextOutput(ns("diverse")),
                 fillRow(flex = c(1,NA),uiOutput(ns("down_crc_output")),uiOutput(ns("save_crc_output")))
    ),
    
    
    bs4Dash::box(title = "CRC Database", status = "info",width = 12,collapsible = T,collapsed = T,
            footer = "*Scorul maxim al serviciului datoriei este de 50%",
            maximizable = TRUE, icon = icon("database"), 
               DT::dataTableOutput(ns("sumar_baza_date_crc")),
            tags$script(src = "crc_buttons.js"),
            tags$script(paste0("crc_module_js('", ns(''), "')")),
                    br(),  uiOutput(ns("action_baza_date_crc"))),
    
    bs4Dash::box(title = "CIP database & upload", status = "info", id = ns("box_cip"), icon = icon("database"),
                 width = 12,collapsible = T,collapsed = TRUE,maximizable = T,
                 mod_cip_ui("cip_ui_1"), footer = "Se salveaza local in format CSV fisierul din Charisma - General -
                 Rapoarte - CRC (CRCB) - Risc Global - F4CIP si apoi se uploadeaza folosind butonul de mai sus."  )
                            
    )
              
        
}
    
#' crc Server Function
#'
#' @noRd 
mod_crc_server <- function(input, output, session, vals){
  ns <- session$ns
  
  view_baza_date_crc <- readRDS("R/reactivedata/crc/view_baza_date_crc.rds")
 
  vals_crc <- reactiveValues(view_baza_date_crc = view_baza_date_crc, baza_date_crc = NA)
  
  threshold_date_input_crc <- as.Date("2019-12-31")
  
  observeEvent(input$box_cip, {req(any(input$box_cip$collapsed==FALSE,
                                           input$box_cip$maximized==TRUE))
    vals$box_selected <- c(vals$box_selected,"box_cip")
  })
  
  observeEvent(vals_crc$view_baza_date_crc,{
    
    vals_crc$unique_dates <-  vals_crc$view_baza_date_crc[[1]]
    
    vals_crc$actions <- purrr::map_chr(vals_crc$unique_dates, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-sm download_btn" data-toggle="tooltip" data-placement="top" title="Download" id = ', id_,
          ' style="margin: 0; color: #20c997;"><i class="fa fa-download"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )  })
    
    output$sumar_baza_date_crc <- DT::renderDataTable({   dt_generate_function(
          show_buttons = TRUE,    pageLength = 5,
          df = cbind(  tibble::tibble(" " = vals_crc$actions),    vals_crc$view_baza_date_crc ),
          escape = FALSE,     round_col = 3:5,    perc_col = 6:8,
          caption = "Sinteza baza date CRC:" )    })
    })
  
  observeEvent(input$data_raport_to_delete,{
    showModal(modalDialog(title = div(style="display:inline-block;margin-left: 5%;color: #c99720;",
                                      "ATENTIE"), size = "l",
                          h3(paste0("Esti sigur ca vrei sa stergi inregistrarile CRC din data de ",
                                    input$data_raport_to_delete," ?"), style = "color: #c92052"),  footer = 
                            tagList(shinyWidgets::actionBttn(inputId = ns("confirm_delete"),label = "Confirm",
                                        style = "stretch",icon = icon("check"),color = "success",size = "md"),
                                    shinyWidgets::actionBttn(inputId = ns("cancel_delete"),label = "Cancel",
                                          style="stretch",icon = icon("window-close"),color = "danger",size = "md")
                            )))
    })
  
  # Remove modal on cancel delete button
  observeEvent(input$cancel_delete,{ 
    removeModal(session = session)  })
  
  observeEvent(input$confirm_delete,{
    
    if (is.na(vals_crc$baza_date_crc)) {
      vals_crc$baza_date_crc <- readRDS("R/reactivedata/crc/baza_date_crc.rds") %>% 
        dplyr::filter(`Data situatie risc global` != as.Date.character(input$data_raport_to_delete))
      removeModal(session = session)
    }
    
    else { vals_crc$baza_date_crc <- vals_crc$baza_date_crc %>% 
      dplyr::filter(`Data situatie risc global` != as.Date.character(input$data_raport_to_delete))   
    removeModal(session = session)}
    
    
  })
  
  
  observeEvent(vals_crc$baza_date_crc,{ req(!is.na(vals_crc$baza_date_crc))
   
      vals_crc$view_baza_date_crc <- sumar_baza_date_crc_func(df = vals_crc$baza_date_crc)
      
      view_baza_date_crc <- isolate(vals_crc$view_baza_date_crc)
      
      saveRDS(object = view_baza_date_crc,file = "R/reactivedata/crc/view_baza_date_crc.rds")
      
      
      baza_date_crc <- isolate(vals_crc$baza_date_crc)
      
      saveRDS(object = baza_date_crc,file = "R/reactivedata/crc/baza_date_crc.rds")
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
                  .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      
  })
  
  observeEvent(input$data_raport_to_download,{
    
    showModal(modalDialog(title = div(style="display:inline-block;margin-left: 10%;color: #c99720;",
                                      "ATENTIE"), size = "l",
                          h3(paste0("Esti sigur ca vrei sa downloadez CRC la data de ",
                                    input$data_raport_to_download, " ?"), style = "color: #20a7c9"),  footer = 
      tagList(shinyWidgets::downloadBttn(outputId = ns("confirm_download"),label = "Download",
                                   color = "success", size = "md",style = "stretch"),
      shinyWidgets::actionBttn(inputId = session$ns("cancel_download"),label = "Cancel",
                   style = "stretch",icon = icon("window-close"),color = "danger",size = "md")
                            )))
  })
  
  observeEvent(input$cancel_download, {removeModal( session = session ) } )
  
  output$confirm_download <- downloadHandler(filename = function() {
      paste0("CRC_database_",  input$data_raport_to_download, ".csv")   },
      content = function(file) {
        readr::write_csv(x = readRDS("R/reactivedata/crc/baza_date_crc.rds") %>%
        dplyr::filter(`Data situatie risc global` == as.Date.character(input$data_raport_to_download)),
                         file = file)
        removeModal(session = session)  }   )
  
    
    

  ##### Upload CRC
  
  output$error_messages <- renderUI( div (style="display:inline-block;margin-left: 10%;padding-top: 5px;color: #20a7c9;", 
  "Se uploadeaza fisierul CRC din Charisma - General - Rapoarte - CRC - Ris Global F4CRED - Data situatie risc global - Salveaza rapid CSV." ) )
  
  observeEvent(input$crc_input,{
    
    rata_dobanzii <- readRDS("R/reactivedata/crc/rata_dobanzii.rds")
    
    
    rata_dobanzii <- rata_dobanzii %>% dplyr::bind_rows(
      rata_dobanzii %>% dplyr::filter(MONEDA == "EUR") %>% dplyr::mutate(dplyr::across(
        .cols = c(MONEDA, valuta_termen, valuta_termen_data),
        .fns = ~ stringr::str_replace_all(string = .x, pattern = "EUR", "NOK")
      )) )
    
    baza_date_crc_sliced <- readRDS("R/reactivedata/crc/baza_date_crc_sliced.rds")
    
    ratele_dobanzii_valabile <-  rata_dobanzii %>% dplyr::select(2:4) %>% 
      tidyr::pivot_wider(names_from = MONEDA,values_from = `Rata dobanzii`)
    
    crc_input_citit <- reactive({  shiny::validate(shiny::need(tools::file_ext(input$crc_input$datapath)=="csv",
                          message = paste0("CSV only. You uploaded a ",tools::file_ext(input$crc_input$datapath)," file.")))
      readr::read_csv(input$crc_input$datapath) })
    
    nume_obligatorii <- c('Data situatie risc global', 'Unitate bancara', 'Cod debitor','Acronim persoana declaranta',
                          'Cod CRC', 'Comportament credit','Tip Credit', 'Termen acordare', 'Serviciul datoriei',
                          'Cod Valuta', 'Suma acordata', 'Suma datorata total','Suma datorata utilizata',
                          'Suma restanta', 'Data acordarii','Data scadentei')
    
    
    crc_csv <- reactive ({ shiny::validate(shiny::need(expr = nume_obligatorii %in% names(crc_input_citit()) %>% all(),
                        message = paste0("Lipsesc coloanele: ",paste0(setdiff(nume_obligatorii, names(crc_input_citit())),
                                      collapse = ";"))))
      
      readr::read_csv(input$crc_input$datapath,
        col_types = readr::cols('Data acordarii' = readr::col_date(format = "%d/%m/%Y"),
          'Data scadentei' = readr::col_date(format = "%d/%m/%Y"),
          `Data situatie risc global` = readr::col_date(format = "%m/%d/%Y 12:00:00 AM"),
          'Comportament credit' = readr::col_character() )  ) %>% 
        dplyr::select(nume_obligatorii) %>% dplyr::filter(`Unitate bancara` == "*") %>%
        dplyr::mutate(valuta_termen = paste0(`Cod Valuta`,
            stringr::str_sub(`Termen acordare`, start = 1, end = 1) ),
          Principal_de_Rambursat = ifelse(`Comportament credit` == "L",
            `Suma restanta`,  ifelse(`Data scadentei` <= lubridate::`%m+%`(`Data situatie risc global`, months(12)),
              `Suma datorata total`, pmax(`Suma restanta`,  (`Suma datorata total` * 365) / as.numeric(
                  difftime( time2 = `Data situatie risc global`,
                    time1 = `Data scadentei`,  units = "days")  ) )  )  )   ) %>%
        dplyr::mutate(Principal_de_Rambursat = as.numeric(Principal_de_Rambursat)) })
    
    
    crc_csv_rata_dobanzii <- reactive ({
        crc_csv() %>% dplyr::mutate(rata_dobanzii =
                      rata_dobanzii$`Rata dobanzii`[match(x = crc_csv()$valuta_termen,
                              table = rata_dobanzii$valuta_termen)]) %>%
          dplyr::mutate(Dobanda_de_Rambursat = rata_dobanzii * (2 * `Suma datorata total` - Principal_de_Rambursat) / 2) %>%
          dplyr::mutate(Rate_datorate = Principal_de_Rambursat + Dobanda_de_Rambursat) })  
    
    crc_cui <-  reactive ({  crc_csv_rata_dobanzii() %>% dplyr::group_by(`Cod debitor`,`Serviciul datoriei`) %>%  
        dplyr::summarise(suma_totala_utilizata = sum(`Suma datorata utilizata`)) %>% 
        tidyr::spread(key = "Serviciul datoriei", value = suma_totala_utilizata, fill = 0) %>% as.data.frame() %>% 
        dplyr::mutate(total_sume_utilizate = rowSums(dplyr::select(., -1))) %>%
        dplyr::mutate(scor_serv_datorie=(0.5*.[[2]]+0.25*.[[3]]+0.15*.[[4]]+0.1*.[[5]])/(total_sume_utilizate)) %>% 
        dplyr::mutate(scor_serv_datorie = ifelse(is.na(scor_serv_datorie),0.5,scor_serv_datorie)) %>%
        dplyr::mutate(are_restante_peste_30_zile = ifelse((.[[2]]+.[[3]])/(total_sume_utilizate)>= 0.95,0,1)) %>% 
        dplyr::mutate(are_restante_peste_30_zile = ifelse(is.na(are_restante_peste_30_zile),0,are_restante_peste_30_zile))  })
    
    crc_final <-  reactive ({ dplyr::left_join(y = crc_cui() %>% dplyr::select(.,`Cod debitor`,
                        scor_serv_datorie,are_restante_peste_30_zile),  x=crc_csv_rata_dobanzii(),by="Cod debitor")   })
    
    sumar_crc_upload <- reactive({ crc_final() %>% dplyr::group_by(`Cod debitor`, `Data situatie risc global`) %>% 
        dplyr::summarise(Nr_beneficiari = dplyr::n_distinct(`Cod debitor`),
                         Total_rate_datorate =   sum(Rate_datorate),
                         Rate_medii_datorate = sum(Rate_datorate),
                         are_restante_peste_30_zile = mean(are_restante_peste_30_zile),
                         scor_mediu_serv_datorie = mean(scor_serv_datorie),
                         sume_totale_utilizate = sum(`Suma datorata utilizata`)) %>%
                          dplyr::group_by(`Data situatie risc global`) %>% 
        dplyr::summarise(`Nr beneficiari` = sum(Nr_beneficiari),
            `Total rate datorate` = sum(Total_rate_datorate),  
            `Rate datorate medii per beneficiar` =      mean(Rate_medii_datorate),
            `Nr mediu beneficiari cu restante peste 30 de zile` = mean(are_restante_peste_30_zile),
            `Scor mediu serviciul datoriei` = mean(scor_mediu_serv_datorie),
            `Scor mediu serviciul datoriei ponderat cu sumele utilizate` = 
                   weighted.mean(scor_mediu_serv_datorie, w = sume_totale_utilizate))     })
    
    output$sumar_crc_upload <- DT::renderDataTable({req(input$crc_input)
      dt_generate_function(df = sumar_crc_upload(),round_col = 2:4,perc_col = 5:7,caption = "Sinteza fisier uploadat:")   })
    
    data_crc_upload <- reactive({ crc_final()$`Data situatie risc global`[1] })
    
    
    vals_crc$check_type_crc <- janitor::compare_df_cols_same(baza_date_crc_sliced, crc_final() %>% 
        dplyr::select(-`Cod CRC`, - `Acronim persoana declaranta`), bind_method = "rbind",verbose = TRUE)
    
    
    observeEvent(vals_crc$check_type_crc,{
    
    output$save_crc_output <- renderUI({req(vals_crc$check_type_crc)
      shinyWidgets::actionBttn(inputId = session$ns("save_crc"),label = "Save CRC uploadat to database",
                 icon = icon("save"), style = "stretch",color = "success",size = "sm")  })
    
    
    output$dobanzi_crc <- DT::renderDataTable({req(sumar_crc_upload,vals_crc$check_type_crc)
      dt_generate_function(df=ratele_dobanzii_valabile,perc_col = 2:5,digits_perc = 2, round_col = NULL,
                           caption = "Voi folosi ratele dobanzii de mai jos pentru a calcula ratele datorate aferente fisierului uploadat:")     })
    
    output$down_crc_output <- renderUI({req(vals_crc$check_type_crc)
      shinyWidgets::downloadBttn(outputId = ns("down_crc_input"),label = "Download CRC uploadat",
                                 style = "stretch",color = "success")     })
    
    output$down_crc_input <- downloadHandler(filename = function(){"CRC.csv"},content = function(file){
      readr::write_csv(x = crc_final() %>% dplyr::select(-`Unitate bancara`, - `Tip Credit`, -valuta_termen) %>%
                         dplyr::relocate(Principal_de_Rambursat,.after = rata_dobanzii), path=file) }  )
    
    if (vals_crc$check_type_crc) {
    output$error_messages <- renderUI( div (style="display:inline-block;margin-left: 20%;padding-top: 5px;color: #20c997;font-size: 1.2rem;",
                                            "Fisierul CRC a fost prelucrat cu succes")  ) }
    else {
      output$error_messages <- renderUI({
        janitor::compare_df_cols_same("existing database"=baza_date_crc_sliced,"uploaded file" = crc_final() %>% 
            dplyr::select(-`Cod CRC`, - `Acronim persoana declaranta`), bind_method = "rbind",verbose = TRUE) }) 
    }
    
    })
    
    
    
    observeEvent(input$save_crc,{
      removeUI("#crc_ui_1-dobanzi_crc")
      baza_date_crc <- readRDS("R/reactivedata/crc/baza_date_crc.rds")
      database_crc_reactive <- reactiveValues(df_old = baza_date_crc, 
                                 df_new = crc_final(), element_id = data_crc_upload(), 
                                 column_id = "Data situatie risc global", finalise_process_compare_df = FALSE)
      
      callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = database_crc_reactive)
      
      observe({req(database_crc_reactive$finalise_process_compare_df)
        vals_crc$baza_date_crc <- database_crc_reactive$df_new_prel
        shinyjs::disable(id = "crc_input",asis = FALSE)   })
      
    })
    
  })
  
}
    
## To be copied in the UI
# mod_crc_ui("crc_ui_1")
    
## To be copied in the server
# callModule(mod_crc_server, "crc_ui_1")
 

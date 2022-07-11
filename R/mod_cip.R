#' cip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cip_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow( 
      shinyFeedback::useShinyFeedback(feedback = TRUE,toastr = TRUE),
      shinyjs::useShinyjs(),
   
      column(width = 4,fileInput(inputId = ns("cip_input"), width = "300px",
                                 accept = '.csv',buttonLabel = 'CSV only',label = "Upload CIP file")),
      
      column(width = 4, br(), uiOutput(ns("save_cip_output") )),
      
      column(width = 4, uiOutput(ns("show_upload_messages"))),
      
      column(width = 12, DT::dataTableOutput(ns("cip_upload_sumar")) ),
      
      column(width = 12, hr()),
      
      column(width = 12,    DT::dataTableOutput(ns("cip_database")),
            tags$script(src = "cip_delete_button.js"),
            tags$script(paste0("cip_delete_module_js('", ns(''), "')")) )
    
    )  )
  
                        
}
    
#' cip Server Function
#'
#' @noRd 
mod_cip_server <- function(input, output, session) {
  ns <- session$ns
 
  
  view_cip_database <- readRDS("R/reactivedata/cip/view_cip_database.rds")
  
  vals_cip <- reactiveValues(view_cip_database = view_cip_database)
  
  nume_obligatorii_cip <- c("Data situatie risc global", "Cod debitor",  "Nume debitor",
                            "Total CEC-uri",  "Total cambii", "Total bilete la ordin","CEC-uri majore",
                            "Cambii majore", "Bilete la ordin majore", "Per de interdictie bancara")
  # Key observer. Everytime it updates I update actions and render teh main table.
  # Note I do not save it, as it will be saved from the beginning. I only save it inside vals_cip$baza_date_cip observer
  observeEvent(vals_cip$view_cip_database,{
    
    vals_cip$unique_dates <-  vals_cip$view_cip_database$data_raport
    
    vals_cip$actions <- purrr::map_chr(vals_cip$unique_dates, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-sm download_btn" data-toggle="tooltip" data-placement="top" title="Download" id = ', id_,
          ' style="margin: 0; color: #20c997;"><i class="fa fa-download"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )  })
    
    output$cip_database <- DT::renderDataTable(dt_generate_function(caption = "Sinteza baza date CIP:",
               df = cbind(tibble::tibble(" " = vals_cip$actions), vals_cip$view_cip_database),
               show_buttons = TRUE, round_col=3:6, escape = FALSE, pageLength=5 ) )
    
    })
  

  observeEvent(input$id_data_raport_to_download,{
     
    showModal(modalDialog(title = div(style="display:inline-block;margin-left: 10%;color: #c99720;",
                                      "ATENTIE"),size = "l",
                        h3(paste0("Esti sigur ca vrei sa downloadezi baza de date CIP raportata la ",
                      input$id_data_raport_to_download, " ?"), style = "color: #20a7c9"),  footer = 
      tagList(shinyWidgets::downloadBttn(outputId = session$ns("down_cip_database"),label = "Download",
                                         color = "success", size = "md", style = "stretch"),
              shinyWidgets::actionBttn(inputId = session$ns("cancel_download"),label = "Cancel",
                      icon = icon("window-close"),color = "danger",size = "md", style = "stretch"))))
    
    
    
    output$down_cip_database <- downloadHandler(filename = function(){paste0("cip_database_",input$id_data_raport_to_download,".csv")},
        content = function(file){
          
            readr::write_csv(x = readRDS("R/reactivedata/cip/baza_date_cip.rds") %>% 
                      dplyr::filter(data_raport == as.Date.character(input$id_data_raport_to_download)),
                path = file) 
          removeModal(session = session)
        } )
    
    observeEvent(input$cancel_download, {removeModal(session = session) } )
    
    
    
    
    
    })
  
  observeEvent(input$id_data_raport_to_delete,{
    
    showModal(modalDialog(title = div(style="display:inline-block;margin-left: 5%;color: #c99720;",
                                      "ATENTIE"), size = "l",
                          h3(paste0("Esti sigur ca vrei sa stergi CIP-ul la data de ",
                                    input$id_data_raport_to_delete, " ?"), style = "color: #c94220"),  
                          footer = 
                            tagList(shinyWidgets::actionBttn(inputId = ns("confirm_delete"),label = "Confirm, sterge inregistrarea",
                                        icon = icon("check"),color = "success",size = "md", style = "stretch"),
                                    shinyWidgets::actionBttn(inputId = ns("cancel_delete"),label = "Cancel, close window",
                                        icon = icon("window-close"),color = "danger",size = "md", style = "stretch")
                            )))
  })
  
  observeEvent(input$cancel_delete,{
    removeModal(session = session) })
  
  
  observeEvent(input$confirm_delete,{
    
    vals_cip$baza_date_cip <- dplyr::filter(readRDS("R/reactivedata/cip/baza_date_cip.rds"),data_raport != 
                                              as.Date.character(input$id_data_raport_to_delete))
    removeModal(session = session)
    
    shinyFeedback::showToast(type = "success",title = "Deleted",message = "Inregistrarea a fost stearsa cu succes",
                             .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    
  })
    
  # Key observer everytime baza date cip is updated: recalculates view_cip and saves baza_date_cip and view_cip
  # Attention: I save view_cip only when baza date cip is updated, otherwise it would be saved even for the first read
  observeEvent(vals_cip$baza_date_cip,{
    
    vals_cip$view_cip_database <- vals_cip$baza_date_cip %>% dplyr::group_by(data_raport) %>% dplyr::summarise(
      Nr_beneficiari_raportati = dplyr::n_distinct(CUI),
      Nr_beneficiari_interdictie =
        sum(Este_in_interdictie),
      Nr_mediu_incidente_majore =
        mean(total_incidente_majore),
      Median_incidente_majore = median(total_incidente_majore)) %>% dplyr::arrange(dplyr::desc(data_raport))
    
    baza_date_cip <- isolate(vals_cip$baza_date_cip)
    view_cip_database <- isolate(vals_cip$view_cip_database)
    saveRDS(view_cip_database,file = "R/reactivedata/cip/view_cip_database.rds")
    saveRDS(baza_date_cip,file = "R/reactivedata/cip/baza_date_cip.rds")
  })

 
  # Observer for CIP upload. I have used reactive() so save upload observer is nested inside upload observer in order to 
 # have access to values stored with reactive(). 
  
  observeEvent(input$cip_input,{
   
    check_cip_names <- reactive( { req(input$cip_input)
      nume_citite <- names(readr::read_csv(input$cip_input$datapath,n_max = 10))
      
      if( nume_citite %in% nume_obligatorii_cip %>% all() ) {"0" } else {
        vals_cip$nume_lipsa <- setdiff(nume_obligatorii_cip, nume_citite)
        "1" }
    })
   # Main processing line of cip upload file
   CIP <- reactive({ req( check_cip_names() == "0" )
     
     shiny::validate(shiny::need(tools::file_ext(input$cip_input$datapath)=="csv",message = paste0("CSV only. You uploaded a ", 
                        tools::file_ext(input$cip_input$datapath), " file.")))
     
     readr::read_csv(input$cip_input$datapath, col_types = readr::cols(
       `Data situatie risc global` = readr::col_date(format = "%m/%d/%Y 12:00:00 AM"),
       `Cod debitor` = readr::col_double(),  `Nume debitor` = readr::col_character(),
      `Total CEC-uri` = readr::col_double(),  `Total cambii` = readr::col_double(),
       `Total bilete la ordin` = readr::col_double(),`CEC-uri majore` = readr::col_double(),
        `Cambii majore` = readr::col_double(), `Bilete la ordin majore` = readr::col_double(),
              `Per de interdictie bancara` = readr::col_character())) %>% dplyr::mutate(
                             total_incidente = `Total CEC-uri` + `Total cambii` + `Total bilete la ordin`,
                             total_incidente_majore = `Bilete la ordin majore` + `CEC-uri majore` + `Cambii majore`,
                lista_date = qdapRegex::ex_date(`Per de interdictie bancara`)) %>% 
       dplyr::mutate(lungime_lista_date = purrr::map_dbl(lista_date, ~ length(.))) %>% 
       dplyr::mutate(data_expirare = dplyr::case_when(is.na(`Per de interdictie bancara`) ~ as.Date("1999-01-01", 
        format = "%Y-%m-%d"), !is.na(`Per de interdictie bancara`) & lungime_lista_date == 1 ~ as.Date.numeric(
          as.numeric(`Per de interdictie bancara`), origin = "1899-12-30") %>% as.Date(),
        !is.na(`Per de interdictie bancara`) & lungime_lista_date != 1 ~ qdapRegex::ex_date(`Per de interdictie bancara`) %>%
          purrr::map_chr(~ .x[[length(.x)]]) %>% as.Date(format = "%d-%m-%Y")),
          data_intrare = dplyr::case_when(is.na(`Per de interdictie bancara`) | lungime_lista_date == 1 ~ as.Date("1999-01-01",
            format = "%Y-%m-%d"),!is.na(`Per de interdictie bancara`) & lungime_lista_date > 1 ~ qdapRegex::ex_date(`Per de interdictie bancara`) %>% 
              purrr::map_chr(~ .x[[max(length(.x) - 1, 1)]]) %>% as.Date(format = "%d-%m-%Y"))) %>% 
                dplyr::mutate(Este_in_interdictie = ifelse(data_expirare >= `Data situatie risc global` &  data_intrare <= `Data situatie risc global`,1,0),
                              A_fost_interdictie = ifelse(is.na(`Per de interdictie bancara`), 0, 1)) %>% 
                  dplyr::select(CUI=`Cod debitor`,total_incidente,  total_incidente_majore,Este_in_interdictie,A_fost_interdictie,
                     bilete_majore = `Bilete la ordin majore`,data_raport=`Data situatie risc global`)   })
   
   cip_upload_sumar <- reactive({ CIP() %>% dplyr::group_by(data_raport) %>% 
       dplyr::summarise(Nr_beneficiari_raportati = dplyr::n_distinct(CUI),
              beneficiari_interdictie=sum(Este_in_interdictie), Nr_mediu_incidente_majore=mean(total_incidente_majore),
              Median_incidente_majore=median(total_incidente_majore)) %>% dplyr::ungroup() })
   
    
    
    
   data_upload_cip <- reactive({ req( check_cip_names() == "0")
     
     cip_upload_sumar()$data_raport  })
   
   output$cip_upload_sumar <- DT::renderDataTable({req( check_cip_names() == "0", cip_upload_sumar() )
     dt_generate_function(round_col = 2:5,caption = "Sinteza fisier uploadat:", df = cip_upload_sumar()) })
   
  
  output$save_cip_output <- renderUI({ req(cip_upload_sumar())
    div (style="display:inline-block;margin-left: 20%;padding-top: 5px;",
   shinyWidgets::actionBttn(inputId = ns("save_cip_upload"),style = "stretch",color = "success",
                  label = "Save CIP upload",icon = icon("save") ) )  })
  
   output$show_upload_messages <- renderUI( {  req( check_cip_names() )
     switch ( check_cip_names(),
              "0" = div (style="display:inline-block;margin-left: 20%;padding-top: 25px;color: #20c997;font-size: 1.2rem;",
                          "Fisierul CIP a fost prelucrat cu succes"),
              "1" = div (style="display:inline-block;margin-left: 20%;padding-top: 23px;color: #c94220;font-size: 1rem;",
                         paste0("STOP, lipsesc coloanele: ",paste0(vals_cip$nume_lipsa,collapse = ";")) %>% stringr::str_c()) )        
     })
  
 
  # I nest save observer inside cip$input observer in order to have access to reactive values that are isolated within observeEvent.
 
  observeEvent(input$save_cip_upload,{
      
      removeUI("#cip_ui_1-cip_upload_sumar")
      removeUI("#cip_ui_1-save_cip_upload")
      shinyjs::disable(id = "cip_input")
      
     unique_cip_database_dates <- vals_cip$view_cip_database$data_raport
     
     # I load slice data in order to check compatibility of upload data with main cip database
     # through janitor-bind_rows with slice data. Upload main database only if this compatibility is true
     slice_cip_database <- readRDS("R/reactivedata/cip/slice_cip_database.rds")
     
     check_type_cip <- reactive({janitor::compare_df_cols_same(slice_cip_database,CIP(),
                                   bind_method = "rbind",verbose = FALSE) })
     
     if (check_type_cip() && !data_upload_cip() %in% unique_cip_database_dates) {
       
       vals_cip$baza_date_cip <- rbind(CIP(),readRDS("R/reactivedata/cip/baza_date_cip.rds"))
       updateActionButton(session = session, inputId = "save_cip_upload")
      shinyFeedback::showToast(type = "success",message = "Upload successfull",title = "Baza de date a fost uploadata cu succes",
          .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
      }
     
     else if (data_upload_cip() %in% unique_cip_database_dates){
       shinyWidgets::sendSweetAlert(session = session,title = "STOP",text = "Am deja data furnizata de tine",type = "warning",
                                    btn_colors = "#20a7c9",btn_labels = "OK, am inteles.") 
       updateActionButton(session = session, inputId = "save_cip_upload")
       #removeUI("#cip_ui_1-save_cip_upload")
       }
     
     })
  
  })
  

 
  
  
}
 
    
## To be copied in the UI
# mod_cip_ui("cip_ui_1")
    
## To be copied in the server
# callModule(mod_cip_server, "cip_ui_1")
 

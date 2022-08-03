#' provizioane_plati UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_provizioane_plati_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
    shinyFeedback::useShinyFeedback(),
    
    bs4Dash::box( title = "Word- regularizare provizion plati", status = "info",  width = 12,
                  collapsible = T,  collapsed = TRUE,maximizable = TRUE, icon = icon("file-word"),
                  footer = "Aici se genereaza adresa Word catre Contabilitate privind regularizarea provizioanelor specifice
                  la datele selectate mai sus.", id = ns("box_word_plati"),
                  fluidRow(    column( width = 3,
                      selectInput(  inputId = ns("from_plati"), label = "Selecteaza data initiala:",
                        choices = "",    width = "300px")        ),
                    
                      column( width = 3,
                      selectInput(inputId = ns("to_plati"), label = "Selecteaza data curenta:",
                        choices = "", width = "300px") ),
                    
                      column( width = 6, br(), div(style="display:inline-block;margin-left: 20%;padding-top: 5px;",
                      downloadLink(outputId = ns("generate_report"),
                                   label = "Click aici pentru a downloada adresa catre contabilitate") ),
                      tags$head(tags$style("#provizioane_plati_ui_1-generate_report {color: #c92052;}")) ),
                      
                      DT::dataTableOutput(ns("variatie_plati"))
                  )),
   
    bs4Dash::box(title = "Upload file provizioane plati", width = 12, icon = icon("file-excel"),
                 collapsed = TRUE,collapsible = TRUE, maximizable = TRUE, status = "info",
                 footer = "Se uploadeaza fisierul din Charisma - Rapoarte adaptive - Situatie Plati Provizioane la data.
                 Atentie, datele generate din Charisma se copiaza intr-un fisier Excel nou, se salveaza local si
                 apoi se uploadeaza.",  id = ns("box_upload_plati"),
                 mod_database_upload_plati_ui("database_upload_plati_ui_1")),
    
    bs4Dash::box(title = "Baza de date a provizioanelor pentru plati",collapsed = FALSE, id = ns("box_database_plati"),
                        width=12,collapsible = T,status = "info",maximizable = T, icon = icon("database"),
                        
                 DT::dataTableOutput(ns("database_plati")),
                 
                 tags$script(src = "plati_buttons.js"),
                 tags$script(paste0("plati_module_js('", ns(''), "')"))),
    
    bs4Dash::box(title = "Coeficienti provizioane plati",width = 12, icon = icon("subscript"),
                collapsed = TRUE, collapsible = TRUE, maximizable = TRUE, status="info", id = ns("box_coeficienti_plati"),
                mod_coeficienti_plati_ui("coeficienti_plati_ui_1") )
    
  )
        
  
  
}
    
#' provizioane_plati Server Function
#'
#' @noRd 
mod_provizioane_plati_server <- function(input, output, session,vals, plati_reactive){
 # Server processing for 2 boxes: Baza de date plati si Regularizare provizion
   ns <- session$ns
  
  plati_reactive$view_sumar_plati <- readRDS("R/reactivedata/plati/view_sumar_plati.rds")
  
  # Below observer saves baza provizioane plati every time baza provizioane plati se schimba(la upload,
  # sau la delete din baza)
  
  observeEvent( plati_reactive$baza_provizioane_plati, { req(!is.na(plati_reactive$baza_provizioane_plati),)
    plati_reactive$view_sumar_plati <- plati_reactive$baza_provizioane_plati %>% dplyr::group_by(data_raport) %>% 
        dplyr::summarise(Nr_contracte = dplyr::n(), PlatiBrute = sum(PlatiEfective), RecuperariTotale = sum(TotalRecuperat),
                         Garantii_accesorii_admise =  sum(ValoareAdmisaFNG), Expunere_CTG_plati = sum(Expunere_CTG_plati),  
                         Provizioane_Constituite = sum(ProvizionNou), CreanteNete = PlatiBrute - RecuperariTotale,  
                         Grad_Acoperire_Provizioane =  Provizioane_Constituite / CreanteNete) %>%
        dplyr::select(1:4,8,7,9,5:6) %>% dplyr::arrange(desc(data_raport))
      
      view_sumar_plati <- isolate(plati_reactive$view_sumar_plati)
      
      saveRDS(object = view_sumar_plati,file = "R/reactivedata/plati/view_sumar_plati.rds")
      
      baza_provizioane_plati <- isolate(plati_reactive$baza_provizioane_plati)
      
      saveRDS(object = baza_provizioane_plati,file = "R/external_volumes/baza_provizioane_plati/baza_provizioane_plati.rds")
      
    
    shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
              .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    
  })
  
  # Below observers update vals$box_selected which is used inisde app_server.R to call corresponding modules
  observeEvent(input$box_upload_plati, {req(any(input$box_upload_plati$collapsed==FALSE,
                                          input$box_upload_plati$maximized==TRUE))
    vals$box_selected <- c(vals$box_selected,"box_upload_plati")
   })
  
  observeEvent(input$box_coeficienti_plati, {req(any(input$box_coeficienti_plati$collapsed==FALSE,
                                                input$box_coeficienti_plati$maximized==TRUE))
    vals$box_selected <- c(vals$box_selected,"box_coeficienti_plati")
  })
  
  # Main observer for generating datatable 
  observeEvent(plati_reactive$view_sumar_plati,{
    
    plati_reactive$unique_dates <-  plati_reactive$view_sumar_plati$data_raport
    
    plati_reactive$actions <- purrr::map_chr(plati_reactive$unique_dates, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-sm download_btn" data-toggle="tooltip" data-placement="top" title="Download" id = ', id_,
          ' style="margin: 0; color: #20c997;"><i class="fa fa-download"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_,' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )  })
    
    output$database_plati <- DT::renderDataTable({dt_generate_function(round_col = c(3:7,9:10),perc_col = 8,
                df = cbind(tibble::tibble(" " = plati_reactive$actions ), plati_reactive$view_sumar_plati),
                   show_buttons = TRUE,pageLength = 5, escape = FALSE,
                       caption = "Sumar baza date Provizioane Plati:") })
    updateSelectInput(session = session, inputId = "from_plati",selected = plati_reactive$unique_dates[2],
                      choices = plati_reactive$unique_dates)
    updateSelectInput(session = session, inputId = "to_plati",selected =  plati_reactive$unique_dates[1],
                      choices = plati_reactive$unique_dates)
    
  })
  
  # Observer for button delete inside datatable for baza plati
  observeEvent(input$data_raport_to_delete,{
    
    showModal(modalDialog(title = "STOP", size = "l",
                          h3(paste0("Esti sigur ca vrei sa stergi provizioanele specifice la data de ",
                                    input$data_raport_to_delete, " ?"), style = "color: #c92052"),  
                          footer = 
                            tagList(shinyWidgets::actionBttn(inputId = session$ns("confirm_delete"),label = "Confirm delete",
                                    icon = icon("check"),color = "success",size = "md",style = "stretch"),
                                    shinyWidgets::actionBttn(inputId = session$ns("cancel_delete"),label = "Cancel",style = "stretch",
                                                             icon = icon("window-close"),color = "danger",size = "md")
                            )))
  }) 
  
  # Remove modal on cancel delete button
  observeEvent(input$cancel_delete,{
    removeModal(session = session)  })
  
  # Update of plati_recative$baza_provizioane_plati aftre delete confirmation. This update will trigger the above
  # observer which saves baza provizioane plati because plati_recative$baza_provizioane has changed
  observeEvent(input$confirm_delete,{
    
    plati_reactive$baza_provizioane_plati <- readRDS("R/external_volumes/baza_provizioane_plati/baza_provizioane_plati.rds") %>% 
      dplyr::filter(data_raport != as.Date.character(input$data_raport_to_delete))
    removeModal(session = session)
  })
  
  # Observer for download button inside datatable baza plati
  observeEvent(input$data_raport_to_download,{
    
    showModal(modalDialog(title = "STOP", size = "l",
                          h3(paste0("Esti sigur ca vrei sa downloadezi provizioanele specifice la data de ",
                                    input$data_raport_to_download, " ?"), style = "color: #20c943"),  footer = 
                            tagList(shinyWidgets::downloadBttn(outputId = session$ns("confirm_download"),label = "Download",
                                                               color = "success", size = "md",style = "stretch"),
                                    shinyWidgets::actionBttn(inputId = session$ns("cancel_download"),label = "Cancel",
                                              icon = icon("window-close"),color = "danger",size = "md", style="stretch")
                            )))
    
    output$confirm_download <- downloadHandler(filename = function() {
      paste0("provizioane_plati_",  input$data_raport_to_download, ".csv")   },
      content = function(file) {
        readr::write_csv(x = readRDS("R/external_volumes/baza_provizioane_plati/baza_provizioane_plati.rds") %>%
                           dplyr::filter(data_raport ==  as.Date.character(input$data_raport_to_download)),
                         path = file)
        removeModal(session = session)  }   )
    
    
  })
  
  # Remove modal on cancel download button
  observeEvent(input$cancel_download,{
    removeModal(session = session)  })
  
  # Observer for regularizare provizion
  observeEvent(c(input$from_plati,input$to_plati),{
    text_data_directory <- "data"
    output$generate_report <- downloadHandler(filename = function() {"regularizare_proviz_plati.docx"},
                                              content = function(file) {
        temporary_directory <- tempdir()
        tempReport <- file.path(temporary_directory, "test_word.Rmd")
        templateReport <- file.path(temporary_directory, "template_provizioane_plati.docx")
        file.copy(from = "test_word.Rmd",to =  tempReport, overwrite = TRUE)
        file.copy(from = "template_provizioane_plati.docx",to =  templateReport, overwrite = TRUE)
          params = list(luna_curenta = lubridate::month(input$to_plati,label = TRUE,abbr = FALSE),
                                                              an_curent = lubridate::year(input$to_plati),
                                                              provizion_curent = plati_reactive$view_sumar_plati$Provizioane_Constituite[
                                                                plati_reactive$view_sumar_plati$data_raport == input$to_plati],
                                                              provizion_anterior = plati_reactive$view_sumar_plati$Provizioane_Constituite[
                                                                plati_reactive$view_sumar_plati$data_raport == input$from_plati],
                                                              nr_garantii = plati_reactive$view_sumar_plati$Nr_contracte[
                                                                plati_reactive$view_sumar_plati$data_raport == input$to_plati],
                                                              luna_anterioara = lubridate::month(input$from_plati,label = TRUE,abbr = FALSE),
                                                              an_anterior = lubridate::year(input$to_plati),
                                                              plati_brute_curente = plati_reactive$view_sumar_plati$PlatiBrute[
                                                                plati_reactive$view_sumar_plati$data_raport == input$to_plati],
                                                              recuperari_totale_curente = plati_reactive$view_sumar_plati$RecuperariTotale[
                                                                plati_reactive$view_sumar_plati$data_raport == input$to_plati],
                                                              admise_garantie_curente = plati_reactive$view_sumar_plati$Garantii_accesorii_admise[
                                                                plati_reactive$view_sumar_plati$data_raport ==  input$to_plati],
                                                              data_curenta = paste(lubridate::day(input$to_plati),lubridate::month(input$to_plati),
                                                                                   lubridate::year(input$to_plati),sep = "."),
                                                              data_anterioara =  paste(lubridate::day(input$from_plati),lubridate::month(input$from_plati),
                                                                                       lubridate::year(input$from_plati),sep = "."))
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv()) ) 
    }) 
  })
  
  sumar_plati_curent <- eventReactive( input$to_plati,{ req( plati_reactive$view_sumar_plati )
    
  plati_reactive$view_sumar_plati %>% dplyr::select(-Grad_Acoperire_Provizioane) %>% 
    dplyr::filter(data_raport == input$to_plati ) %>% t() %>% 
    data.frame() %>% dplyr::slice(-1) %>%  setNames( input$to_plati ) })
  
  sumar_plati_anterior <- eventReactive( input$from_plati,{ req( plati_reactive$view_sumar_plati )
    
    plati_reactive$view_sumar_plati %>% dplyr::select(-Grad_Acoperire_Provizioane) %>% 
      dplyr::filter(data_raport == input$from_plati ) %>% t() %>% 
      data.frame() %>% dplyr::slice(-1) %>%  setNames( input$from_plati ) })
  
  output$variatie_plati <- DT::renderDataTable( { req( sumar_plati_curent(), sumar_plati_anterior())
   
     dt_generate_function(df = sumar_plati_curent() %>% cbind( sumar_plati_anterior() ) %>% 
       dplyr::mutate(dplyr::across(.cols = c(1:2), ~as.numeric(.x))) %>%  
         dplyr::mutate(Variatie = .[[1]] - .[[2]]),  round_col = 1:3,rownames = TRUE, show_buttons=TRUE,
    caption = paste0("Variatia provizioanelor pentru plati la data de ", input$to_plati, " fata de ", input$from_plati),  )  })
  
  
}
    
## To be copied in the UI
# mod_provizioane_plati_ui("provizioane_plati_ui_1")
    
## To be copied in the server
# callModule(mod_provizioane_plati_server, "provizioane_plati_ui_1")
 

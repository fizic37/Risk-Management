#' database_upload_plati UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_upload_plati_ui <- function(id){
  ns <- NS(id)
 tagList(
      shinyjs::useShinyjs(),
      fluidRow(column(width = 6,
        fileInput(inputId = ns("plati_input"),      width = "300px",
          label = "Upload provizioane plati file",
          buttonLabel = "Excel Only",
          placeholder = "No file uploaded",
          accept = c(".xls", ".xlsx") )  ), 
      column(width = 6, uiOutput( ns("show_plati_date") ))),
      
      #verbatimTextOutput(ns("diverse")),
      DT::dataTableOutput(ns("sumar_plati_input")),
      br(),
      uiOutput(ns("output_save_plati")),
      br(),
      
      uiOutput(ns("show_balanta_parteneri")),
     
      DT::dataTableOutput(ns("tabel_diferente"))
 )
    
    
  
}
    
#' database_upload_plati Server Function
#'
#' @noRd 
mod_database_upload_plati_server <- function(input, output, session, plati_reactive) {
  ns <- session$ns
  
  # Below threshold represents the date before which I do not allow saving plati input
  threshold_date_plati_input <- as.Date("2019-12-31")
  
  slice_provizioane_plati <- readRDS("R/reactivedata/plati/slice_provizioane_plati.rds")
  
  output$diverse <- renderPrint(reactiveValuesToList(plati_reactive))
  
  vals_upload_plati <- reactiveValues(
      nume_obligatorii = c( "CodBeneficar",
        slice_provizioane_plati %>% dplyr::select(-Plata_neta,-Expunere_CTG_plati,-data_raport) %>% names() ),
      column_names_date = slice_provizioane_plati %>%
        dplyr::select(dplyr::contains("data")) %>% dplyr::select(-data_raport) %>% names() )
  
 
   # Observer for plati file input
  observeEvent(input$plati_input,{
      shiny::validate(shiny::need(any(tools::file_ext(input$plati_input$datapath) %in% c("xls", "xlsx")),
          message = paste0("Excel only, you uploaded a ",  tools::file_ext(input$plati_input$datapath)," file")))
    
   output$show_balanta_parteneri <- renderUI(
     fileInput(ns("balanta_parteneri_input"),
               width = "300px", accept = c(".xls", ".xlsx"),
               label = "Upload baza de date parteneri",
               buttonLabel = "Excel only",  placeholder = "no file uploaded")   )
    
    vals_upload_plati$file_input = input$plati_input$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive = vals_upload_plati,)
    
    
    # Below observer activates after excel module is called
    
    observe ({req(vals_upload_plati$all_names == TRUE)
      
      vals_upload_plati$file_read_prel <- vals_upload_plati$file_read_prel %>%  dplyr::filter(!is.na(DocumentId)) %>% 
        dplyr::mutate_at(.vars = 'CUI',as.numeric) %>% dplyr::mutate_at(.vars = 'Banca',
              ~ ifelse(Banca == 'BCR0001',"BCR",ifelse(Banca == '8479295','INTESA',Banca)))
      
      vals_upload_plati$data_plata_upload <- max(vals_upload_plati$file_read_prel$DataPlata1,na.rm=TRUE) %>%
        lubridate::ceiling_date(unit = "month") - 1})
    
    
    output$show_plati_date <- renderUI({req(input$plati_input, vals_upload_plati$all_names)
      if (!vals_upload_plati$all_names) {
        
        paste("Lipseste coloana: ",vals_upload_plati$missing_names,collapse = " ; ")    }
      
      
      else if(!is.na(vals_upload_plati$data_plata_upload) | length(vals_upload_plati$data_plata_upload) !=0) {
        dateInput(inputId = session$ns("data_plati_input"),width = "300px",
                  label = "Selecteaza data raportului uploadat",
                  value = vals_upload_plati$data_plata_upload,autoclose = TRUE) }
      
      else {paste("Am intampinat probleme cu coloana DataPlata1. Contacteaza administratorul!!")}
    })
    
    output$output_save_plati <- renderUI({
      fluidRow(column(width = 6,
      shinyWidgets::actionBttn(inputId = ns("plati_input_save"),icon = icon("save"),size = "sm",
                   label = "Click aici pentru a salva fisierul uploadat", style = "stretch",color = "success")),
      column(width = 6, downloadLink(outputId = ns("down_plati_upload_prelucrat"),
                                       label = "Downloadeaza fisierul uploadat prelucrat"),
             tags$head(tags$style("#database_upload_plati_ui_1-down_plati_upload_prelucrat {color: #20a7c9;}")) ) )})
    
    output$down_plati_upload_prelucrat <- downloadHandler(filename = function() {paste0("Plati_provizioane_",
              input$data_plati_input,".csv")},
              content = function(file) {
      
    vals_upload_plati$previous_date <- ifelse(input$data_plati_input %in% plati_reactive$view_sumar_plati$data_raport,
              plati_reactive$view_sumar_plati$data_raport[which(plati_reactive$view_sumar_plati$data_raport == 
                                                                  input$data_plati_input)+1],
              plati_reactive$view_sumar_plati$data_raport[1])
      
      vals_upload_plati$previous_report <- readRDS("R/reactivedata/plati/baza_provizioane_plati.rds") %>% 
        dplyr::filter(data_raport == vals_upload_plati$previous_date)
       
      readr::write_csv(x = vals_upload_plati$file_read_prel %>% 
            dplyr::left_join( vals_upload_plati$previous_report %>%
                       dplyr::select(DocumentId,ProvizionNou) %>% 
                          dplyr::rename_at(.vars = 2,  .funs = ~paste0("ProvizionActual_",
                                as.Date.numeric(vals_upload_plati$previous_date,origin = "1970-01-01"))),
                                by="DocumentId") %>%
              dplyr::bind_rows(vals_upload_plati$previous_report %>% 
                                 dplyr::filter(ProvizionNou>0, !DocumentId %in%  vals_upload_plati$file_read_prel$DocumentId) %>%
                                 dplyr::rename_at(.vars = "ProvizionNou",  .funs = ~paste0("ProvizionActual_",
                                    as.Date.numeric(vals_upload_plati$previous_date,origin = "1970-01-01"))) %>%
                                 dplyr::mutate(Plata_neta = 0,ValoareAdmisaFNG = 0, PlatiEfective=0, TotalRecuperat = 0)) %>%
              dplyr::mutate(dplyr::across(.cols = dplyr::matches("ProvizionActual_"),~ifelse(is.na(.x),0,.x))) %>%
              dplyr::select(28,2:4,1,5:10,32,11:12,29,20:22,13:19,23:27,30:31),file = file)
              })
    
    })
  
  observeEvent(input$data_plati_input,{
    
    vals_upload_plati$file_read_prel <- vals_upload_plati$file_read_prel %>% 
      dplyr::mutate(Plata_neta = PlatiEfective - TotalRecuperat, data_raport = input$data_plati_input) %>% 
      
      dplyr::mutate(Expunere_CTG_plati=ifelse(RecuperatCTG==0 & DataPlata1 > lubridate::`%m-%`(data_raport,months(24)),
                                              ProcCTG*Plata_neta/100,0))
    
    output$sumar_plati_input <- DT::renderDataTable({
      dt_generate_function(df = vals_upload_plati$file_read_prel %>% 
                             dplyr::summarise(Nr_contracte = dplyr::n(),PlatiBrute = sum(PlatiEfective), 
                                              RecuperariTotale = sum(TotalRecuperat),  Garantii_accesorii_admise =  sum(ValoareAdmisaFNG),   
                                              Expunere_CTG_plati = sum(Expunere_CTG_plati),
                                              Provizioane_Constituite = sum(ProvizionNou),  
                                              CreanteNete = PlatiBrute - RecuperariTotale,
                                              Grad_Acoperire_Provizioane =  Provizioane_Constituite/CreanteNete) %>% 
                             dplyr::mutate(data_raport=input$data_plati_input) %>% dplyr::select(9,1:8), 
                           round_col = 2:8,perc_col = 9, 
                           caption = "Sinteza fisier uploadat:") %>%  
        DT::formatDate(columns = 1,method="toLocaleDateString")  })
  })
  
  observeEvent(input$plati_input_save,{
    removeUI("#database_upload_plati_ui_1-plati_input_save")
    if (input$data_plati_input <= threshold_date_plati_input) {
      shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",showCloseButton = TRUE,
            text = "Nu pot salva fisiere anterioare datei de 01 Ianuarie 2019"   )  }
    else {
      
      baza_provizioane_plati <- readRDS("R/reactivedata/plati/baza_provizioane_plati.rds")
      # I use below reactive as input for module compare_df
      database_vals_upload_plati <- reactiveValues(df_old = baza_provizioane_plati, df_new = vals_upload_plati$file_read_prel,
               element_id = input$data_plati_input, column_id = "data_raport", finalise_process_compare_df = FALSE)
    }
      
      callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = database_vals_upload_plati)
      
      observe({req(database_vals_upload_plati$finalise_process_compare_df)
        
       plati_reactive$baza_provizioane_plati <- database_vals_upload_plati$df_new_prel
       
      })
  })
  
  observeEvent(input$balanta_parteneri_input,{
    # I remove save button since vals_upload_plati$file_read_prel will no longer exists after module read excel will be called
    removeUI("#database_upload_plati_ui_1-plati_input_save")
    
    # I isolate vals_upload_plati$file_read_prel pentru ca voi mai avea nevoie de el si dupa ce dispare in urma call-ului catre modul read excel
    fisier_plati_input <- isolate(vals_upload_plati$file_read_prel)
    
    nume_obligatorii_balanta <- c("Partener|Cod","Partener|Denumire","Sold final|Debit","Sold final|Credit")
    
    balanta_reactiv <- reactiveValues(nume_obligatorii = nume_obligatorii_balanta)
    
    balanta_reactiv$file_input <- input$balanta_parteneri_input$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive = balanta_reactiv)
    
    observe({req(balanta_reactiv$all_names==TRUE)
      balanta_reactiv$prelucrata <- balanta_reactiv$file_read_prel %>% dplyr::filter(!is.na(`Partener|Cod`)) %>%
        dplyr::mutate(Sold_debitor = `Sold final|Debit` - `Sold final|Credit`) %>%
        dplyr::group_by(`Partener|Cod`,`Partener|Denumire`) %>%
        dplyr::summarise(Sold_final_debitor = sum(Sold_debitor)) %>% dplyr::ungroup()
      
      balanta_reactiv$fisier_input_prel <- fisier_plati_input  %>% dplyr::mutate(Plata_neta = PlatiEfective - TotalRecuperat) %>%
        dplyr::group_by(CodBeneficar) %>% dplyr::summarise(Plata_neta_Charisma = sum(Plata_neta))
      
      output$tabel_diferente <- DT::renderDataTable({
        dt_generate_function(df =  balanta_reactiv$fisier_input_prel %>% 
                               dplyr::left_join(balanta_reactiv$prelucrata,  by = c("CodBeneficar" = "Partener|Cod")) %>% 
                               dplyr::mutate(Diferente = Plata_neta_Charisma - Sold_final_debitor) %>% 
                               dplyr::filter(abs(Diferente) > 1 | is.na(Diferente)) %>%
                               dplyr::bind_rows(dplyr::left_join(x =  balanta_reactiv$prelucrata, y = balanta_reactiv$fisier_input_prel,
                                                                 by = c("Partener|Cod" = "CodBeneficar")) %>% 
                                                  dplyr::mutate(Diferente = Plata_neta_Charisma - Sold_final_debitor) %>% 
                                                  dplyr::filter(abs(Diferente) > 1 | is.na(Diferente))) %>%
                               # below i remove plata_neta_charisma = 0 si beneficiar inexistent in balanta
                               dplyr::filter(!(Plata_neta_Charisma == 0 & is.na(`Partener|Cod`))) %>%
                               dplyr::select(6,3,2,4,5,1),round_col = 3:5,caption = "Diferente identificate", show_buttons=TRUE
        )
      })
      
      
    })
    
  })
  
}
    
## To be copied in the UI
# mod_database_upload_plati_ui("database_upload_plati_ui_1")
    
## To be copied in the server
# callModule(mod_database_upload_plati_server, "database_upload_plati_ui_1")
 

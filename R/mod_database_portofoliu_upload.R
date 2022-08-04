#' database_portofoliu_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_portofoliu_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
    shinyjs::useShinyjs(),
    
    fillRow(flex = c(1, NA),
      fileInput(ns("sold"),  label = "Upload fisierul de solduri",
        accept = c(".xlsx", ".xls"), buttonLabel = "Excel Only",
        placeholder = "No file uploaded", width = "300px"),
      br(),
      uiOutput(outputId = ns("date_portof_output"))  ),
    br(),
    br(),
    br(), br(),
    DT::dataTableOutput(ns("sumar_portof_input")), verbatimTextOutput(ns("diverse")), br(),
    br(),
    uiOutput(ns("calculate_depreciate_output")),
    DT::dataTableOutput(ns("coeficienti_provizioane_nonifrs")),
    br(),
    
    DT::dataTableOutput(ns("sinteza_portof_input_categorie")),
    br(),
    uiOutput(ns("calculate_provizioane_depreciate_output")),
    uiOutput(ns("save_portofoliu_output")), 
    br(),
    DT::dataTableOutput(outputId = ns("provizion_detaliat"))
  )
 
                       
}
    
#' database_portofoliu_upload Server Function
#'
#' @noRd 
mod_database_portofoliu_upload_server <- function(input, output, session,vals_portofoliu) {
  ns <- session$ns
  
 threshold_date_input <- as.Date("2019-12-31")
  
  vals_portof_upload <- reactiveValues(nume_obligatorii= c("ID Document",'Beneficiar','Cod Partener','Banca',
              'Nr contract','Valuta','Soldul garantiei [in LEI]',"Data contract",
              'Soldul creditului [in LEI]','Procentul de garantare','Tip Fond [centralizat]'),
              optional_names = c("%contragarantare", "SoldGarantieContragarantata_LEI"),
              colum_types = list("Cod Partener"="text","Data contract"="date",
                "Soldul garantiei [in LEI]"="numeric", "Procentul de garantare"="numeric","ID Document" ="numeric"))
              
  
  observeEvent(input$sold,{
    
    vals_portof_upload$file_input = input$sold$datapath
    
    mod_read_excel_server("read_excel_ui_1", excel_reactive = vals_portof_upload)
    #callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive = vals_portof_upload)
    
    # observer for when excel file does not contains mandatory column names
    observe({req(vals_portof_upload$all_names == FALSE)
      
      output$date_portof_output <- renderText({
        paste0("STOP, nu am gasit coloanele: ",paste(vals_portof_upload$missing_names,collapse = "; ") %>% stringr::str_c()) })
      })
    
    # Below observer activates after excel module is called and the excel contains all the mandatory column names
    observe({ req(vals_portof_upload$all_names == TRUE)
      
      # Check to see if I have the column SoldGarantieContragarantata_LEI or %contragarantare
      exista_proc_ctg <- reactive ({ "%contragarantare" %in% names( vals_portof_upload$file_read_prel ) })
      exista_sold_ctg <- reactive ({ "SoldGarantieContragarantata_LEI" %in% names( vals_portof_upload$file_read_prel ) })
      
      nu_exista_ctg <- reactive ({ if(!exista_sold_ctg() & !exista_proc_ctg()) {TRUE} 
        else {FALSE}   })
      
      sunt_nume_lipsa <- reactive ({ vals_portof_upload$nume_obligatorii %in% 
          names( vals_portof_upload$file_read_prel ) %>%     all() })
      
      nume_lipsa <- reactive ({ setdiff(vals_portof_upload$nume_obligatorii,
                                        names( vals_portof_upload$file_read_prel )) })
      
      
      
      date_upload_file <- reactive({shiny::validate(shiny::need(tools::file_ext(input$sold$datapath)=="xlsx",
                            message = paste0("XLSX only! You uploaded a ",tools::file_ext(input$sold$datapath)," file")))
        
        shiny::validate(shiny::need(sunt_nume_lipsa(),message = paste0("Lipseste coloana: ",
                                            nume_lipsa(),collapse = ";") %>% stringr::str_c()))
        
        max( vals_portof_upload$file_read_prel$`Data contract`, na.rm = TRUE)    })
      
     
      # I process portfolio for contragarantii
      portofoliu_contragarantii <- reactive ({ 
        if (exista_sold_ctg()) { vals_portof_upload$file_read_prel %>% 
            dplyr::mutate(contragarantii = SoldGarantieContragarantata_LEI) %>% tidyr::replace_na(list(contragarantii=0)) }
        
        else if (exista_proc_ctg()) { vals_portof_upload$file_read_prel %>% 
            dplyr::mutate(contragarantii=`%contragarantare` * `Soldul garantiei [in LEI]`/100) %>% 
                    tidyr::replace_na(list(contragarantii=0)) }
        
        else { vals_portof_upload$file_read_prel %>% dplyr::mutate(contragarantii=0) }
        
       })
      
      output$date_portof_output <- renderUI({req(date_upload_file())
        
        shinyWidgets::airDatepickerInput(ns('date_portof_input'),label = "Selecteaza data portofoliului uploadat",
                  value = date_upload_file(),minDate = as.Date("2001-12-31"),maxDate = as.Date("2030-12-31"),
                  autoClose = TRUE,language = "ro", width = "320px")  })
      
     
      portofoliu_surse_proprii <- reactive({  req(portofoliu_contragarantii())
        portofoliu_contragarantii() %>% 
          dplyr::filter(`Tip Fond [centralizat]` == sort(unique(portofoliu_contragarantii()$`Tip Fond [centralizat]`))[1]) })
      
      duplicated_portofoliu_id <- reactive({ req( portofoliu_surse_proprii())
        if ( length(which(duplicated(portofoliu_surse_proprii()$`ID Document`)==TRUE))>0) {"TRUE" }
          else {"FALSE"}
        })
      
      sumar_portofoliu <- reactive({ req(portofoliu_surse_proprii(), duplicated_portofoliu_id())
        switch(EXPR = duplicated_portofoliu_id(),
               "TRUE" = portofoliu_surse_proprii() %>% dplyr::filter(`ID Document` %in% 
                  portofoliu_surse_proprii()$`ID Document`[which(duplicated(portofoliu_surse_proprii()$`ID Document`)==TRUE)]),
               "FALSE" = portofoliu_surse_proprii() %>% dplyr::summarise(Nr_contracte = dplyr::n(),
                        Sold_garantii = sum(`Soldul garantiei [in LEI]`),   Sold_contragarantii=sum(contragarantii)) %>% 
                        dplyr::mutate(Data_raport_uploadat = input$date_portof_input) %>% 
                          dplyr::select(4,1,2,3)) })
      
      output$sumar_portof_input <- DT::renderDataTable({req(input$date_portof_input)
        
        dt_generate_function( df = sumar_portofoliu(), show_buttons=TRUE,
            caption = switch(EXPR = duplicated_portofoliu_id(), "TRUE" = "STOP, am gasit contractele duplicate de mai jos.
              Verifica si uploadeaza fisierul corect", "FALSE" = "Sinteza fisier uploadat"),
            round_col = switch(EXPR = duplicated_portofoliu_id(), "TRUE" = c(7,9,10,12,13),
                                           "FALSE" = 2:4)) 
      })
          
      
      output$calculate_depreciate_output <- renderUI({req(portofoliu_surse_proprii())
        shinyWidgets::actionBttn(ns("calculate_depreciate"),style = "stretch",color = "success",size = "md",
          label = "Calculeaza garantiile depreciate",icon = icon("chart-area") )   })
   
      observeEvent(input$calculate_depreciate,{
        
        bi_sinteza <- readRDS("R/reactivedata/bi_sinteza.rds")
        
        removeUI("#database_portofoliu_upload_ui_1-calculate_depreciate")
        
        shinyjs::disable(id = "sold")
        
        if ( any(is.na(portofoliu_surse_proprii()$`ID Document`))) {
          output$sinteza_portof_input_categorie <- DT::renderDataTable({req(portofoliu_surse_proprii())
            dt_generate_function(df = portofoliu_surse_proprii() %>% 
                        dplyr::filter(is.na(`ID Document`)),
          caption="STOP, nu am gasit ID Document pentru tabelul de mai jos. Uploadeaza fisierul corect.",
          round_col = c(7,9,10,12,13))      })
        }
        
        else if (any(is.na(portofoliu_surse_proprii()$`Cod Partener`))) {
          output$sinteza_portof_input_categorie <- DT::renderDataTable({req(portofoliu_surse_proprii())
            dt_generate_function(df = portofoliu_surse_proprii() %>% dplyr::filter(is.na(`Cod Partener`)),
              caption="STOP, nu am gasit Cod Partener pentru tabelul de mai jos. Uploadeaza fisierul corect.",
              round_col = c(7,9,10,12,13)) })
        }
        
        else if (bi_sinteza$Snapshot_cereri_plata <= input$date_portof_input) {
          shinyWidgets::sendSweetAlert(session = session,title = "STOP, nu am actualizat BI-ul cu cereri de plata",type = "error",showCloseButton = TRUE,
              text = paste0("Eu am snapshot-ul ",as.character(bi_sinteza$Snapshot_cereri_plata)," Actualizeaza BI-ul cereri de plata
              in fereastra de mai jos si apoi incearca din nou pentru a putea genera categoriile depreciate!"))  }
        
        else if (bi_sinteza$Snapshot_instiintari_neplata <= input$date_portof_input) {
          shinyWidgets::sendSweetAlert(session = session,title = "STOP,Nu am actualizat BI-ul instiintari de neplata pentru data raportului selectata
                        de tine",type = "error", showCloseButton = TRUE,
                                       text = paste0("Eu am snapshot-ul ",as.character(bi_sinteza$Snapshot_instiintari_neplata),
                                                     " Actualizeaza BI-ul cu instiintari de neplata in fereastra de mai jos si apoi incearca din nou pentru a putea genera categoriile depreciate!")) }
        
        else {
          baza_cereri_plata <- readRDS("R/reactivedata/bi_cereri_plata.rds")
          baza_instiintari <- readRDS("R/reactivedata/bi_instiintari.rds")
          baza_insolventa <- readRDS("R/reactivedata/insolventa.rds")
          
          portofoliu_cereri_plata <- reactive({ portofoliu_surse_proprii() %>% dplyr::left_join(y = baza_cereri_plata, 
                                                                                          by = 'ID Document') })
          portofoliu_instiintari <- reactive({portofoliu_cereri_plata() %>% dplyr::left_join(y = baza_instiintari,
                                                                                             by = 'ID Document') })
          portofoliu_prelucrat <- reactive({portofoliu_instiintari() %>% dplyr::left_join(y = baza_insolventa, 
                                                                                  by=c('Cod Partener'='Cod'))  })
          
          
          
          if (max(baza_insolventa$DataInsolventa) < input$date_portof_input) {
            shinyWidgets::sendSweetAlert(session=session,title = "ATENTIE",type = "warning",showCloseButton = TRUE,
            text = paste0("Atentie, data maxima de insolventa pe care o am stocata ,", as.character(max(baza_insolventa$DataInsolventa)),
            ", este mai mica decat data raportului selectata de tine Acesta este doar un mesaj de atentionare, voi merge mai departe 
            u prelucrarea portofoliului. Poti updatat fisierul de insolvente in ferereastra de mai jos.")) } 
          
          
          portofoliu_categorie <- eventReactive( input$date_portof_input,{
            portofoliu_prelucrat() %>% dplyr::mutate(anul_de_raportare = input$date_portof_input) %>% 
              dplyr::group_by(`Cod Partener`, Beneficiar,anul_de_raportare) %>% 
              dplyr::summarise(expunere = sum(`Soldul garantiei [in LEI]`), contragarantii = sum(contragarantii),
                               min_cerere = ifelse(all(is.na(data_cerere_plata)), NA, min(data_cerere_plata, na.rm = TRUE)),
                               min_insolventa = ifelse(all(is.na(DataInsolventa)), NA, min(DataInsolventa, na.rm = TRUE)),
                               min_instiintare = ifelse(all(is.na(data_instiintare)), NA, min(data_instiintare, na.rm = TRUE))) %>%
              dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("min_")),  as.Date.numeric, origin = "1970-01-01") %>% as.data.frame() %>%
              dplyr::mutate(categorie_contaminata = ifelse(!is.na(min_cerere) &  min_cerere <= anul_de_raportare,"cerere_plata",
                  ifelse(!is.na(min_insolventa) & min_insolventa <= anul_de_raportare,"insolventa",
                  ifelse(!is.na(min_instiintare) & min_instiintare <= anul_de_raportare,"instiintare_neplata",  "standard")  )) ) })
          
          output$sinteza_portof_input_categorie <- DT::renderDataTable({req(portofoliu_categorie())
              dt_generate_function(df = portofoliu_categorie() %>% dplyr::group_by(categorie_contaminata) %>% 
                                   dplyr::summarise(expunere=sum(expunere), contragarantii=sum(contragarantii)) %>% 
                                   dplyr::bind_rows(apply(X = dplyr::select(.,-1),MARGIN = 2,FUN=sum)) %>% 
                                   tidyr::replace_na(replace = list(categorie_contaminata="Total")),
                                   show_buttons=TRUE,
                                 caption = paste0("Sinteza portofoliu depreciat la data de ",
                                                  input$date_portof_input," :"),round_col = 2:3)   })
          
          
          
      output$calculate_provizioane_depreciate_output <- renderUI({
            fluidRow(column(width = 6,
            shinyWidgets::actionBttn(ns("calculate_provizioane_depreciate"), icon = icon("calculator"),
                         label = "Calculeaza provizioanele depreciate",style = "stretch",color = "success",size = "md")),
            
            column(width = 6, shinyWidgets::downloadBttn(ns("down_portof_depreciat"),style = "stretch",
                    color = "success",size = "md", label = "Download portofoliul depreciat"))  )
          })
        
      output$down_portof_depreciat <- downloadHandler(filename = function()  {paste0("portof_depreciat_",
                    input$date_portof_input,".csv")},content = function(file) {
                      readr::write_csv(x = portofoliu_categorie(), file = file)   })
      updateActionButton(session = session, inputId = "calculate_depreciate")
      
        }
        
      observeEvent(input$calculate_provizioane_depreciate,{
          
          removeUI("#database_portofoliu_upload_ui_1-calculate_provizioane_depreciate")
          removeUI("#database_portofoliu_upload_ui_1-down_portof_depreciat_bttn")
        
        vals_portof_upload$coef_non_ifrs <- readRDS(file = "R/reactivedata/ifrs/istoric_coef_provizionare.rds") %>%
          dplyr::filter(FromDate <= input$date_portof_input, ToDate >= input$date_portof_input) %>%
          dplyr::select(-FromDate, -ToDate) %>% dplyr::slice(1)
        
        output$coeficienti_provizioane_nonifrs <- DT::renderDataTable({
          dt_generate_function(df = vals_portof_upload$coef_non_ifrs,editable="cell",round_col = 1:5,digits = 2,
                               caption = "Coeficientii de mai jos sunt folositi pentru calculul provizioanelor. Dublu-clik in oricare din celule 
              entru a edita coeficientul iar valoarea provizonului se va actualiza corespunzator si in tabelul de mai sus.")  })
        
        output$save_portofoliu_output <- renderUI({
            fluidRow(
              column(width = 5, shinyWidgets::downloadBttn( ns("down_portof_proviz"),style = "stretch",color = "success",
                                                            label = "Download portofoliul provizionat",size = "md")),
              column(width = 3,
                  shinyWidgets::prettyToggle(inputId = ns("link_portofoliu"),value = FALSE,icon_off = icon("plus"),
                      icon_on = icon("redo-alt"),
                      label_off = "Click to show portofoliul detaliat", label_on = "Click to hide portofoliul detaliat")),
              column(width = 4,
                         shinyWidgets::actionBttn(inputId = session$ns("save_portofoliu"),label = "Salveaza portofoliul prelucrat",
                              icon = icon("save"),style = "stretch",color = "success", size = "md"))    
               )
              })
          
          
          
          portofoliu_provizion <- reactive({ dplyr::left_join( x = portofoliu_prelucrat(),
            y = dplyr::select( portofoliu_categorie(), 'Cod Partener', categorie_contaminata,
              anul_de_raportare ),    by = "Cod Partener" ) %>%
              dplyr::select(  DocumentId = 'ID Document',   anul_de_raportare,
                Banca, Beneficiar, 'Cod Partener',  'Nr contract',
                expunere = 'Soldul garantiei [in LEI]',   contragarantii,   categorie_contaminata ) %>% 
              dplyr::mutate(provizion_contabil = ifelse(
                categorie_contaminata == "cerere_plata",
                (expunere - contragarantii * vals_portof_upload$coef_non_ifrs$Ajustare_ctg) * 
                  vals_portof_upload$coef_non_ifrs$Coef_trans_cereri_plata_plati * 
                  vals_portof_upload$coef_non_ifrs$Coef_provizionare_plati,
                ifelse(categorie_contaminata == "insolventa",
                       (expunere - contragarantii * vals_portof_upload$coef_non_ifrs$Ajustare_ctg) * 
                         vals_portof_upload$coef_non_ifrs$Coef_trans_cereri_plata_insolvente * 
                  vals_portof_upload$coef_non_ifrs$Coef_trans_cereri_plata_plati * 
                    vals_portof_upload$coef_non_ifrs$Coef_provizionare_plati,
                       ifelse(categorie_contaminata == "instiintare_neplata",
                              (expunere - contragarantii * vals_portof_upload$coef_non_ifrs$Ajustare_ctg) * 
                                vals_portof_upload$coef_non_ifrs$Coef_trans_cereri_plata_instiintari * 
                  vals_portof_upload$coef_non_ifrs$Coef_trans_cereri_plata_plati * 
                    vals_portof_upload$coef_non_ifrs$Coef_provizionare_plati,
                              0)))) })
          
          output$down_portof_proviz <- downloadHandler(filename = function() {paste0("portof_provizionat_",
            input$date_portof_input,".csv")},content = function(file) {
              readr::write_csv(x = portofoliu_provizion(),file = file)      })
          
          output$provizion_detaliat <- DT::renderDataTable({req(input$link_portofoliu)
            DT::datatable(data = portofoliu_provizion(),rownames = FALSE,options = list(pageLength=5, dom = "ftp"),
                          editable = list(target='cell',
                 disable=list(columns=0:7)),caption = "Double click in ultimele 2 coloane pentru a modifica. 
                      Foloseste search. Atentie, apasand butonul save vei salva valorile completate ale provizionului") %>%
              DT::formatRound(columns = c(7,8,10), digits = 2)  })
          
          portofoliu_provizion_updatat <- eventReactive(input[['provizion_detaliat_cell_edit']],{
            DT::editData(portofoliu_provizion(), info=input[['provizion_detaliat_cell_edit']],rownames = FALSE) })
          
          output$sinteza_portof_input_categorie <- DT::renderDataTable({
             dt_generate_function(caption = paste0("Sinteza portofoliu depreciat si provizioane la data de ",input$date_portof_input," :"),
                       show_buttons = T, round_col = 2:4,  df = portofoliu_provizion() %>% dplyr::group_by(categorie_contaminata) %>% 
                                   dplyr::summarise(expunere=sum(expunere),contragarantii=sum(contragarantii),provizion=sum(provizion_contabil)) %>% 
                                   dplyr::bind_rows(apply(X = dplyr::select(.,-1),MARGIN = 2,FUN=sum)) %>% 
                                   tidyr::replace_na(replace = list(categorie_contaminata="Total")))    })
          updateActionButton(session = session, inputId = "calculate_provizioane_depreciate")
          
          observeEvent(input$save_portofoliu,{
            
              portof_database <- readRDS('R/external_volumes/portofoliu/portof_database.rds')

              
              removeUI("#database_portofoliu_upload_ui_1-save_portofoliu")
              removeUI("#database_portofoliu_upload_ui_1-sumar_portof_input")
              removeUI("#database_portofoliu_upload_ui_1-coeficienti_provizioane_nonifrs")
              removeUI("#database_portofoliu_upload_ui_1-date_portof_input")
              removeUI("#database_portofoliu_upload_ui_1-provizion_detaliat")
              removeUI("#database_portofoliu_upload_ui_1-link_portofoliu")
              
              database_portofoliu_reactive <- reactiveValues(df_old = portof_database, 
                        df_new = portofoliu_provizion(),
                element_id = input$date_portof_input, column_id = "anul_de_raportare", finalise_process_compare_df = FALSE)
              
              callModule(mod_compare_df_server, "compare_df_ui_1", df_reactive = database_portofoliu_reactive)
              
              observe({req(database_portofoliu_reactive$finalise_process_compare_df)
                vals_portofoliu$portof_database <- database_portofoliu_reactive$df_new_prel
                                          })
              updateActionButton(session = session, inputId = "save_portofoliu")
          })
      })
      
      })
    
  })
    
  })
  
  # Observer for editing coeficienti
  observeEvent(eventExpr = input[['coeficienti_provizioane_nonifrs_cell_edit']],{
    cellinfo <-  input[['coeficienti_provizioane_nonifrs_cell_edit']]
    vals_portof_upload$coef_non_ifrs <- DT::editData(data = vals_portof_upload$coef_non_ifrs, info=cellinfo,rownames = FALSE) 
    })
   
  
}
    
## To be copied in the UI
# mod_database_portofoliu_upload_ui("database_portofoliu_upload_ui_1")
    
## To be copied in the server
# callModule(mod_database_portofoliu_upload_server, "database_portofoliu_upload_ui_1")
 

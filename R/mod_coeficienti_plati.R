#' coeficienti_plati UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_coeficienti_plati_ui <- function(id){
  ns <- NS(id)
  tagList(       shinyFeedback::useShinyFeedback(),
    
    bs4Dash::box(title = "Data raport plati",width = 12,collapsible = TRUE,status = "danger",collapsed = FALSE,
                 icon = icon("calendar-check"), footer = "Aici se selecteaza data de la care 
                 se calculeaza coeficientii de mai jos. Data initiala reprezinta data la care se selecteaza valoarea creantelor nete, 
                 a expunerii din contragarantii sau a valorii admise in garantie a accesoriilor iar data finala reprezinta
                 data pana la care se extrag recuperarile aferente indicatorilor astfel selectati.",
                 fluidRow(
                 column(width = 4,
                 shinyWidgets::airDatepickerInput(ns("data_initiala_plati"),value = as.Date("2019-12-31"),
                            label = "Data initiala a raportului", width = "300px",autoClose = TRUE)),
                 
                 column(width =4, shinyWidgets::airDatepickerInput(ns("data_finala_plati"),value = as.Date("2020-12-31"),
                          label = "Data finala a raportului", width = "300px",autoClose = TRUE)),
                 
                 column(width = 4, div( style = "padding-top: 27px; margin-left:25px",
                   downloadButton(outputId = ns("down_sumar_creante"),
                                                 label = "Download detaliat recuperari")) )
                 )   ),
    
    
    bs4Dash::box(title = "Grad anual de recuperare creante nete",status = "info",
                 
                 width = 12,collapsible = TRUE,collapsed = FALSE,
                 fluidRow(     
                        column(width = 6,DT::dataTableOutput(ns("recuperari_accesorii"))),
                        
                        column(width = 6, DT::dataTableOutput(ns("recuperari_ctg"))),
                        
                        column(width = 12, DT::dataTableOutput(ns("recuperari_creante")))
                  )  )
    
    # ,bs4Dash::box(title = "Grad recuperare expunere CTG",status = "info",width = 12,collapsible = TRUE,collapsed = TRUE,
    #              footer = "Aici se produce acelasi indicator de recuperare a expunerii din contragarantare aferente platilor - 
    #              similar cu tabelul de mai sus. Diferenta consta in faptul ca recuperarile sunt preluate din fisierul FRC si,
    #              teoretic pot fi mai mari decat recuperarile prelucrate din baza de date a provizioanelor (sold RecuperatCTG).",
    #              fluidRow(       
    #              column(width = 8, verbatimTextOutput(ns("mesaj_data_ctg"))),
    #              column(width = 4, shinyWidgets::actionBttn(ns("show_ctg_upload"), size = "md",
    #                   label = "Click aici pentru a updata FRC",style = "stretch",color = "success",icon = icon("upload"))),
    #              column(width = 12, hr(),
    #                     DT::dataTableOutput(ns("sumar_recuperari_ctg")), br()),
    #                    
    #                     column(width=12,downloadButton(outputId = ns("down_recuperare_ctg"),
    #                                     label = "Download detaliat"))))
  )
  
}
    
#' coeficienti_plati Server Function
#'
#' @noRd 
mod_coeficienti_plati_server <- function(input, output, session, plati_reactive){
  # Server processing for box coeficienti plati
  ns <- session$ns
  
  baza_frc <- readRDS(file = "R/reactivedata/plati/incasari_frc.rds")
  
  vals_coef_plati <- reactiveValues( baza_frc = baza_frc )
  
  observeEvent(plati_reactive,{
    
    # Procesare simulare coeficienti
  
    if ( is.null(plati_reactive$baza_provizioane_plati) ) {
      baza_provizioane_plati <- readRDS("R/reactivedata/plati/baza_provizioane_plati.rds") %>%
        dplyr::select(DocumentId,NrCTR,Banca,Beneficiar,CUI,PlatiEfective,TotalRecuperat,
                      RecuperatFond,RecuperatCTG,ValoareAdmisaFNG,ProvizionNou,Plata_neta,Expunere_CTG_plati,data_raport) } else {
      baza_provizioane_plati <- isolate(plati_reactive$baza_provizioane_plati) %>%
        dplyr::select(DocumentId,NrCTR,Banca,Beneficiar,CUI,PlatiEfective,TotalRecuperat,
                      RecuperatFond,RecuperatCTG, ValoareAdmisaFNG,ProvizionNou,Plata_neta,Expunere_CTG_plati, data_raport) }
    
  
    baza_selectata <- reactive({ baza_provizioane_plati %>% 
      dplyr::filter(data_raport == as.Date(input$data_initiala_plati)) }) #names of baza splitata are unique values of data_raport
  
  
  
 
  # Procesare grad de recuperare
  
  baza_splitata <- split(x = baza_provizioane_plati,f = baza_provizioane_plati$data_raport) %>% 
                                  purrr::map(.f = ~dplyr::as_tibble(x=.x))
  
  # I have to use for loop. Arguments of type.x within purrr::map and dplyr::rename_with are the same
  for (i in 1:length(baza_splitata)) {
    baza_splitata[[i]] <- dplyr::rename_with(.data = baza_splitata[[i]],
          .cols = dplyr::matches("ValoareAdmisaFNG|RecuperatFond|Expunere_CTG_plati|RecuperatCTG|Plata_neta|TotalRecuperat"),
                .fn = ~paste0(.x,'_',baza_splitata[[i]]$data_raport[1]))
  }
  
  
  
  baza_selectata <- reactive({ baza_splitata[which(names(baza_splitata) == input$data_initiala_plati)][[1]]  })
  
  baza_splitata_filtrata <- reactive({ baza_splitata[which(names(baza_splitata) > input$data_initiala_plati & 
                          names(baza_splitata) <= input$data_finala_plati ) ] %>% 
      purrr::map(~dplyr::select(.data = .x,"DocumentId",
    dplyr::matches("ValoareAdmisaFNG|RecuperatFond|Expunere_CTG_plati|RecuperatCTG|Plata_neta|TotalRecuperat"))) })
  
  baza_plati_nete_brut <- reactive({ 
    
    purrr::map(.x = baza_splitata_filtrata(),~dplyr::left_join(x = dplyr::select( baza_selectata(),DocumentId,
    dplyr::matches("ValoareAdmisaFNG|RecuperatFond|Expunere_CTG_plati|RecuperatCTG|Plata_neta|TotalRecuperat")),
          y =.x, by="DocumentId")) })
 
  
  
  baza_plati_nete <- reactive({ 
    shiny::validate(shiny::need(length(baza_plati_nete_brut())>0,message = "Nu pot calcula grad de recuperare pentru cea mai recenta data selectata"))
    cbind(dplyr::select(.data = baza_selectata(),"DocumentId",
                dplyr::matches("ValoareAdmisaFNG|RecuperatFond|Expunere_CTG_plati|RecuperatCTG|Plata_neta|TotalRecuperat")), # preiau 2 coloane, DocumentId si creantele brute la data de referinta
          purrr::map_dfc(.x = baza_plati_nete_brut(),.f = ~cbind(dplyr::select(.x, -c(1:7))))) %>%  # adaug restul creantelor dupa data de referinta
      dplyr::mutate_all(~tidyr::replace_na(data = .,replace = 0)) })  # inlocuiesc valorile NA cu zero - creanta recuperata in totalitate
  
  valoare_admisa_fng_data_selectata <- reactive({ baza_selectata() %>% dplyr::select(dplyr::starts_with("ValoareAdmisaFNG")) %>%
      dplyr::pull() %>% sum(na.rm = TRUE) })
  
  recuperari_fng_data_selectata <- reactive({ baza_selectata() %>% dplyr::select(dplyr::starts_with("RecuperatFond")) %>%
      dplyr::pull() %>% sum(na.rm = TRUE) })
  
  recuperari_totale_data_selectata <- reactive({ baza_selectata() %>% dplyr::select(dplyr::starts_with("TotalRecuperat")) %>%
      dplyr::pull() %>% sum(na.rm = TRUE) })
  
  creante_nete_data_selectata <- reactive({ baza_selectata() %>% dplyr::select(dplyr::starts_with("Plata_neta")) %>%
      dplyr::pull() %>% sum(na.rm = TRUE) })
  
  recuperari_FRC_data_selectata <- reactive({ baza_selectata() %>% dplyr::select(dplyr::starts_with("RecuperatCTG")) %>%
      dplyr::pull() %>% sum(na.rm = TRUE) })
  
  expunere_CTG_data_selectata <- reactive({ baza_selectata() %>% dplyr::select(dplyr::starts_with("Expunere_CTG_plati")) %>%
      dplyr::pull() %>% sum(na.rm = TRUE) })
  
  data_maxima_provizioane <- reactive({ min(max(baza_provizioane_plati$data_raport, na.rm = T), input$data_finala_plati) })
  
  recuperari_creante <- reactive({ baza_plati_nete() %>% 
      dplyr::select(dplyr::matches("Plata_neta|TotalRecuperat")) %>%
      dplyr::summarise_all(.funs = ~ sum(.)) %>% 
      tidyr::pivot_longer(cols = dplyr::matches("Plata_neta|TotalRecuperat")) %>% 
      dplyr::slice(stringr::str_which(string = name, pattern = "TotalRecuperat")) %>%
      setNames(nm = c("Recuperari_Totale", "Sume_cumulate_recuperate")) %>%
      dplyr::mutate(Sume_cumulate_recuperate = Sume_cumulate_recuperate - recuperari_totale_data_selectata()) %>%
      dplyr::mutate(Grad_recuperare_cumulat = Sume_cumulate_recuperate/creante_nete_data_selectata())
    
  })
  
  caption_recuperari_creante <-  reactive({  paste( "Valoare creantelor nete la data de  ",
              as.character(input$data_initiala_plati), "in valoare de ",
              formatC(  creante_nete_data_selectata(),  digits = 0,   big.mark = ",",   format = "f"  ),
              " lei a fost recuperata pana la data de ", as.character(data_maxima_provizioane())," intr-un procent cumulat de ",
      round(recuperari_creante()$Grad_recuperare_cumulat[nrow(recuperari_creante())], 4) * 100,  "%")  })
  
  output$recuperari_creante <- DT::renderDataTable( { dt_generate_function(df = recuperari_creante(),
          round_col = 2,perc_col = 3,caption = caption_recuperari_creante(),dom="tp",pageLength=5, show_buttons=TRUE)  } )
  
  recuperari_ctg <- reactive({ req( baza_plati_nete(), recuperari_FRC_data_selectata(),expunere_CTG_data_selectata() )
    baza_plati_nete() %>% 
      dplyr::select(dplyr::matches("Expunere_CTG_plati|RecuperatCTG")) %>%
      dplyr::summarise_all(.funs = ~ sum(.)) %>% 
      tidyr::pivot_longer(cols = dplyr::matches("Expunere_CTG_plati|RecuperatCTG")) %>% 
      dplyr::slice(stringr::str_which(string = name, pattern = "RecuperatCTG")) %>%
      setNames(nm = c("Recuperari_FRC", "Sume_cumulate_recuperate")) %>%
      dplyr::mutate(Sume_cumulate_recuperate = Sume_cumulate_recuperate - recuperari_FRC_data_selectata()) %>%
      dplyr::mutate(Grad_recuperare_cumulat = Sume_cumulate_recuperate/expunere_CTG_data_selectata())
    
  })
  
  caption_recuperari_FRC <-  reactive({  paste( "Valoare expunerii din contragarantii aferente platilor (Expunere_CTG_plati) la data de  ",
          as.character(input$data_initiala_plati), "in valoare de ",
          formatC(  expunere_CTG_data_selectata(),  digits = 0,   big.mark = ",",   format = "f"  ),
    " lei a fost recuperata pana la data de ", as.character(data_maxima_provizioane())," intr-un procent cumulat de ",
    round( recuperari_ctg()$Grad_recuperare_cumulat[nrow(recuperari_ctg())], 4) * 100,  "%")  })
  
 
  
  output$recuperari_ctg <- DT::renderDataTable( { dt_generate_function(df = recuperari_ctg(),
      round_col = 2,perc_col = 3,caption = caption_recuperari_FRC(),dom="tp",pageLength=5, show_buttons=TRUE)  } )
  
  recuperari_accesorii <- reactive({ baza_plati_nete() %>% 
      dplyr::select(dplyr::matches("ValoareAdmisaFNG|RecuperatFond")) %>%
      dplyr::summarise_all(.funs = ~ sum(.)) %>% 
           tidyr::pivot_longer(cols = dplyr::matches("ValoareAdmisaFNG|RecuperatFond")) %>% 
           dplyr::slice(stringr::str_which(string = name, pattern = "Recuperat")) %>%
           setNames(nm = c("Recuperari_accesorii", "Sume_cumulate_recuperate")) %>%
           dplyr::mutate(Sume_cumulate_recuperate = Sume_cumulate_recuperate - recuperari_fng_data_selectata()) %>%
           dplyr::mutate(Grad_recuperare_cumulat = Sume_cumulate_recuperate/valoare_admisa_fng_data_selectata())
  })
  
  
  caption_recuperari_accesorii <-  reactive({  paste( "Valoare admisa a garantiilor accesorii de catre FNG(ValoareAdmisaFNG) la data de  ",
        as.character(input$data_initiala_plati), "in valoare de ",
        formatC(  valoare_admisa_fng_data_selectata(),  digits = 0,   big.mark = ",",   format = "f"  ),
        " lei a fost recuperata pana la data de ",as.character(data_maxima_provizioane())," intr-un procent cumulat de ",
        round(recuperari_accesorii()$Grad_recuperare_cumulat[nrow(recuperari_accesorii())], 4) * 100,  "%")  })
                
  output$recuperari_accesorii <-  DT::renderDataTable( { dt_generate_function(df = recuperari_accesorii(),
          round_col = 2,perc_col = 3,caption = caption_recuperari_accesorii(),dom="tp",pageLength=5, show_buttons=TRUE)  } )
  
  output$down_sumar_creante <- downloadHandler(filename = function(){"creante_recuperate.csv"},content = function(file){
    readr::write_csv(x = baza_plati_nete(),file = file) } )
  
  
  # FRC grad de recuperare - not usable anymore
  
  
  # output$mesaj_data_ctg <- renderText({ req( vals_coef_plati$baza_frc )
  #   paste0( "Data maxima a recuperarilor FRC este de ", max( vals_coef_plati$baza_frc$`Data incasarii`, na.rm=T)) })
  # 
  # observeEvent(input$show_ctg_upload, {
  #   showModal(   modalDialog(  title = "Actualizeaza recuperarile FRC. Atentie, fisierul uploadat il salvez ca atare, nu
  #                       verific daca sunt mai multe date decat ce am stocat.",
  #       size = "l", footer = modalButton("Close",   icon = icon("xmark")),
  #       fluidRow(
  #         column(   width = 4,
  #           fileInput(  inputId = ns("input_frc"),  label = "Upload FRC file",
  #             accept = ".xlsx",  buttonLabel = "Excel only",  placeholder = "No file uploaded"),
  #           textOutput(ns("mesaj_upload_frc")) ),
  #         column(  width = 8,     br(),
  #           div(style = "padding-left: 70px; padding-top: 5px;",
  #                  shinyWidgets::downloadBttn( ns("link_frc"),   size = "md", style = "stretch",  color = "success",
  #                   label = "Downloadeaza modelul de fisier FRC")) )
  #       )   )   )
  #   
  #   output$link_frc <- downloadHandler(  filename = function() { "incasari_frc.xlsx" },
  #       content = function(file) {  file.copy(from = "R/reactivedata/plati/incasari_frc.xlsx", to = file) }   )
  # })
  # 
  # observeEvent(input$input_frc,{
  #   shiny::validate(shiny::need(expr = tools::file_ext(input$input_frc$datapath) == "xlsx",
  #       message = "Please upload only XLSX files"))
  #   
  #   vals_coef_plati$frc_read <- readxl::read_excel("R/reactivedata/plati/incasari_frc.xlsx",sheet = "Sheet1",
  #                      col_types = c("text","date","numeric","numeric")) %>% 
  #     dplyr::mutate(dplyr::across(.cols = dplyr::starts_with('Data'), ~as.Date(.x)))
  #   
  #   if (janitor::compare_df_cols_same( vals_coef_plati$frc_read, vals_coef_plati$baza_frc)) {
  #     vals_coef_plati$baza_frc <- vals_coef_plati$frc_read
  #     shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
  #             .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
  #     removeModal(session)    }
  #   
  #   else {  shinyFeedback::showToast(type = "error",title = "ERROR",message = "NU am putut salva. Check with your administrator",
  #                           keepVisible = TRUE )    }
  #   
  #   })
  # 
  # 
  # baza_selectata_frc <- reactive({ dplyr::left_join(baza_selectata(),y = vals_coef_plati$baza_frc[,-1],
  #     by=c("DocumentId"="Contract Id")) %>% 
  #     tidyr::replace_na(list(`Suma incasata FRC (RON)`=0)) })
  # 
  # browser()
  # 
  # sumar_recuperari_ctg <- reactive({  req(input$data_initiala_plati %in% plati_reactive$view_sumar_plati$data_raport) 
  #   baza_selectata_frc() %>% 
  #     dplyr::filter(Expunere_CTG_plati>0,!is.na(`Data incasarii`)) %>% 
  #     dplyr::filter(`Data incasarii` > input$data_initiala_plati) %>%
  #     dplyr::group_by(Anul_incasarii_FRC=lubridate::year(`Data incasarii`)) %>% 
  #     dplyr::summarise(Suma_incasata_FRC=sum(`Suma incasata FRC (RON)`)) %>% 
  #     dplyr::mutate(Grad_anual_recuperare=Suma_incasata_FRC/sum(baza_selectata()$Expunere_CTG_plati)) %>% 
  #     dplyr::mutate(Grad_cumulat_recuperare=cumsum(Grad_anual_recuperare))  })
  # 
  # caption_recuperari_frc <- reactive({    paste( "Valoarea expunerii CTG din plati",  "la data de ",
  #       as.character(input$data_initiala_plati),  " in valoare de ",formatC(format = "f", 
  #         sum(baza_selectata()$Expunere_CTG_plati, na.rm = T),  digits = 0, big.mark = ","  ),
  #       " a fost recuperata dupa cum se prezinta in tabelul de mai jos. Ultima data de actualizare a incasarilor FRC este  ",
  #       as.character(max(vals_coef_plati$baza_frc$`Data incasarii`, na.rm = TRUE)) )     })
  # 
  # output$sumar_recuperari_ctg <- DT::renderDataTable({ 
  #   shiny::validate( shiny::need(input$data_initiala_plati %in% plati_reactive$view_sumar_plati$data_raport,
  #               message = "Nu detin plati nete la data selectata. Data selectata trebuie sa fie de obicei final de luna. Uita-te in baza de date a platilor pentru a vedea toate datele rapoartelor de plati pe care le am.") )
  #  
  #   dt_generate_function(df = sumar_recuperari_ctg(),caption = caption_recuperari_frc(), show_buttons=TRUE,
  #                        round_col = 2, perc_col=3:4, digits_perc = 1,digits = 0)
  #   })
  # 
  # output$down_recuperare_ctg <-  downloadHandler(filename = function(){"recuperare_ctg.csv"},content = function(file){
  #   readr::write_csv(x = baza_selectata_frc() %>% dplyr::filter(Expunere_CTG_plati>0,!is.na(`Data incasarii`)),file = file) } )
  # 
})
  
  }
    
## To be copied in the UI
# mod_coeficienti_plati_ui("coeficienti_plati_ui_1")
    
## To be copied in the server
# callModule(mod_coeficienti_plati_server, "coeficienti_plati_ui_1")
 

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
  tagList(
    bs4Dash::box(title = "Data raport plati",width = 12,collapsible = TRUE,status = "danger",collapsed = T,
                 icon = icon("calendar-check"), footer = "Aici se selecteaza data de la care 
                 se calculeaza coeficientii de mai jos.",
                 shinyWidgets::airDatepickerInput(ns("data_raport_plati"),value = as.Date("2019-12-31"),
                            label = "Selecteaza de aici data raportului", width = "300px",autoClose = TRUE)),
    
    bs4Dash::box(title = "Sensibilitate provizioane plati",status = "info",
                 collapsible = T,collapsed = T,width = 12, icon = icon("calculator"),
                 footer = "Aici se calculeaza senzitivitatea provizioanelor specifice in functie de
                 coeficientii editabili de mai sus.",
                        column(width = 12, fillRow(flex = c(1, NA), numericInput(inputId = ns("coef_garantii_plati"), 
                            value = 0.25,width = '500px',
                            label = "Coeficient ajustare garantii accesorii-simulare" ),
                            numericInput(inputId = ns("coef_ctg_plati"),value = 0, width='500px',
                            label = "Coeficient ajustare expunere din contragarantii-simulare")),
                               hr(),  DT::dataTableOutput(ns("sumar_simulare")))),
    
    bs4Dash::box(title = "Grad anual de recuperare creante nete",status = "info",
                 width = 12,collapsible = T,collapsed = T,
                        column(width = 12,DT::dataTableOutput(ns("sumar_creante"))),
                        column(width=12,br()),
                        column(width = 6,downloadButton(outputId = ns("down_sumar_creante"),
                                label = "Download detaliat creante recuperate"))),
    
    bs4Dash::box(title = "Grad recuperare expunere CTG",status = "info",width = 12,collapsible = T,collapsed = F,
                        column(width = 12,DT::dataTableOutput(ns("sumar_recuperari_ctg"))),
                        column(width=12,br()),
                        column(width=12,downloadButton(outputId = ns("down_recuperare_ctg"),
                                        label = "Download detaliat"))))
  
}
    
#' coeficienti_plati Server Function
#'
#' @noRd 
mod_coeficienti_plati_server <- function(input, output, session, plati_reactive){
  # Server processing for box coeficienti plati
  ns <- session$ns
  
  observeEvent(plati_reactive,{
  
    if (is.null(plati_reactive$baza_provizioane_plati)) {
      baza_provizioane_plati <- readRDS("R/reactivedata/plati/baza_provizioane_plati.rds") } else {
      baza_provizioane_plati <- isolate(plati_reactive$baza_provizioane_plati) }
    
  
    baza_selectata <- reactive({ baza_provizioane_plati %>% 
      dplyr::filter(data_raport == as.Date(input$data_raport_plati)) }) #names of baza splitata are unique values of data_raport
  
  
  baza_selectata_simulata <- reactive({ baza_selectata() %>% 
      dplyr::mutate(Provizion_Simulat = pmax(PlatiEfective - TotalRecuperat - 
        input$coef_garantii_plati*ValoareAdmisaFNG - input$coef_ctg_plati*Expunere_CTG_plati,0)) })
  
  
  sumar_simulare <- reactive({ baza_selectata_simulata() %>% dplyr::summarise(Creante_Nete = sum(PlatiEfective) - sum(TotalRecuperat), 
          ProvizionContabil = sum(ProvizionNou),Provizion_Simulat = sum(Provizion_Simulat),
          Grad_acoperire_provizioane_simulat = Provizion_Simulat/Creante_Nete) %>%
      dplyr::mutate(Impact_Provizion_Simulat = .[[3]] - .[[2]]) %>% dplyr::select(1:3,5,4) %>% 
      dplyr::rename_at(.vars = 1:5,~paste0(.,"_",input$data_raport_plati)) })
  
  output$sumar_simulare <- DT::renderDataTable({
    dt_generate_function(df=sumar_simulare(),caption = "Provizioane rezultate in urma 
        simularii. Selectati valorile de mai sus si se va actualiza valoarea corespunzatoare a provizionului simulat mai jos:",
                         round_col = 1:4,perc_col = 5) })
  
  
  
  baza_splitata <- split(x = baza_provizioane_plati,f = baza_provizioane_plati$data_raport) %>% 
                                  purrr::map(.f = ~dplyr::as_tibble(x=.x))
  
  # I have to use for loop. Arguments of type.x within purrr::map and dplyr::rename_with are the same
  for (i in 1:length(baza_splitata)) {
    baza_splitata[[i]] <- dplyr::rename_with(.data = baza_splitata[[i]],.cols = dplyr::matches("Plata_neta|RecuperatCTG"),
                .fn = ~paste0(.x,'_',baza_splitata[[i]]$data_raport[1]))
  }
  
  
  
  baza_selectata <- reactive({ baza_splitata[which(names(baza_splitata) == input$data_raport_plati)][[1]]  })
  
  baza_splitata_filtrata <- reactive({ baza_splitata[which(names(baza_splitata) > input$data_raport_plati) ] %>% 
      purrr::map(~dplyr::select(.data = .x,"DocumentId",dplyr::matches("Plata_neta|RecuperatCTG"))) })
  
  baza_plati_nete_brut <- reactive({ 
    
    purrr::map(.x = baza_splitata_filtrata(),~dplyr::left_join(x = dplyr::select(baza_selectata(),
                      DocumentId,dplyr::matches("Plata_neta|RecuperatCTG")),y =.x, by="DocumentId")) })
 
  baza_plati_nete <- reactive({ 
    shiny::validate(shiny::need(length(baza_plati_nete_brut())>0,message = "Nu pot calcula grad de recuperare pentru cea mai recenta data selectata"))
    cbind(dplyr::select(.data = baza_selectata(),"DocumentId",dplyr::matches("Plata_neta|RecuperatCTG")), # preiau 2 coloane, DocumentId si creantele brute la data de referinta
          purrr::map_dfc(.x = baza_plati_nete_brut(),.f = ~cbind(dplyr::select(.x,-1,-2)))) %>%  # adaug restul creantelor dupa data de referinta
      dplyr::mutate_all(~tidyr::replace_na(data = .,replace = 0)) })  # inlocuiesc valorile NA cu zero - creanta recuperata in totalitate
  
 
  output_recuperari_plati <- reactive({ 
    baza_plati_nete() %>% dplyr::summarise_all(.funs = ~sum(.)) %>% dplyr::select(-DocumentId) %>% 
      tidyr::pivot_longer(cols = dplyr::matches("Plata_neta|RecuperatCTG")) %>% 
      setNames(nm=c("Indicator","Valoare_creanta_neta", "Recuperari FRC")) %>% 
      dplyr::mutate(Recuperari_brute=-1*(Valoare_creanta_neta-Valoare_creanta_neta[1])) %>%
      dplyr::mutate(Grad_recuperare_cumulat=Recuperari_brute/Valoare_creanta_neta[1])  })
  
  caption_baza_plati_nete <- reactive({ paste("Creantele nete la data de ",as.character(input$data_raport_plati),"in valoare de ",
                                              formatC(sum(baza_plati_nete()[,2]),digits = 0,big.mark = ",",format="f"),
                                              " lei au fost recuperate intr-un procent cumulat de ",
                                              round(output_recuperari_plati()[nrow(output_recuperari_plati()),4]*100,1),"%")  })
  
  output$sumar_creante <- DT::renderDataTable({
      DT::datatable(data = output_recuperari_plati(), caption = caption_baza_plati_nete(), extensions = "Buttons",rownames = FALSE,
                options = list(dom = "Btp", buttons = c("excel", "csv"))) %>% DT::formatRound(2:3,digits = 0) %>% 
                        DT::formatPercentage(4,digits = 2)
    })
  
  output$down_sumar_creante <- downloadHandler(filename = function(){"creante_recuperate.csv"},content = function(file){
    readr::write_csv(x = baza_plati_nete(),file = file) } )
  
  
  # FRC grad de recuperare
  
  frc <- readxl::read_excel("R/reactivedata/plati/incasari_frc.xlsx",col_types = c("numeric","text","date","numeric"))
  frc$`Data incasarii` <- as.Date(frc$`Data incasarii`)
  
  baza_selectata_frc <- reactive({ dplyr::left_join(baza_selectata(),y = frc[,-2],by="DocumentId") %>% 
      tidyr::replace_na(list(`Suma incasata FRC (RON)`=0)) })
  
  sumar_recuperari_ctg <- reactive({  req(input$data_raport_plati %in% plati_reactive$view_sumar_plati$data_raport) 
    baza_selectata_frc() %>% 
      dplyr::filter(Expunere_CTG_plati>0,!is.na(`Data incasarii`)) %>% 
      dplyr::filter(`Data incasarii` > input$data_raport_plati) %>%
      dplyr::group_by(Anul_incasarii_FRC=lubridate::year(`Data incasarii`)) %>% 
      dplyr::summarise(Suma_incasata_FRC=sum(`Suma incasata FRC (RON)`)) %>% 
      dplyr::mutate(Grad_anual_recuperare=Suma_incasata_FRC/sum(baza_selectata()$Expunere_CTG_plati)) %>% 
      dplyr::mutate(Grad_cumulat_recuperare=cumsum(Grad_anual_recuperare))  })
  
  caption_recuperari_frc <- reactive({paste("Valoarea expunerii CTG din plati","la data de ",as.character(input$data_raport_plati),
                                            " in valoare de ", formatC(sum(baza_selectata()$Expunere_CTG_plati,na.rm = T), format="f",digits = 0,big.mark = ","),
                                            " a fost recuperata dupa cum se prezinta in tabelul de mai jos. Ultima data de actualizare a incasarilor FRC este  ",
                                            as.character(max(frc$`Data incasarii`,na.rm = TRUE))) })
  output$sumar_recuperari_ctg <- DT::renderDataTable({ 
    shiny::validate( shiny::need(input$data_raport_plati %in% plati_reactive$view_sumar_plati$data_raport,
                message = "Nu detin plati nete la data selectata. Data selectata trebuie sa fie de obicei final de luna. Uita-te in baza de date a platilor pentru a vedea toate datele rapoartelor de plati pe care le am.") )
   
    dt_generate_function(df = sumar_recuperari_ctg(),caption = caption_recuperari_frc(), show_buttons=TRUE,
                         round_col = 2, perc_col=3:4, digits_perc = 1,digits = 0)
    })
  
  output$down_recuperare_ctg <-  downloadHandler(filename = function(){"recuperare_ctg.csv"},content = function(file){
    readr::write_csv(x = baza_selectata_frc() %>% dplyr::filter(Expunere_CTG_plati>0,!is.na(`Data incasarii`)),file = file) } )
  
})
  
  }
    
## To be copied in the UI
# mod_coeficienti_plati_ui("coeficienti_plati_ui_1")
    
## To be copied in the server
# callModule(mod_coeficienti_plati_server, "coeficienti_plati_ui_1")
 

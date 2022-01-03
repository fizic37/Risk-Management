#' raportare_bnr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_raportare_bnr_ui <- function(id){
  ns <- NS(id)
  
  shinydashboard::tabBox(width = 12,tabPanel(title = "Upload files",icon = icon("upload"),
    fluidRow(
    shinydashboard::box(title = "Upload BI acordari",width = 12,status = "success",
        footer = "Se downloadeaza modelul de BI folosind link-ul, se actualizeaza corespunzator snapshot-ul si se filtreaza
                             luna de raportare. Fisierul astfel salvat se uploadeaza folosind butonul dedicat.",
    fluidRow(      
        column(width = 6,fileInput(inputId = ns("bi_pc_input"),accept = c(".xlsx",".xls"),width = "300px",
                  label = "Upload BI acordari",buttonLabel = "Excel only",placeholder = "no file uploaded")),
        column(width = 6, br(),downloadLink(outputId = ns("link_bi_pc_acordari"), 
          label = "Downloadeaza modelul de BI acordari Prima Casa"))
    ),
    DT::dataTableOutput(ns("bi_prelucrat"))),
    
    shinydashboard::box(title = "Upload Sold Prima Casa",width = 12,status = "success",
          footer = "Se uploadeaza pivot-ul soldului de garantii Prima Casa folosind link-ul furnizat.
          Pivotul se construieste pornind de la baza de date a soldului PC, 
          Cod Finantator pe rows, TipGarantie pe rows, soldul creditului, a garantiei si numarul de contracte in 
          values, display pe classic, repeat all item labels.",    
    fluidRow(
      column(width = 6, fileInput(inputId = ns("sold_pc_pivot_input"),accept = c(".xlsx",".xls"),width = "300px",
        label = "Upload pivot sold PC",buttonLabel = "Excel only",placeholder = "no file uploaded")),
      column(width = 6, br(),downloadLink(ns("link_pivot_sold"),
              label = "Click aici pentru a downloada modelul de pivot"))
      #,column(width = 4, br(),downloadLink(ns("link_portofoliu_sold"),
              #label = "Click aici pentru a downloada portofoliul si modelul de pivot (cca. 70 mb)"))
    ),
    
    DT::dataTableOutput(outputId = ns("sold_pc_prelucrat"))))),
    
    tabPanel(title = "Corespondenta Banci",icon = icon("check-circle"),
             DT::dataTableOutput(ns("coresp_banci_bi")), hr(),
             DT::dataTableOutput(ns("coresp_banci_sold"))))
  
}
    
#' raportare_bnr Server Function
#'
#' @noRd 
mod_raportare_bnr_server <- function(input, output, session){
  
  ns <- session$ns
  #library(magrittr)
  load("data/coresp_banci.rda")
  load("data/coresp_banci_bi.rda")
  
  output$coresp_banci_bi <- DT::renderDataTable({DT::datatable(data = coresp_banci_bi,rownames = FALSE,
        extensions = "Buttons", caption = "Corespondenta banci raportare BI garantii acordate PC:",
      options = list(dom = "Bftp",buttons = c("excel","csv"),pageLength=7))})
  
  output$coresp_banci_sold <- DT::renderDataTable({DT::datatable(data = coresp_banci,rownames = FALSE,
        extensions = "Buttons", caption = "Corespondenta banci raportare sold PC catre BNR:",
                options = list(dom = "Bftp", buttons = c("excel", "csv"), pageLength = 7)) })
  
  output$link_bi_pc_acordari <- downloadHandler(filename = "bi_acordari_pc.xlsx",content = function(file) {
    file.copy(from = "inst/extdata/prima_casa/bi_acordari.xlsx",to = file)   })
  
  output$link_pivot_sold <- downloadHandler(filename = "sold_pc.xlsx",content = function(file) {
    file.copy(from = "inst/extdata/prima_casa/pivot_sold.xlsx",to = file)   })
  
  
  #output$link_portofoliu_sold <- downloadHandler(filename = "sold_pc_pivot.xlsx",content = function(file) {
   # file.copy(from = "inst/extdata/prima_casa/model_portofoliu_pc.xlsx",to = file)   })
  
  sketch = htmltools::withTags(table(class = 'display',
    thead(tr(
        th(rowspan = 2, 'Finantator'),
        th(colspan = 3, 'Contract de garantare'),
        th(colspan = 3, 'Promisiune de garantare')),
      tr(
        lapply(c('Sold credit (LEI)','Sold garantie (LEI)','Numar contracte',
                 'Sold credit (LEI)','Sold garantie (LEI)','Numar promisiuni'), th) ) ) ))
  
  sketch_bi = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Finantator'),
        th(colspan = 3, 'Contract de garantare'),
        th(colspan = 3, 'Promisiune de garantare')
      ),
      tr(lapply(c('Numar Solicitari','Finantare RON','Garantie RON',
                 'Numar Solicitari','Finantare RON','Garantie RON'), th)))))
  
 
  
  observeEvent(input$sold_pc_pivot_input,{
    
    nume_obligatorii_sold <- c("Cod Finantator", "TipGarantie", "Sum of Sold Credit (Lei)", "Sum of Sold Garantie (Lei)",
                               "Count of Cod Partener")
    
    sold_reactiv <- reactiveValues(nume_obligatorii=nume_obligatorii_sold)
    
    sold_reactiv$file_input <-  input$sold_pc_pivot_input$datapath
    
    callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive=sold_reactiv)
    
    observe({req(sold_reactiv$all_names==TRUE)
      sold_reactiv$fisier_prelucrat <- sold_reactiv$file_read_prel %>% 
        dplyr::filter(`Cod Finantator` != "Grand Total") %>% 
        dplyr::left_join(coresp_banci, by = c("Cod Finantator" = "Banca_Generat_Charisma")) %>% 
        dplyr::select(-`Cod Finantator`) %>%
        tidyr::pivot_wider(names_from = TipGarantie, values_from = c(`Sum of Sold Credit (Lei)`,
                                                                     `Sum of Sold Garantie (Lei)`,`Count of Cod Partener`),values_fn = sum,values_fill=0) %>% 
        dplyr::select(-dplyr::contains("NA")) %>% 
        dplyr::group_by(Banca_Raport_BNR) %>% dplyr::summarise_all(.funs = ~sum(.)) %>% 
        dplyr::rename_at(.vars = 1,.funs = ~c("Finantator")) %>%
        dplyr::select(Finantator,dplyr::contains("CG"),dplyr::contains("PG")) %>%
        dplyr::arrange(desc(`Sum of Sold Credit (Lei)_CG`)) %>%
        janitor::adorn_totals(where = "row",na.rm = TRUE,name = "Total") 
      
      output$sold_pc_prelucrat <- DT::renderDataTable({req(sold_reactiv$fisier_prelucrat)
        DT::datatable(data = sold_reactiv$fisier_prelucrat,rownames = FALSE,
            options = list(dom = "Bt",pageLength=(nrow(sold_reactiv$fisier_prelucrat)+1),
                  buttons=c("excel","csv")), container = sketch,extensions = "Buttons",
            caption = "Sold Prima Casa octombrie 2020") %>% DT::formatRound(columns = 2:7,digits = 0) 
        })
      
      #output$diverse <- renderPrint({req(sold_reactiv$fisier_prelucrat)
       # str(sold_reactiv$fisier_prelucrat) })
    })
    
  
    
   })
  
  observeEvent(input$bi_pc_input,{
    
    nume_obligatorii_bi <- c("Nume Banca", "TipDocument", "Numar Solicitari", "Finantare RON","Garantie RON")
    
    bi_reactiv <- reactiveValues(nume_obligatorii=nume_obligatorii_bi)
    
    bi_reactiv$file_input <-  input$bi_pc_input$datapath
    
    #callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive = bi_reactiv)
    mod_read_excel_server("read_excel_ui_1", excel_reactive = bi_reactiv)
   
    observe({ req(bi_reactiv$all_names==TRUE)
      bi_reactiv$fisier_prelucrat <- bi_reactiv$file_read_prel %>% dplyr::filter(`Nume Banca` != "Grand Total") %>% 
        dplyr::left_join(coresp_banci_bi, by = c("Nume Banca" = "Banca_BI")) %>% 
        dplyr::select(-`Nume Banca`) %>%
        dplyr::mutate(TipDocument = ifelse(stringr::str_detect(TipDocument,pattern = "Promisiune"),
                                           "PG","CG")) %>%
        tidyr::pivot_wider(names_from = TipDocument, values_from = c(`Numar Solicitari`,`Finantare RON`,
              `Garantie RON`),values_fn = sum,values_fill=0) %>% 
        dplyr::select(-dplyr::contains("_NA")) %>% 
        dplyr::group_by(Banca_Raport_BNR) %>% dplyr::summarise_all(.funs = ~sum(.)) %>% 
        dplyr::rename_at(.vars = 1,.funs = ~c("Finantator")) %>%
        dplyr::select(Finantator,dplyr::contains("CG"),dplyr::contains("PG")) %>% 
        dplyr::arrange(desc(`Garantie RON_CG`)) %>%
        janitor::adorn_totals(where = "row",na.rm = TRUE,name = "Total") 
      
      output$bi_prelucrat <- DT::renderDataTable({req(bi_reactiv$fisier_prelucrat)
        DT::datatable(data = bi_reactiv$fisier_prelucrat,rownames = FALSE,
                      options = list(dom = "Bt",pageLength=(nrow(bi_reactiv$fisier_prelucrat)+1),
              buttons=c("excel","csv")), container = sketch_bi,extensions = "Buttons",
                      caption = "Garantii acordate Prima Casa") %>% DT::formatRound(columns = 2:7,digits = 0)  })
      
      })
    
  })
    
  
                                

 
}
    
## To be copied in the UI
# mod_raportare_bnr_ui("raportare_bnr_ui_1")
    
## To be copied in the server
# callModule(mod_raportare_bnr_server, "raportare_bnr_ui_1")
 

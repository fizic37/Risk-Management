#' calibrare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_calibrare_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
    
    shinyFeedback::useShinyFeedback(),
    
    bs4Dash::box(title = "Data calibrarii", icon = icon("calendar-alt"),width = 12,
                 footer = "Se selecteaza data la care se realizeaza calibrarea. Aceasta reprezinta data la care 
                 se va extrage portofoliul de garantii pentru a se verifica evolutia acestuia si a veniturilor in decursul a 3 ani.",
                 collapsible = T,collapsed = T,maximizable = T,status = "danger",
                 
                 fluidRow( column(width = 4,
                 shinyWidgets::airMonthpickerInput(inputId = ns("data_calibrare"),label = "Data calibrarii",
                                                   value = Sys.Date()) ),
                 column(width = 8, br(), shinyWidgets::actionBttn(inputId = ns("check_data"),color = "success",
                                  label = "Check data availability",icon = icon("tasks"),style = "stretch")
                 ) ) ),
    
    bs4Dash::box(title = "Upload venituri file", icon = icon("file-invoice-dollar"),
                 status = "info",width = 12,collapsible = T,collapsed = F,maximizable = T,
                 footer = "Se uploadeaza fisierul din Charisma-Garantare-Rapoarte-Monitorizare-Raport 
                 verificare incasare comisioane in perioada", 
                 fluidRow(
                   column(width = 4, fileInput(inputId = ns("venituri_input"),buttonLabel = "Excel only",
                           label = "Uploadeaza fisierul de venituri",accept = ".xlsx",placeholder = "Nothing uploaded" ) ),
                   
                   column(width = 8, uiOutput(ns("show_upload_results"))),
                   
                   column(width = 12, DT::dataTableOutput(outputId = ns("table_results")))
                 ))
 
  )
}
    
#' calibrare Server Functions
#'
#' @noRd 
mod_calibrare_server <- function(id){
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    nume_obligatorii <- c("Contract Id","Nr. contract garantare",    "Tip incasare comision",
                                 "% Comision",  "Comision acordat","Comision incasat",   "Tip plata",  "Conventia")
    
    vals_venituri <- reactiveValues(nume_obligatorii = nume_obligatorii)
    
    observeEvent(input$check_data, { req( input$data_calibrare )
      vals_venituri$data_calibrare <- input$data_calibrare %>% lubridate::`%m+%`(months(1))-1
      
      view_baza_date_crc <- readRDS(file = "R/reactivedata/crc/view_baza_date_crc.rds")
      view_cip_database <- readRDS(file = "R/reactivedata/cip/view_cip_database.rds")
      view_portofoliu <- readRDS(file = "R/reactivedata/portofoliu/view1_portofoliu.rds")
      
      
      
      if( !vals_venituri$data_calibrare %in%  view_baza_date_crc$`Data situatie risc global`) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",btn_colors = "#c92052",
                                     text = "Nu detin datele CRC necesare. Please Upload CRC file!")
        bs4Dash::updateTabItems(session = session,inputId = "sidebar_ui_1-tabs",selected = 'crc')
        
      }
      else if ( !vals_venituri$data_calibrare %in%  view_cip_database$data_raport) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",btn_colors = "#c92052",
                                     text = "Nu detin datele CIP necesare. Please Upload CIP file!")
        bs4Dash::updateTabItems(session = session,
                                inputId = "sidebar_ui_1-tabs",selected = 'crc') }
      
      else if ( !vals_venituri$data_calibrare %in%  view_portofoliu$anul_de_raportare ) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",btn_colors = "#c92052",
          text = "Nu detin portofoliul de garantii la data filtrata. Please Upload portfolio file!")
        bs4Dash::updateTabItems(session = session,
                                inputId = "sidebar_ui_1-tabs",selected = 'solduri')
      }
      
      else {
        vals_venituri$crc <- readRDS(file = 'R/reactivedata/crc/baza_date_crc.rds') %>%
          dplyr::filter(`Data situatie risc global` == vals_venituri$data_calibrare ) %>%
          dplyr::group_by(`Cod debitor`, `Acronim persoana declaranta`) %>%
          dplyr::summarise()
        vals_venituri$portofoliu <- readRDS("R/reactivedata/portofoliu/portof_database.rds") %>%
          dplyr::filter( anul_de_raportare == vals_venituri$data_calibrare ) %>%
          dplyr::group_by(`Cod Partener`,Beneficiar,categorie_contaminata) %>% dplyr::summarise(expunere=sum(expunere),
                      contragarantii=sum(contragarantii)) %>% dplyr::ungroup()
       
        
        
      }
      
    })
    
    observeEvent(input$venituri_input,{
      
      vals_venituri$file_input <-  input$venituri_input$datapath
      
      
      callModule(mod_read_excel_server, "read_excel_ui_1",excel_reactive = vals_venituri)
      
      # observer for when excel file does not contains mandatory column names
      observe({req(vals_venituri$all_names == FALSE)
        output$show_upload_results <- renderUI({
          h5(paste0("STOP, nu am gasit coloanele: ",paste(vals_venituri$missing_names,collapse = "; ") %>% stringr::str_c()) ) })
      })
      
      # Below observer activates after excel module is called and the excel contains all the mandatory column names
      observe({ req(vals_venituri$all_names == TRUE)
       
        venituri_final <- reactive( {
          
         conventii <- readxl::read_excel("R/reactivedata/venituri/tipologie_conventii.xlsx", sheet = "bi_conventii", skip = 3) %>%
            dplyr::mutate(conventie_venituri = stringr::str_c(`Cod conventie`, " - ", `Lista conventii`))
         
         venituri_tip_conventie <- dplyr::left_join(vals_venituri$file_read_prel, conventii[, 3:4],by = c("Conventia" = "conventie_venituri")) %>%
           dplyr::mutate(`Tip conventie` = ifelse(stringr::str_detect(string = `Nr. contract garantare`,pattern = "OUG43."),"OUG43",
                 ifelse( stringr::str_detect(string =  `Nr. contract garantare`,pattern = "CGP_OPT."),"Conventie OPTIMM",
                    ifelse( stringr::str_detect(string =  `Nr. contract garantare`,pattern = "PM."),"Conventie Prima Masina (OUG66)",
                         ifelse(`Nr. contract garantare` == "70", "Conventie expresa", `Tip conventie`))) ) )
          
          tipologie_conventii <-  readxl::read_excel("R/reactivedata/venituri/tipologie_conventii.xlsx", sheet = "tipologie")
          
          venituri_tip_sursa <- dplyr::left_join(venituri_tip_conventie, tipologie_conventii, by = c("Tip conventie" = "Tip conventii")) %>% 
            dplyr::mutate(comision_facturat_efectiv = ifelse(`Tip plata`=="VFS" & !is.na(`Tip plata`), 
                                    `Comision acordat`+`Comision incasat`, `Comision acordat`))
          
          return(venituri_tip_sursa)
        })
        
        
        output$show_upload_results <- renderUI( div(style="display:inline-block;margin-left: 40%;padding-top: 35px;font-size: 1rem;",
          shinyWidgets::prettyToggle(inputId = ns("show_detailed_results"),
              value = FALSE,label_on = "Show less of venituri results",label_off = "Show a more detailed venituri results",
              icon_off = icon("angle-double-right"),icon_on = icon("angle-left"),status_off = "info",status_on = "success")) )
        
        
        view_results <- reactive ( { req( venituri_final() )
         if (is.null(input$show_detailed_results) || input$show_detailed_results==FALSE) {
           vals_venituri$round_col <- 2
           return(       
           venituri_final() %>%  dplyr::group_by(`Tip sursa`) %>% 
                   dplyr::summarise(Venituri_incasate = sum(comision_facturat_efectiv)) %>%
                   dplyr::arrange(desc(`Tip sursa`), desc(Venituri_incasate)) )
          
           }
          
        else if (input$show_detailed_results==TRUE) { 
          vals_venituri$round_col <- 3
          return(
          venituri_final() %>%  dplyr::group_by(`Tip sursa`, `Tip conventie`) %>% 
                   dplyr::summarise(Venituri_incasate = sum(comision_facturat_efectiv)) %>%
                   dplyr::arrange(desc(`Tip sursa`), desc(Venituri_incasate)) )
          }
          
        })
       
        browser()
        
        output$table_results <- DT::renderDataTable( { req(  view_results(), vals_venituri$round_col )
          
          dt_generate_function(df =  view_results(), show_buttons=TRUE, round_col = vals_venituri$round_col,
                               caption = "Sinteza venituri file uploadat") })
        
      })
        
    })
  })
}
    
## To be copied in the UI
# mod_calibrare_ui("calibrare_ui_1")
    
## To be copied in the server
# mod_calibrare_server("calibrare_ui_1")

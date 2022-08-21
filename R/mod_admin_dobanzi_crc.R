#' admin_dobanzi_crc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_dobanzi_crc_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(
    shinyFeedback::useShinyFeedback(),
    
    column(width = 5, br(),shinyWidgets::actionBttn(ns("save_dobanzi"),style = "stretch",color = "success",
                        icon=icon("save"), label = "Salveaza modificarile efectuate mai jos")),
    
    column(width = 3, shinyWidgets::airMonthpickerInput(inputId = ns("data_dobanzi"),
        label = "Data la care arat dobanzile",value = Sys.Date())),
    
    column(width = 4),
  
    column(width = 12, actionButton(inputId = ns("set_date"), label = "Set date", 
        style = "color: #3cbcad; border-color: #fff;", icon = icon('calendar-check')), br()),
  
    column(width = 12, DT::dataTableOutput(ns("baza_dobanzi")) )
  )
}
    
#' admin_dobanzi_crc Server Functions
#'
#' @noRd 
mod_admin_dobanzi_crc_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rata_dobanzii <- readRDS("R/reactivedata/crc/rata_dobanzii.rds") 
    
    vals_admin_dobanzi <- reactiveValues()
    
    view_baza_date_crc <- readRDS("R/reactivedata/crc/view_baza_date_crc.rds")
    
    observeEvent(input$data_dobanzi,{
      vals_admin_dobanzi$data_dobanzi <- max(rata_dobanzii$Data[which(rata_dobanzii$Data<=input$data_dobanzi)])
        
      vals_admin_dobanzi$rata_dobanzii <- rata_dobanzii %>% dplyr::filter(Data==vals_admin_dobanzi$data_dobanzi) %>%
            dplyr::filter( !MONEDA %in% c("CHF","NOK")) %>%
        dplyr::select(Data, MONEDA, `Termen acordare`, `Rata dobanzii`)
      })
    
    
    output$baza_dobanzi <- DT::renderDataTable( { req( vals_admin_dobanzi$rata_dobanzii )
      dt_generate_function( df = vals_admin_dobanzi$rata_dobanzii, perc_col=4,  
          editable = list(target="cell", disable=list(columns=c(1,2))),
            #"cell", #list(target="row", disable=list(columns=c(1,2))), # This does not work for editable option
          show_buttons=TRUE,digits_perc=2,
        caption = "Baza de date a dobanzilor. Dublu click pentru a actualiza. 
        Dobanzile folosite pentru CHF si NOK vor fi cele ale EUR."  )  } )
    
    # Observer for editing baza de date
    observeEvent(eventExpr = input[['baza_dobanzi_cell_edit']],{
      cellinfo <-  input[['baza_dobanzi_cell_edit']]
      vals_admin_dobanzi$rata_dobanzii <- DT::editData(data = vals_admin_dobanzi$rata_dobanzii, 
                              info=cellinfo,rownames = FALSE)  })
    
    observeEvent(input$save_dobanzi, {
      shinyWidgets::ask_confirmation(ns("confirm_save"),
              title = "Confirmi modificarile?",
        text = "Esti sigur ca vrei sa salvezi modificarile efectuate?",
        btn_labels = c("NU, renunta","OK, salveaza"),btn_colors = c("#ff007b","#00ff84"),type = "info") 
    })
    
    observeEvent(input$confirm_save,{
      
      if ( length(unique(vals_admin_dobanzi$rata_dobanzii$Data)) !=1 ) {
        shinyWidgets::sendSweetAlert(session = session,
              title = "STOP",type = "warning",btn_labels = "OK",btn_colors = "#ff007b",
              text = "Data completata in tabel trebuie sa fie aceeasi pentru toate inregistrarile" )
      } else if ( unique(vals_admin_dobanzi$rata_dobanzii$Data) <= 
                max(view_baza_date_crc$`Data situatie risc global`) ) {
        shinyWidgets::sendSweetAlert(session = session,
          title = "STOP",type = "warning",btn_labels = "OK",btn_colors = "#ff007b",
          text = "In baza de date CRC am salvate informatii cu o data mai mare sau egala decat ce ai completat
          tu in tabel. Sterge aceste informatii din baza de date CRC pentru a putea sa te las sa salvezi." )
      }
      
      else if ( janitor::compare_df_cols_same(vals_admin_dobanzi$rata_dobanzii,rata_dobanzii,bind_method = "rbind") ) {
      
      
      vals_admin_dobanzi$final <- vals_admin_dobanzi$rata_dobanzii  %>% dplyr::bind_rows(
        vals_admin_dobanzi$rata_dobanzii  %>% dplyr::filter(MONEDA == "EUR") %>% dplyr::mutate(dplyr::across(
                 .cols = MONEDA, .fns = ~ stringr::str_replace_all(string = .x, pattern = "EUR", "NOK") )) ) %>%
             dplyr::bind_rows( vals_admin_dobanzi$rata_dobanzii  %>% dplyr::filter(MONEDA == "EUR") %>% 
                  dplyr::mutate(dplyr::across(.cols = MONEDA, .fns = ~ stringr::str_replace_all(string = .x,
                                                              pattern = "EUR", "CHF") )) )
        
      
      vals_admin_dobanzi$final <- rbind( vals_admin_dobanzi$final, 
                rata_dobanzii %>% dplyr::filter(Data != vals_admin_dobanzi$final$Data[1] ) )
      
      
      saveRDS( object =  vals_admin_dobanzi$final, file = "R/reactivedata/crc/rata_dobanzii.rds")
      
        shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database",
      .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
        
      }
        
        
      
      
    })
    
    observeEvent(input$set_date, {
      showModal(modalDialog(title = "De aici poti sa setezi data noilor dobanzi",size = "l",
    footer = list( div(style = "padding-right: 100px;padding-top: 10px;",
    "Selecteaza o data si aceasta va deveni data noilor dobanzi fara a trebui sa mai completezi
    fiecare linie din tabel"),
    actionButton(ns("save_new_date"),label = 'Salveaza', icon=icon("save")),
    modalButton(label = "Close", icon = icon("times")) ),
    shinyWidgets::airMonthpickerInput(inputId = ns("set_new_date"),minDate=as.Date("2022-08-01"),
                  label = "Selecteaza luna de inceput",value=Sys.Date())
      ) )
      if (input$set_date == 0) {shinyFeedback::showFeedbackWarning(inputId = "set_date",
        text = "De aici poti sa setezi data noilor modificari",color = "success",session = session)}
      
      else {shinyFeedback::hideFeedback(inputId = "set_date",session = session)}
      
    } )
    
    observeEvent(input$save_new_date,{
      vals_admin_dobanzi$rata_dobanzii$Data = input$set_new_date
      removeModal(session = session)
    })
 
  })
}
    
## To be copied in the UI
# mod_admin_dobanzi_crc_ui("admin_dobanzi_crc_1")
    
## To be copied in the server
# mod_admin_dobanzi_crc_server("admin_dobanzi_crc_1")

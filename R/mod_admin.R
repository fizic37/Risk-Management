#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinybusy::add_busy_spinner(color = "#c92052", position = "bottom-right", timeout = 200),
    shinyFeedback::useShinyFeedback(),
    
    bs4Dash::box( title="Coeficienti de provizionare garantii depreciate",
                 status = "info",width = 12, collapsible = T, collapsed = T,
                 maximizable = TRUE, icon = icon("square-root-alt"),
                 fluidRow( column( width =4,
                 
                shinyWidgets::airDatepickerInput(ns("data_coeficienti"),label = "Selecteaza data coeficientilor",
                            value = Sys.Date(),autoClose = TRUE,language = "ro", width = "250px") ),
                 
                 column(width = 3, br(), actionButton(inputId = ns("new_coeficienti"), label = "Add new coefficients", 
                                                      style = "color: #3cbcad; border-color: #fff;", icon = icon('plus'))),
                 column(width = 5, br(),shinyWidgets::actionBttn(inputId = ns("down_baza_coeficienti"),
                            label = "Downloadeaza istoricul coeficientilor",style = "stretch",color = "success",
                            icon = icon("download"),size = "sm")) ,
    
                  column(width = 12, DT::dataTableOutput(ns("coeficienti_depreciate")))
                 )  ),
    
    bs4Dash::box(title = "Scenarii de evolutie macroeconomica", status = "info",width = 12, collapsible = T, collapsed = T,
                 maximizable = TRUE, icon = icon("chart-area"),
                 fluidRow( column( width = 4,
                 tagList(shinyWidgets::airDatepickerInput(ns("data_scenarii"),label = "Selecteaza data scenariilor",
                    value = Sys.Date(),autoClose = TRUE,language = "ro", width = "250px"), br(),
                    shinyWidgets::downloadBttn(ns("down_scenarii"),label = "Downloadeaza baza de date a scenariilor",
                          style = "stretch",color = "success",size = "sm")) ),
                 column(width = 8,DT::dataTableOutput(ns("scenarii")) ) )
    )
  )
  
}
    
#' admin Server Functions
#'
#' @noRd 
mod_admin_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    baza_coeficienti <- readRDS( 'R/reactivedata/ifrs/coef_non_ifrs.rds' )
    
    baza_scenarii <- readRDS("R/reactivedata/ifrs/scenarios.rds")
    
    vals_admin <- reactiveValues(baza_coeficienti = baza_coeficienti, baza_scenarii = baza_scenarii)
    
    
    scenarii_filtered <-reactive({ baza_scenarii %>% dplyr::filter(From_Date <= input$data_scenarii,
                                                                   To_Date >= input$data_scenarii)    })
    
    output$scenarii <- DT::renderDataTable( {req (scenarii_filtered())
      DT::datatable(data = scenarii_filtered(),selection = list(mode = "single",selected = NULL, target = "row"),
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
              paste0("Scenariile de evolutie macroeconomica in vigoare la data de ", input$data_scenarii,
      ". Click in tabel pentru a actualiza")), rownames = FALSE, options = list(dom = "t"), class = "compact") %>%
        DT::formatPercentage(columns = 1:3,digits = 1) %>% DT::formatStyle(color = "#919191",columns = 1:5)
      })
    
    observeEvent(input$scenarii_rows_selected,{
      showModal(modalDialog(title = "New scenarios", size = "l", footer = list(modalButton("Cancel"),
              actionButton(ns("save_new_scenario"),label = "Save",icon = icon("save"))),
              
              DT::dataTableOutput(ns("scenarii_actuale")),
              DT::dataTableOutput(ns("scenarii_propuse"))
      ))
      
      output$scenarii_actuale <- DT::renderDataTable( { req( scenarii_filtered() )
        dt_generate_function(df = scenarii_filtered(),perc_col = 1:3, digits_perc=1, editable=TRUE)
        })
      
      })
    
    scenarii_modificate <- eventReactive( input[['scenarii_actuale_cell_edit']],{
      DT::editData( scenarii_filtered(), info=input[['scenarii_actuale_cell_edit']],rownames = FALSE) })
    
    
    observeEvent(input$save_new_scenario,{
      
      if ( any(scenarii_modificate()$From_Date < scenarii_filtered()$From_Date) ) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
            text = "Nu pot salva scenarii care incep mai devreme decat ce am in baza de date")
      } else if ( any( length(unique(scenarii_modificate()$From_Date))>1, length(unique(scenarii_modificate()$To_Date))>1) ) {
        shinyWidgets::sendSweetAlert(session = session,title = "STOP",type = "error",
                                     text = "Nu pot salva scenarii in care From_Date si To_Date nu sunt unice")
      } else {
        
        
        if ( unique( scenarii_modificate()$From_Date) > unique(scenarii_filtered()$From_Date) ) {
          saveRDS(object = baza_scenarii %>% dplyr::filter(To_Date !=as.Date("9999-12-31")) %>%
                    dplyr::bind_rows(scenarii_filtered() %>% dplyr::mutate(To_Date = unique(scenarii_modificate()$From_Date)-1)) %>%
                    dplyr::bind_rows(scenarii_modificate() %>% dplyr::mutate(To_Date=as.Date("9999-12-31"))),
                  file = "R/reactivedata/ifrs/scenarios.rds") } else {
                      saveRDS(object = baza_scenarii  %>% dplyr::filter(To_Date !=as.Date("9999-12-31")) %>%
                            dplyr::bind_rows( scenarii_modificate() %>% dplyr::mutate(To_Date=as.Date("9999-12-31"))),
                            file = "R/reactivedata/ifrs/scenarios.rds")      }
        
        output$scenarii <- DT::renderDataTable( {req (scenarii_modificate())
          DT::datatable(data = scenarii_modificate(),selection = list(mode = "single",selected = NULL, target = "row"),
                        caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
              paste0("Scenariile de evolutie macroeconomica in vigoare la data de ", input$data_scenarii,
              ". Click in tabel pentru a actualiza")), rownames = FALSE, options = list(dom = "t"), class = "compact") %>%
            DT::formatPercentage(columns = 1:3,digits = 1) %>% DT::formatStyle(color = "#919191",columns = 1:5)
        })
        
        removeModal(session = session)
        shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Am salvat cu succes noile scenarii",
          .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
        
        
      }
        
        
        
      
    })
    
    
    coeficienti_depreciate <- reactive({ baza_coeficienti %>% dplyr::filter(FromDate <= input$data_coeficienti,
                                    ToDate>= input$data_coeficienti)
    })
    
    output$coeficienti_depreciate <- DT::renderDataTable({ req(coeficienti_depreciate())
      dt_generate_function( coeficienti_depreciate(), perc_col = 1:5,digits_perc = 0,
                           caption=paste0("Coeficienti provizionare garantii depreciate valabili la data de ",
                                          input$data_coeficienti), editable=TRUE)
      })
    
   
    
    observeEvent(input$new_coeficienti,{
      coeficienti_actuali <<- baza_coeficienti %>% dplyr::arrange(desc(ToDate)) %>%
        dplyr::slice(1) 
      coeficienti_de_modificat <- coeficienti_actuali %>% dplyr::mutate(FromDate = 
          lubridate::`%m+%`(as.Date("2021-12-31"), lubridate::years(1)),
          ToDate = as.Date("9999-12-31"))
      showModal(modalDialog(title = "New Coefficients", size = "l", footer = list(modalButton("Cancel"),
                actionButton(ns("save_new_coeficienti"),label = "Save",icon = icon("save"))),
                DT::dataTableOutput(ns("coeficienti_actuali")),
                DT::dataTableOutput(ns("coeficienti_de_modificat"))
      ))
      
      output$coeficienti_actuali <- DT::renderDataTable(dt_generate_function(coeficienti_actuali,
          caption = "Coeficienti actuali", perc_col = 1:5,digits_perc = 0))
      
      output$coeficienti_de_modificat <- DT::renderDataTable(dt_generate_function(coeficienti_de_modificat,
        caption = "Completeaza mai jos noii coeficienti",perc_col = 1:5,digits_perc = 0, editable="cell"))
      
      coeficienti_modificati <<- eventReactive(input[['coeficienti_de_modificat_cell_edit']],{
        DT::editData(coeficienti_de_modificat, info=input[['coeficienti_de_modificat_cell_edit']],rownames = FALSE) })
      
    })
    
    observeEvent(input$save_new_coeficienti,{
      removeModal(session = session)
      
      baza_coeficienti <- baza_coeficienti %>% dplyr::mutate(dplyr::across(ToDate, .fns = ~ifelse(.x==as.Date("9999-12-31"),
              lubridate::`%m+%`(coeficienti_actuali$FromDate,lubridate::years(1))-1,.x) %>% as.Date(origin="1970-01-01"))) %>%
        dplyr::bind_rows(coeficienti_modificati()) %>% dplyr::arrange(desc(FromDate))
      
     saveRDS(object = baza_coeficienti,file = 'R/reactivedata/ifrs/coef_non_ifrs.rds')
     
     shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database. Click refresh to see it",
                      .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    })
      
    
 
  })
}
    
## To be copied in the UI
# mod_admin_ui("admin_ui_1")
    
## To be copied in the server
# mod_admin_server("admin_ui_1")

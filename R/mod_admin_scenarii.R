#' admin_scenarii UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_scenarii_ui <- function(id){
  ns <- NS(id)
  fluidRow( column( width = 4,
                    shinyWidgets::airDatepickerInput(ns("data_scenarii"),label = "Selecteaza data scenariilor",
                    value = Sys.Date(),autoClose = TRUE,language = "ro", width = "250px") ),
            
            column(width = 3, br(), actionButton(inputId = ns("new_scenarii"), label = "Add new scenarii", 
                                  style = "color: #3cbcad; border-color: #fff;", icon = icon('plus'))),
            
            column(width = 5, br(),
                            shinyWidgets::downloadBttn(ns("down_scenarii"),label = "Downloadeaza baza de date a scenariilor",
                                                       style = "stretch",color = "success",size = "sm")),
            
            column(width = 12,DT::dataTableOutput(ns("scenarii")) ) 
            
            )
}
    
#' admin_scenarii Server Functions
#'
#' @noRd 
mod_admin_scenarii_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    istoric_scenarii <- readRDS("R/reactivedata/ifrs/scenarios.rds") %>% 
                          dplyr::arrange(desc(To_Date))
    
    vals_admin_scenarii <- reactiveValues( istoric_scenarii = istoric_scenarii )
    
    output$down_scenarii <- downloadHandler(filename = function() {"istoric_scenarii.csv"}, content = function(file) {
      readr::write_csv(x = vals_admin_scenarii$istoric_scenarii, file = file )   })
    
    scenarii_filtered <-reactive({ vals_admin_scenarii$istoric_scenarii %>% dplyr::filter(From_Date <= input$data_scenarii,
                                                                   To_Date >= input$data_scenarii)    })
    
    output$scenarii <- DT::renderDataTable( {req (scenarii_filtered())
      DT::datatable(data = scenarii_filtered(),selection = list(mode = "single",selected = NULL, target = "row"),
                    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
      paste0("Scenariile de evolutie macroeconomica in vigoare la data de ", input$data_scenarii,
         ". Click in tabel pentru a actualiza")), rownames = FALSE, options = list(dom = "t"), class = "compact") %>%
        DT::formatPercentage(columns = 1:3,digits = 1) %>% DT::formatStyle(color = "#919191",columns = 1:5)
    })
    
    observeEvent( input$new_scenarii, {
      scenariul_actual <<- vals_admin_scenarii$istoric_scenarii  %>%  dplyr::slice(1:3) %>% dplyr::arrange(desc(Probability))
      
      showModal( modalDialog( title = "New Scenarii",    size = "l",
                              footer = list( h6("Se completeaza scenariile valabile dupa expirarea scenariului actual. 
                    Atentie la data completarii datei de expirare, aceasta va fi data de expirare a scenariului actual
                    si se va activa doar cand se modifica una din info de mai sus. 
                    Noul scenariu va primi automat data expirarii 2100-01-01",    style = "color:#c92052;"),
                                modalButton("Cancel"),
                                            uiOutput(ns("show_save_scenarii"))),
                              fluidRow(  column( width = 12,
                                                 h5("Scenariul de baza", style= "color: #20c997;"), hr() ),
                                         
                                         column(width = 4, 
                                                shinyWidgets::autonumericInput(inputId = ns("crestere_baza"),
                                                  label = "Crestere economica",
                                                  value = scenariul_actual$Economic_Growth[1],align = "right",
                                                  maximumValue = 0.2, modifyValueOnWheel = TRUE,  minimumValue = -0.2,
                                                  wheelStep = 0.1,decimalPlaces = 3 ) ),
                                         column(width = 4, 
                                                shinyWidgets::autonumericInput(inputId = ns("robor_baza"),
                                                label = "Robor 3M scenariul de baza",
                                                value = scenariul_actual$Robor_3M[1],align = "right",
                                                maximumValue = 0.3, modifyValueOnWheel = TRUE,  minimumValue = -0.1,
                                                wheelStep = 0.1,decimalPlaces = 3 ) ),
                                         column(width = 4,
                                                shinyWidgets::autonumericInput(inputId = ns("probability_baza"),
                                                label = "Probabilitatea scenariului de baza",
                                                value = scenariul_actual$Probability[1],align = "right",
                                                maximumValue = 1, modifyValueOnWheel = TRUE,  minimumValue = 0,
                                                wheelStep = 0.1,decimalPlaces = 2 )),
                                         
                                         column(width = 12, h5("Scenariul 2", style = "color: #c99720;"), hr() ),
                                         
                                         column(width = 4, 
                                                shinyWidgets::autonumericInput(inputId = ns("crestere_medie"),
                                                    label = "Crestere economica",
                                                    value = scenariul_actual$Economic_Growth[2],align = "right",
                                                    maximumValue = 0.2, modifyValueOnWheel = TRUE,  minimumValue = -0.2,
                                                    wheelStep = 0.1,decimalPlaces = 3 ) ),
                                         column(width = 4, 
                                                shinyWidgets::autonumericInput(inputId = ns("robor_medie"),
                                                  label = "Robor 3M scenariul 2",
                                                  value = scenariul_actual$Robor_3M[2],align = "right",
                                                  maximumValue = 0.3, modifyValueOnWheel = TRUE,  minimumValue = -0.1,
                                                  wheelStep = 0.1,decimalPlaces = 3 ) ),
                                         column(width = 4,
                                                shinyWidgets::autonumericInput(inputId = ns("probability_medie"),
                                                  label = "Probabilitatea scenariului 2",
                                                  value = scenariul_actual$Probability[2],align = "right",
                                                  maximumValue = 1, modifyValueOnWheel = TRUE,  minimumValue = 0,
                                                  wheelStep = 0.1,decimalPlaces = 2 )),
                                         
                                         column(width = 12, h5("Worst case scenario", style="color:#c92052;"), hr() ),
                                         
                                         column(width = 4, 
                                                shinyWidgets::autonumericInput(inputId = ns("crestere_worst"),
                                                  label = "Crestere economica",
                                                  value = scenariul_actual$Economic_Growth[3],align = "right",
                                                  maximumValue = 0.2, modifyValueOnWheel = TRUE,  minimumValue = -0.2,
                                                  wheelStep = 0.1,decimalPlaces = 3 ) ),
                                         column(width = 4, 
                                                shinyWidgets::autonumericInput(inputId = ns("robor_worst"),
                                                  label = "Robor 3M worst case",
                                                  value = scenariul_actual$Robor_3M[3],align = "right",
                                                  maximumValue = 0.3, modifyValueOnWheel = TRUE,  minimumValue = -0.1,
                                                  wheelStep = 0.1,decimalPlaces = 3 ) ),
                                         column(width = 4,
                                                shinyWidgets::autonumericInput(inputId = ns("probability_worst"),
                                                  label = "Probabilitatea worst case",
                                                  value = scenariul_actual$Probability[3],align = "right",
                                                  maximumValue = 1, modifyValueOnWheel = TRUE,  minimumValue = 0,
                                                  wheelStep = 0.1,decimalPlaces = 2 )),
                                         
                                         column(width = 6, 
                                                shinyWidgets::airDatepickerInput(ns("data_new_scenarii"),
                                                  label = "Data de intrare in vigoare a noilor scenarii",
                                                  value = input$data_scenarii,language = "ro", autoClose = TRUE,
                                                  minDate = scenariul_actual$From_Date[1]+1,
                                                  position = "top right") ),
                                         
                                         column(width = 6, uiOutput(ns("show_data_scenarii_actuale")))
                              )
      )
      )
      
     
      scenarii_noi <<- reactive( {
        req( input$crestere_baza, input$crestere_medie, input$crestere_worst,
             input$robor_baza, input$robor_medie, input$robor_worst,
             input$probability_baza, input$probability_medie, input$probability_worst )
        
        data.frame( Economic_Growth  = c(input$crestere_baza, input$crestere_medie, input$crestere_worst),
                    Robor_3M  = c(input$robor_baza, input$robor_medie, input$robor_worst),
                    Probability = c(input$probability_baza, input$probability_medie, input$probability_worst),
                    From_Date = input$data_new_scenarii, To_Date = as.Date("2100-01-01"),
                    row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE  )
      })
      
      
      modify_scenarii <- reactive({ req( scenarii_noi(), scenariul_actual)
        ifelse( (scenarii_noi() %>% dplyr::select(1:3) == scenariul_actual %>%  dplyr::select(1:3)) %>% data.frame() %>% 
                  dplyr::summarise_all(list(sum)) %>% sum() == 9,"no","yes")   })
      
      output$show_save_scenarii <- renderUI({ req( modify_scenarii() == "yes")
        
        shinyWidgets::actionBttn(  ns("save_new_scenarii"),
                                   label = "Save", icon = icon("save"),  size = "sm",
                                   style = "stretch",  color = "success" )   })
      
      
      output$show_data_scenarii_actuale <- renderUI({ req( modify_scenarii() == "yes" )
        
        shinyWidgets::airDatepickerInput( ns("data_scenarii_actuale"),autoClose = T,position = "top left",
                                          label = "Data de expirare a scenariilor actuale", language = "ro",
                                          value = input$data_new_scenarii - 1  )
      })
      
      
    })
    
    observeEvent(input$save_new_scenarii,{
      removeModal(session = session)
      
      vals_admin_scenarii$istoric_scenarii <- dplyr::bind_rows( scenarii_noi(),
            vals_admin_scenarii$istoric_scenarii %>% dplyr::slice(-c(1:3)),  
              scenariul_actual %>% dplyr::mutate( To_Date = input$data_scenarii - 1) )
     
      saveRDS(object = vals_admin_scenarii$istoric_scenarii,file = 'R/reactivedata/ifrs/scenarios.rds')
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database. Click refresh to see it",
                               .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    })
    
                                                 
    
    
  })
}
    
## To be copied in the UI
# mod_admin_scenarii_ui("admin_scenarii_1")
    
## To be copied in the server
# mod_admin_scenarii_server("admin_scenarii_1")

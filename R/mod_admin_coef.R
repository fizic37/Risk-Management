#' admin_coef UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_admin_coef_ui <- function(id){
  ns <- NS(id)
  
  fluidRow( 
    shinyjs::useShinyjs(),
    column( width =4,
                    
          shinyWidgets::airDatepickerInput(ns("data_coeficienti"),label = "Selecteaza data coeficientilor",
                 value = Sys.Date(),autoClose = TRUE,language = "ro", width = "250px") ),
            
            column(width = 3,  
                   br(), actionButton(inputId = ns("new_coeficienti"), label = "Add new coefficients", 
                        style = "color: #3cbcad; border-color: #fff;", icon = icon('plus'))),
            
          column(width = 5, br(),shinyWidgets::actionBttn(inputId = ns("down_istoric_coeficienti"),
                      label = "Downloadeaza istoricul coeficientilor",style = "stretch",color = "success",
                              icon = icon("download"),size = "sm")) ,
            
          column(width = 12, DT::dataTableOutput(ns("coeficienti_depreciate")))
  )
}
    
#' admin_coef Server Functions
#'
#' @noRd 
mod_admin_coef_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    istoric_coeficienti <- readRDS( 'R/reactivedata/ifrs/istoric_coef_provizionare.rds' ) %>%
                              dplyr::arrange(desc(ToDate))
    
    vals_admin_coef <- reactiveValues( istoric_coeficienti = istoric_coeficienti)
    
    output$down_istoric_coeficienti <- downloadHandler(filename = function() {"istoric_coficienti_provizionare.csv"},
                content = function(file) { readr::write_csv(x = vals_admin_coef$istoric_coeficienti, file = file ) } )
    
    coeficienti_depreciate <- reactive({ vals_admin_coef$istoric_coeficienti %>% dplyr::filter(FromDate <= input$data_coeficienti,
                                                ToDate>= input$data_coeficienti)
    })
    
    output$coeficienti_depreciate <- DT::renderDataTable({ req(coeficienti_depreciate())
      dt_generate_function( coeficienti_depreciate(), perc_col = 1:5,digits_perc = 0,
                            caption=paste0("Coeficienti provizionare garantii depreciate valabili la data de ",
                                           input$data_coeficienti), editable=TRUE)
    })
    
    
    
    observeEvent( input$new_coeficienti, {
      coeficienti_actuali <<- vals_admin_coef$istoric_coeficienti  %>%  dplyr::slice(1)
     
      showModal( modalDialog( title = "New Coefficients",    size = "l",
          footer = list(modalButton("Cancel"),
                        uiOutput(ns("show_save_coef"))),
          fluidRow(  column( width = 6,
              shinyWidgets::autonumericInput(  ns("coef_trans_cereri_plata"),
                maximumValue = 1, modifyValueOnWheel = TRUE,  align = "right",  wheelStep = 0.01,
                label = "Transformare cereri de plata in plati", decimalPlaces = 2,
                value = coeficienti_actuali$Coef_trans_cereri_plata_plati  ),
              
              shinyWidgets::autonumericInput( ns("coef_trans_insolv"),
                label = "Transformare insolvente in cereri de plata", decimalPlaces = 2,
                maximumValue = 1, modifyValueOnWheel = TRUE,align = "right",
                wheelStep = 0.01,value = coeficienti_actuali$Coef_trans_cereri_plata_insolvente  ),
              
              shinyWidgets::autonumericInput( ns("coef_trans_instiintari"),
                maximumValue = 1, modifyValueOnWheel = TRUE,align = "right",
                wheelStep = 0.01, decimalPlaces = 2,
                label = "Transformare instiintari de neplata in cereri de plata",
                value = coeficienti_actuali$Coef_trans_cereri_plata_instiintari )
                    ),
            column(   width = 6,
              shinyWidgets::airDatepickerInput( ns("data_modificare_coeficienti"),
                label = "Data de modificare a coeficientilor", autoClose = TRUE,
                value = input$data_coeficienti, language = "ro"  ),
             
               shinyWidgets::autonumericInput(  ns("ajustare_ctg"),
                maximumValue = 1,  modifyValueOnWheel = TRUE,     align = "right",
                wheelStep = 0.01,  label = "Ajustare Contragarantii",
                value = coeficienti_actuali$Ajustare_ctg, decimalPlaces = 2  ),
              
              shinyWidgets::autonumericInput( ns("provizionare_plati"),
                maximumValue = 1, modifyValueOnWheel = TRUE,  align = "right", wheelStep = 0.01,
                  label = "Coeficient mediu de provizionare a platilor de garantii",
                value = coeficienti_actuali$Coef_provizionare_plati,  decimalPlaces = 2  ),
             
              uiOutput(ns( 'show_date_coef_actuali' ))), 
             
            
            column(width = 12, uiOutput(ns( "message_coef_input")))
          )
          
        )
      )
      
      coeficienti_noi <<- reactive({
          req(input$coef_trans_cereri_plata,input$coef_trans_insolv,
            input$coef_trans_instiintari, input$ajustare_ctg, input$provizionare_plati  )
          
        data.frame( Coef_trans_cereri_plata_plati = input$coef_trans_cereri_plata,
            Coef_trans_cereri_plata_insolvente = input$coef_trans_insolv,
            Coef_trans_cereri_plata_instiintari = input$coef_trans_instiintari,
            Ajustare_ctg = input$ajustare_ctg,
            Coef_provizionare_plati = input$provizionare_plati,
            FromDate = input$data_modificare_coeficienti,
            ToDate = as.Date("2100-01-01"),
            row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE  )
        })
      
      modify_coef <- reactive({ req( coeficienti_noi(), coeficienti_actuali)
        ifelse( length( which( coeficienti_noi() %>% dplyr::select(1:5) == coeficienti_actuali %>% 
                              dplyr::select(1:5) ) ) != 5,"yes","no")     })
      
      output$show_save_coef <- renderUI({ req( modify_coef() == "yes")
       
        shinyWidgets::actionBttn(  ns("save_new_coeficienti"),
          label = "Save", icon = icon("save"),  size = "sm",
          style = "stretch",  color = "success" )   })
      
      output$show_date_coef_actuali <- renderUI({ req( modify_coef() == "yes" )
        
        shinyWidgets::airDatepickerInput( ns("data_coef_actuali"),autoClose = T,
          label = "Data de expirare a coeficientilor actuali", language = "ro",
              value = input$data_modificare_coeficienti - 1  )
        })
      
})
    
    
    
    observeEvent(input$save_new_coeficienti,{
      removeModal(session = session)
      
      vals_admin_coef$istoric_coeficienti <- dplyr::bind_rows(coeficienti_noi(),vals_admin_coef$istoric_coeficienti %>% dplyr::slice(-1), 
                coeficienti_actuali %>% dplyr::mutate(ToDate = input$data_modificare_coeficienti - 1))
      
      saveRDS(object = vals_admin_coef$istoric_coeficienti,file = 'R/reactivedata/ifrs/istoric_coef_provizionare.rds')
      
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Saved to database. Click refresh to see it",
          .options = list("timeOut"=1500, 'positionClass'="toast-bottom-right", "progressBar" = TRUE))
    })
    
 
  })
}
    
## To be copied in the UI
# mod_admin_coef_ui("admin_coef_1")
    
## To be copied in the server
# mod_admin_coef_server("admin_coef_1")

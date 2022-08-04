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
    shinyjs::useShinyjs(),
    
    
    bs4Dash::box( title="Upload modelul de adresa catre contabilitate - Provizioane Specifice",
                  status = "info",width = 12, collapsible = T, collapsed = T,
                  maximizable = TRUE, icon = icon("file-word"),
                  footer = "Atentie, nu voi prelucra upload-ul tau. 
                  Doar il voi salva si voi incerca sa scriu in el info updatate.",
                  
                  fluidRow(column(width = 3, fileInput(inputId = ns("upload_doc"),
                                  label = "Uploadeaza modelul de adresa",accept = c(".docx"),
                                  placeholder = "nothing uploaded",buttonLabel = "docx only")),
                           
                           column(width = 8, div(style = "padding-top: 24px; padding-left: 190px;",
                                  shinyWidgets::downloadBttn(outputId = ns("down_doc"),
                                  label = "Downloadeaza modelul existent de adresa",
                                  style = "stretch",color = "success")))) ),
    
    bs4Dash::box( title="Coeficienti de provizionare garantii depreciate",
                 status = "info",width = 12, collapsible = T, collapsed = TRUE,
                 maximizable = TRUE, icon = icon("square-root-alt"),id = ns("box_admin_coef"),
                 mod_admin_coef_ui("admin_coef_1") ),
    
    bs4Dash::box(title = "Scenarii de evolutie macroeconomica", status = "info",width = 12, collapsible = T,
                 collapsed = TRUE, maximizable = TRUE, icon = icon("chart-area"),id = ns("box_admin_scenarii"),
                 mod_admin_scenarii_ui("admin_scenarii_1") )
    
  )
  
}
    
#' admin Server Functions
#'
#' @noRd 
mod_admin_server <- function(id, vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
   
    observeEvent(input$box_admin_coef,{ vals$box_selected <- c(vals$box_selected, "box_admin_coef")  })
    
    observeEvent(input$box_admin_scenarii,{ vals$box_selected <- c(vals$box_selected, "box_admin_scenarii") })
    
    observeEvent(input$upload_doc,{ shiny::validate( shiny::need(expr = tools::file_ext(input$upload_doc$datapath) == "docx",
                                  message = "Docx only") )
      file.copy(from = input$upload_doc$datapath, to = "R/reactivedata/template_provizioane_plati.docx",overwrite = TRUE)
      shinyFeedback::showToast(type = "success",title = "SUCCES",message = "Am salvat cu succes modelul de adresa",
      .options = list("timeOut"=1000, 'positionClass'="toast-bottom-right", "progressBar" = TRUE) )
    })
    
    output$down_doc <- downloadHandler(filename = function() {"template_provizioane_plati.docx"},
            content = function(file) {file.copy(from = "R/reactivedata/template_provizioane_plati.docx",to = file, overwrite = TRUE ) } )
    
    
  })
}
    
## To be copied in the UI
# mod_admin_ui("admin_ui_1")
    
## To be copied in the server
# mod_admin_server("admin_ui_1")

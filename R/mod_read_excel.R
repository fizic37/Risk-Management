#' read_excel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_excel_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' read_excel Server Functions
#'
#' @noRd 
mod_read_excel_server <- function(id, excel_reactive, green="#20c997", red="#c92052"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # I need below function in order to extract column type
        get_column_types <- function(names, columns) {
      if (names%in% names(columns)) columns[[names]] else "guess"
    }
    
    observeEvent(excel_reactive$file_input,{
     
      sheets_read <- eventReactive(excel_reactive$file_input,{
        shiny::validate(shiny::need(tools::file_ext(excel_reactive$file_input) %in% c("xlsx","xls"), message = FALSE))
        readxl::excel_sheets(excel_reactive$file_input) })
      
      if ( length(sheets_read())>1 ) {
        
        shinyWidgets::inputSweetAlert(session = session,inputId = ns("ok_sheet"), input = "select",
              inputOptions = sheets_read(),type = "warning",  btn_labels = "OK",btn_colors = green,#"#20c997",
                                      title = "STOP, fiserul ure mai multe sheet-uri", 
                                      "Selecteaza de mai jos sheet-ul pe care sa-l citesc")
        
        selected_sheet <- eventReactive(input$ok_sheet,{input$ok_sheet})
      }
      
      else if (length(sheets_read())==1) {
        selected_sheet <- reactive({1})
      }
      
      # First read of the excel
      
      excel_first_read <- reactive({req(selected_sheet())
        readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), range = "A1:AA50",.name_repair = "minimal")
        })
     
      
      # I get the row index where name if the columns are
      index_citire <- reactive({ req(excel_first_read())
        apply(excel_first_read(),1,function(x) (sum(excel_reactive$nume_obligatorii %in% x)==length(excel_reactive$nume_obligatorii))) %>% 
          which(TRUE) %>% max(0,na.rm = TRUE)})
      
      # Second read of the excel, this time starting where the column names are
      file_read <- reactive({req(excel_first_read())
        readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), skip = index_citire())   })
    
      
      observe({req(file_read())
        
        excel_reactive$all_names <- excel_reactive$nume_obligatorii %in% names(file_read()) %>% all() 
        
        excel_reactive$missing_names <- setdiff(excel_reactive$nume_obligatorii,names(file_read()))
        
        if (!is.null(excel_reactive$optional_names) && excel_reactive$optional_names %in% names(file_read()) %>% any()) {
          excel_reactive$coloane_selectate <- c(excel_reactive$nume_obligatorii, 
              excel_reactive$optional_names[which(excel_reactive$optional_names %in% names(file_read()))]) }
        
        else {excel_reactive$coloane_selectate <- excel_reactive$nume_obligatorii }
        
        if (excel_reactive$all_names) {
          
         excel_reactive$new_column_types <- purrr::map_chr(.x = names(file_read()),
                                ~get_column_types(names=.x,columns=excel_reactive$colum_types))
              
          excel_reactive$file_read_prel <- readxl::read_excel(excel_reactive$file_input,sheet = selected_sheet(), 
                  skip = index_citire(),col_types = excel_reactive$new_column_types) %>%
            dplyr::select(excel_reactive$coloane_selectate) %>%
            dplyr::mutate_if(.predicate = lubridate::is.POSIXct,.funs = as.Date.POSIXct) %>%
            dplyr::mutate_if(.predicate = lubridate::is.POSIXlt,.funs = as.Date.POSIXlt)
        }
        
      })
      
      
      
      
    }) 
  })
}
    
## To be copied in the UI
# mod_read_excel_ui("read_excel_ui_1")
    
## To be copied in the server
# mod_read_excel_server("read_excel_ui_1")

#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_compare_df_ui <- function(id){
  ns <- NS(id)
  
}
    
#' compare_df Server Function
#'
#' @noRd 
mod_compare_df_server <- function(input, output, session,df_reactive,green="#20c997", red="#c92052"){
  ns <- session$ns
  library(magrittr)
  
  # It needs inside df_reactive a df_old which is the old database, a element_id which is the date by which it
  # searches if df_old already contains that date, and a column_id which is the column of df_old where it searches for element_id
  
  observeEvent(df_reactive$element_id,{
    
   df_reactive$needs_overwrite <- as.character(df_reactive$element_id %in% 
                                    unique(dplyr::pull(.data = df_reactive$df_old,df_reactive$column_id)))
    
   if (df_reactive$needs_overwrite == "TRUE"){
     shinyWidgets::ask_confirmation(inputId = session$ns("overwrite_plati_input"),title = "STOP",session = session,
                text = "Am deja un fisier cu data selectata de tine in baza mea de date",
                                   btn_labels = c("Cancel","Overwrite"),btn_colors = c(red,green))  }
   
    else{
      df_reactive$check_upload <- janitor::compare_df_cols_same(df_reactive$df_old, df_reactive$df_new, 
                                          bind_method = "bind_rows", verbose = TRUE)
      
      if (df_reactive$check_upload) {
        shinyWidgets::ask_confirmation(inputId = session$ns("ok_write_database"),session = session,
                                       title = "SUCCES",btn_colors = c(red,green),
          text = "Baza de date poate fi uploadata cu scucces. Esti sigur ca vrei sa continui?",btn_labels = c("Cancel", "OK"))  }
        
      else {
        shinyWidgets::sendSweetAlert(session = session,title = "NO PROCESSING",type = "error",
              text = paste("A aparut o problema la salvare",df_reactive$check_upload))
      }  
        
    }  
   
   observeEvent(input$ok_write_database,{ req(input$ok_write_database == TRUE)
     df_reactive$df_new_prel <- dplyr::bind_rows(df_reactive$df_old, df_reactive$df_new)
     df_reactive$finalise_process_compare_df <- TRUE
     
   })
   
   
   observeEvent(input$overwrite_plati_input,{req(input$overwrite_plati_input==TRUE)
     df_reactive$df_new_prel <- dplyr::bind_rows(df_reactive$df_old %>% 
                dplyr::filter(!!dplyr::sym(df_reactive$column_id) != df_reactive$element_id), df_reactive$df_new)
     
     df_reactive$finalise_process_compare_df <- TRUE
    })
   
   observeEvent(input$overwrite_plati_input,{req(input$overwrite_plati_input==FALSE)
     removeModal(session = session) })
   
  })
 
}
    
## To be copied in the UI
# mod_compare_df_ui("compare_df_ui_1")
    
## To be copied in the server
# callModule(mod_compare_df_server, "compare_df_ui_1")
 

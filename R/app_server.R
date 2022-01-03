#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  sidebar_selected <- c()
  box_selected <- c()
  vals <- reactiveValues(sidebar_selected = sidebar_selected, box_selected = box_selected)
  plati_reactive <- reactiveValues()
  
  view1_portofoliu <- readRDS("R/reactivedata/portofoliu/view1_portofoliu.rds")
  view2_portofoliu <- readRDS("R/reactivedata/portofoliu/view2_portofoliu.rds")
  view3_portofoliu <- readRDS("R/reactivedata/portofoliu/view3_portofoliu.rds")
  view4_portofoliu <- readRDS("R/reactivedata/portofoliu/view4_portofoliu.rds")
  
  vals_portofoliu <- reactiveValues( view1_portofoliu =  view1_portofoliu,  view2_portofoliu =  view2_portofoliu,
                   view3_portofoliu = view3_portofoliu, view4_portofoliu = view4_portofoliu)
 
  mod_sidebar_server("sidebar_ui_1", vals)
  
  observeEvent(vals$sidebar_selected,{
    
    if (sum("home" == vals$sidebar_selected)==1) {
      
      mod_home_server("home_ui_1", parrent=session)
     
      vals$sidebar_selected <- c(vals$sidebar_selected,"home")   }
    
    if (sum("plati" == vals$sidebar_selected)==1) {
      callModule(mod_provizioane_plati_server, "provizioane_plati_ui_1", vals, plati_reactive)
      vals$sidebar_selected <- c(vals$sidebar_selected,"plati")   }
    
    if (sum("solduri" == vals$sidebar_selected)==1) {
      callModule(mod_database_portofoliu_server, "database_portofoliu_ui_1", vals, vals_portofoliu)
      vals$sidebar_selected <- c(vals$sidebar_selected,"solduri")   }
    
    if (sum("crc" == vals$sidebar_selected)==1) {
      callModule(mod_crc_server, "crc_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"crc")   }
    
    if (sum("ifrs" == vals$sidebar_selected)==1) {
      callModule(mod_ifrs_portofoliu_server, "ifrs_portofoliu_ui_1", vals_portofoliu, parrent_session=session)
      mod_ifrs_database_server("ifrs_database_ui_1")
      vals$sidebar_selected <- c(vals$sidebar_selected,"ifrs")   }
    
    if (sum("calibrare" == vals$sidebar_selected)==1) {
      mod_calibrare_server("calibrare_ui_1")
      vals$sidebar_selected <- c(vals$sidebar_selected,"calibrare")   }
    
  })
  
  observeEvent(vals$box_selected,{
    
    if ( sum("box_upload_plati" == vals$box_selected)==1 ) { 
      callModule(mod_database_upload_plati_server, "database_upload_plati_ui_1", plati_reactive)
      vals$box_selected <- c(vals$box_selected, "box_upload_plati") }
    
    if ( sum("box_coeficienti_plati" == vals$box_selected)==1 ) { 
      callModule(mod_coeficienti_plati_server, "coeficienti_plati_ui_1", plati_reactive)
      
      vals$box_selected <- c(vals$box_selected, "box_coeficienti_plati") }
    
    
    if ( sum("box_utility" == vals$box_selected)==1 ) { 
      
      callModule(mod_database_util_files_server, "database_util_files_ui_1")
      
      vals$box_selected <- c(vals$box_selected, "box_utility") }
    
    if ( sum("box_upload_portofoliu" == vals$box_selected)==1 ) { 
      
      callModule(mod_database_portofoliu_upload_server, "database_portofoliu_upload_ui_1", vals_portofoliu)
      
      vals$box_selected <- c(vals$box_selected, "box_upload_portofoliu") }
    
    if ( sum("box_coeficienti_portofoliu" == vals$box_selected)==1 ) { 
      
      mod_coeficienti_portofoliu_server("coeficienti_portofoliu_ui_1", vals_portofoliu)
      
      #vals$box_selected <- c(vals$box_selected, "box_coeficienti_portofoliu")
      }
    
    if ( sum("box_cip" == vals$box_selected)==1 ) { 
      
      callModule(mod_cip_server, "cip_ui_1")
      
      vals$box_selected <- c(vals$box_selected, "box_cip") }
    
    
    
    
    
    
  })
    
}

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
      vals_portofoliu$ifrs_database <- readRDS("R/external_volumes/ifrs9/ifrs_database.rds")
      vals_portofoliu$ifrs_dates <- sort(vals_portofoliu$ifrs_database$data_raport %>% unique(),decreasing = TRUE)
      mod_ifrs_database_server("ifrs_database_1", database_ifrs = vals_portofoliu$ifrs_database)
      mod_ifrs_calculate_server("ifrs_calculate_ui_1", vals_portofoliu, parrent_session=session)
      mod_ifrs_migration_server("ifrs_migration_ui_1", database_ifrs = vals_portofoliu$ifrs_database,
                                ifrs_dates = vals_portofoliu$ifrs_dates )
      
      vals$sidebar_selected <- c(vals$sidebar_selected,"ifrs")   }
    
    if (sum("admin" == vals$sidebar_selected)==1) {
      mod_admin_server("admin_ui_1", vals)
      vals$sidebar_selected <- c(vals$sidebar_selected,"admin")   }
    
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
      
      vals$box_selected <- c(vals$box_selected, "box_coeficienti_portofoliu")
    }
    
    if ( sum("box_regulariz_portofoliu" == vals$box_selected)==1 ) { 
      
      mod_regularizare_portofoliu_server("regularizare_portofoliu_1", vals_portofoliu)
      
      vals$box_selected <- c(vals$box_selected, "box_regulariz_portofoliu")
    }
    
    if ( sum("box_cip" == vals$box_selected)==1 ) { 
      
      callModule(mod_cip_server, "cip_ui_1")
      
      vals$box_selected <- c(vals$box_selected, "box_cip") }
    
    if ( sum("box_admin_coef" == vals$box_selected)==1 ) { 
      
      mod_admin_coef_server("admin_coef_1")
      
      vals$box_selected <- c(vals$box_selected, "box_admin_coef") }
    
    if ( sum("box_admin_scenarii" == vals$box_selected)==1 ) { 
      
      mod_admin_scenarii_server("admin_scenarii_1")
      
      vals$box_selected <- c(vals$box_selected, "box_admin_scenarii") }
    
    if ( sum("box_admin_dobanzi_crc" == vals$box_selected)==1 ) { 
      
      mod_admin_dobanzi_crc_server("admin_dobanzi_crc_1")
      
      vals$box_selected <- c(vals$box_selected, "box_admin_dobanzi_crc") }
    
    
    
    
    
    
  })
    
}

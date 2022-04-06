#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    bs4Dash::dashboardPage(title = "Riskmanagament",
                              header = bs4Dash::dashboardHeader(title = "Risk Management"),
                              sidebar = bs4Dash::bs4DashSidebar(mod_sidebar_ui("sidebar_ui_1"), 
                                                  status = "teal",skin = "light"),
                              footer = bs4Dash::bs4DashFooter(left = "Developed by Tita Marius for FNGCIMM"),
                              
                              body = bs4Dash::bs4DashBody( bs4Dash::tabItems(
                                
                                bs4Dash::tabItem(tabName = "home",mod_home_ui("home_ui_1") ),
                                
                                bs4Dash::tabItem(tabName = "plati", mod_provizioane_plati_ui("provizioane_plati_ui_1") ),
                                
                                bs4Dash::tabItem(tabName = "solduri", mod_database_portofoliu_ui("database_portofoliu_ui_1") ),
                                
                                bs4Dash::tabItem(tabName = "crc", mod_crc_ui("crc_ui_1") ),
                                
                                bs4Dash::tabItem(tabName = "ifrs", mod_ifrs_portofoliu_ui("ifrs_portofoliu_ui_1") ),
                                
                                bs4Dash::tabItem(tabName = "admin", mod_admin_ui("admin_ui_1") )
                                
                              ) )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RiskManagement'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


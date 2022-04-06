#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    
   tags$ol(
      h4("Provizioane specifice - plati de garantii"),
      tags$li( HTML("Se ",as.character(actionLink(ns("upload_fisier_plati"), "uploadeaza Excel-ul ")),
                    " continand provizioanele specifice din Charisma.") ),
        
        #a("Se uploadeaza fisierul de provizioane din Charisma", href="#shiny-tab-plati", "data-toggle" = "tab") ),
     
      tags$li(HTML("Se genereaza catre contabilitate ",
        as.character(actionLink(ns("word_provizioane_specifice"), "adresa Word")),
        " de regularizare lunara a provizioanelor specifice.") ),
      
      tags$li(HTML(as.character(actionLink(ns("baza_provizioane_plati"),"Baza de date a provizioanelor specifice") ),
                        " poate fi decarcata detaliat sau vizualizata sintetic") ),
      
      tags$li(HTML( as.character(actionLink(inputId = ns("coeficienti_plati"),"Coeficientii IFRS9")),
        " aferenti provizioanelor specifice pot fi calculati pentru toata perioada istorica disponibila ", 
                    ))
      
      
    ),
   
   hr(),
   tags$ol(
     h4("Provizioane aferente garantiilor depreciate"),
     tags$li( HTML("Se ",as.character(actionLink(ns("upload_utile_solduri"), "uploadeaza fisierele utile ")),
      " continand BI-ul cu cererile de plata primite, ultimele insolvente disponibile si BI-ul de mapare a CUI-urilor") ),
     
     tags$li( HTML( "Se ", as.character(actionLink(ns("upload_fisier_solduri"), "uploadeaza fisierul de solduri")),
                    " din fileserver/Provizioane")),
     
     tags$li(HTML(as.character(actionLink(ns("baza_provizioane_solduri"),"Baza de date a provizioanelor depreciate") ),
                  " poate fi decarcata detaliat sau vizualizata sintetic") )
   )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id, parrent){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    #observeEvent(input$coeficienti_plati,{
     # shinyWidgets::ask_confirmation(ns("ask"),title = "top","some_text")
      
      
     #bs4Dash::updateTabItems(session = parrent,inputId = "sidebar",selected = 2)
     # })
})
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# mod_home_server("home_ui_1")

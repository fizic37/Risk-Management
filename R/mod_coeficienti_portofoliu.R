#' coeficienti_portofoliu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_coeficienti_portofoliu_ui <- function(id){
  ns <- NS(id)
  fluidPage( 
    fluidRow(column(width = 4,
    shinyWidgets::airDatepickerInput(inputId = ns("select_year_portof"), label = "Selecteaza data raportului",
                 value = as.Date("2019-12-31"), maxDate = Sys.Date(),language = "ro",width = "300px",
                 minDate = as.Date("2010-12-31"), autoClose = TRUE ) ),
    column(width = 4, shinyWidgets::airDatepickerInput(inputId = ns("data_cerere_plata"),
                  label = "Selecteaza data maxima a cererii de plata",value = as.Date("2021-12-31"),
                  todayButton = TRUE,autoClose = TRUE,language = "ro", width = "300px")),
    column(width = 4, br(),downloadButton(outputId = ns("down_cereri_plata"),label = "Download cererile de plata filtrate"))
    ,
    
    column(width = 12, DT::dataTableOutput(ns("rata_cereri_plata")), br() ),
    
    column(width = 8, actionButton(inputId = ns("view_rata_cereri"),label = "Change view - percentage/brute",
                                                 icon = icon("toggle-on"),width = "300px")),
    
    column(width = 4, downloadButton(outputId = ns("down_rata_cereri"), label = "Download baza detaliata"))
    )
  )
             
}
    
#' coeficienti_portofoliu Server Functions
#'
#' @noRd 
mod_coeficienti_portofoliu_server <- function(id, vals_portofoliu){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    baza_citita <- readRDS(file = "R/external_volumes/portofoliu/portof_database.rds")
    
    cereri_plata <- readRDS(file = "R/external_volumes/cereri_plata/cereri_plata.rds") %>% 
                        dplyr::mutate(an_cerere_plata = lubridate::year(Data_cerere_plata))
    
    vals_portof_coef <- reactiveValues( cereri_plata = cereri_plata )
    
   
    observeEvent( vals_portof_coef$cereri_plata,{
      shinyWidgets::updateAirDateInput(session = session,inputId = "data_cerere_plata",
                value = max(vals_portof_coef$cereri_plata$Data_cerere_plata, na.rm = TRUE)
      )   }, once = TRUE )
  
    
    cereri_plata_splitata_prel <- eventReactive( input$data_cerere_plata, {
      cereri_plata_filtrata <-  cereri_plata %>% 
        dplyr::filter(Data_cerere_plata > input$select_year_portof & Data_cerere_plata <= input$data_cerere_plata) %>%
        dplyr::group_by(`Cod Partener`, an_cerere_plata) %>%
        dplyr::summarise(Cerere_Plata = sum(Cerere_Plata)) %>%
        dplyr::mutate(Cerere_Plata_cumulata = cumsum(Cerere_Plata)) %>% dplyr::ungroup()
     
       cereri_plata_splitata <- split( cereri_plata_filtrata, cereri_plata_filtrata$an_cerere_plata )
       
       purrr::map( cereri_plata_splitata, ~rename_col(df=.x) )
      
    })
    
    
    baza_selectata_final <-  eventReactive( input$select_year_portof, { req( cereri_plata_splitata_prel() )
      
      baza_selectata <- baza_citita[baza_citita$anul_de_raportare==input$select_year_portof,] %>% 
        dplyr::group_by(`Cod Partener`,categorie_contaminata) %>%  dplyr::summarise(expunere=sum(expunere))  
      
      baza_selectata_procesata <- purrr::map_df( .x = cereri_plata_splitata_prel(),
      .f = ~ dplyr::left_join(  x = baza_selectata,
           y = dplyr::select(.data = .x,   "Cod Partener",  dplyr::contains("Cerere_Plata_cumulata")  ),
                   by = "Cod Partener" ) ) %>% dplyr::select("Cod Partener",   categorie_contaminata, 
                         dplyr::contains("Cerere_Plata_cumulata")) %>%
        dplyr::group_by(`Cod Partener`, categorie_contaminata)  %>%
        dplyr::summarise_at(dplyr::vars(-dplyr::group_cols()), sum, na.rm = T) %>% dplyr::ungroup() %>%
        #I add back expunere
        dplyr::left_join(y = baza_selectata %>% dplyr::select("Cod Partener", expunere), by = "Cod Partener")
      
      cbind(  dplyr::select(  baza_selectata_procesata, "Cod Partener",    categorie_contaminata,
                              expunere),  max_col(  dplyr::select(baza_selectata_procesata,
              dplyr::contains("Cerere_plata_cumulata") )  ) )  # This cumulates Cere_plata_cumulata
      
      })
    
   
    tabel_rata_cerere_plata <- reactive({ req(baza_selectata_final())
      baza_selectata_final()  %>% dplyr::select(-"Cod Partener") %>%
      dplyr::filter(categorie_contaminata != "cerere_plata") %>% dplyr::group_by(categorie_contaminata) %>%
      dplyr::summarise_at(.vars = dplyr::vars(-dplyr::group_cols()), sum) %>% dplyr::ungroup()  })
    
    output$rata_cereri_plata <- DT::renderDataTable({ req( tabel_rata_cerere_plata() )
      if (is.null(input$view_rata_cereri) ||  input$view_rata_cereri %% 2 == 0) {
      dt_generate_function( df = tabel_rata_cerere_plata(), show_buttons=TRUE,
                            round_col = 2:ncol( tabel_rata_cerere_plata() ) ,
    caption = paste0( "Evolutie rata de transformare in cereri plata a garantiilor in sold la data de ",
                              as.character(input$select_year_portof) )  )
      }
      
      else {
        dt_generate_function(
          df = cbind(   tabel_rata_cerere_plata()[, 1:2],
                        tabel_rata_cerere_plata()[, 3:ncol(tabel_rata_cerere_plata())] /
                          tabel_rata_cerere_plata()$expunere),
          round_col = 2, show_buttons = TRUE,
          perc_col = 3:ncol(tabel_rata_cerere_plata()),
          caption = paste0(
            "Rata de trasformare in cerere de plata a garantiilor in sold la ",
            as.character(input$select_year_portof)  )  )  } 
      
    })
    
    output$down_rata_cereri <- downloadHandler(filename = function(){ paste0("migration_from_",input$select_year_portof,".csv")},
        content = function(file) { readr::write_csv(x = baza_selectata_final(),file = file) })
    
    output$down_cereri_plata <- downloadHandler( filename = function() { 
      paste0( "cereri_plata_", input$data_cerere_plata,".csv" ) },
      content = function(file) { readr::write_csv(x =  cereri_plata %>% 
        dplyr::filter(Data_cerere_plata > input$select_year_portof & Data_cerere_plata <= input$data_cerere_plata) %>%
            dplyr::select(-an_cerere_plata),file = file )   } )
})
    
}
    
## To be copied in the UI
# mod_coeficienti_portofoliu_ui("coeficienti_portofoliu_ui_1")
    
## To be copied in the server
# mod_coeficienti_portofoliu_server("coeficienti_portofoliu_ui_1")

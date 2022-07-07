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
    selectInput(inputId = ns("select_year_portof"), label = "Selecteaza data raportului",
                         choices = as.Date("2019-12-31"),
                         selected = as.Date("2019-12-31"),width = "300px") ),
    column(width = 4, shinyWidgets::airDatepickerInput(inputId = ns("data_cerere_plata"),
                  label = "Selecteaza data maxima a cererii de plata",value = as.Date("2021-12-31"),
                  todayButton = TRUE,autoClose = TRUE,language = "ro", width = "300px")),
    column(width = 4, br(),downloadButton(outputId = ns("down_cereri_plata"),label = "Download cererile de plata filtrate"))
    ),
    
    verbatimTextOutput(ns("diverse")),
             DT::dataTableOutput(ns("rata_cereri_plata")),
             br(),
             fillRow(flex = c(1,NA),actionButton(inputId = ns("view_rata_cereri"),label = "Change view - percentage/brute",
                                                 icon = icon("toggle-on"),width = "300px"),
                     downloadButton(outputId = ns("down_rata_cereri"), label = "Download baza detaliata"))
  )
             
}
    
#' coeficienti_portofoliu Server Functions
#'
#' @noRd 
mod_coeficienti_portofoliu_server <- function(id, vals_portofoliu){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    baza_citita <- readRDS(file = "R/reactivedata/portofoliu/portof_database.rds")
    
    cereri_plata <- readRDS(file = "R/reactivedata/plati/external_volume/cereri_plata.rds") %>% 
                        dplyr::mutate(an_cerere_plata = lubridate::year(Data_cerere_plata))
    
    vals_portof_coef <- reactiveValues( cereri_plata = cereri_plata )
    
    observeEvent(vals_portofoliu$view1_portofoliu, {
    updateSelectInput(session = session,inputId ='select_year_portof',
                      choices = vals_portofoliu$view1_portofoliu$anul_de_raportare[
                        which(vals_portofoliu$view1_portofoliu$anul_de_raportare< max(cereri_plata$Data_cerere_plata))]) 
    })
    
    observeEvent( vals_portof_coef$cereri_plata,{
      shinyWidgets::updateAirDateInput(session = session,inputId = "data_cerere_plata",
                                       value = as.Date("2022-01-14")
                                       #max(vals_portof_coef$cereri_plata$Data_cerere_plata)
      )
    }, once = TRUE)
    
    to_listen <- reactive({ list(input$data_cerere_plata, input$select_year_portof) })
    
    observeEvent( to_listen(),{
      
      
      vals_portof_coef$cereri_plata_filtrata <- vals_portof_coef$cereri_plata %>% 
        dplyr::filter(Data_cerere_plata > input$select_year_portof & Data_cerere_plata <= input$data_cerere_plata) %>%
        dplyr::group_by(`Cod Partener`, an_cerere_plata) %>%
        dplyr::summarise(Cerere_Plata = sum(Cerere_Plata)) %>%
        dplyr::mutate(Cerere_Plata_cumulata = cumsum(Cerere_Plata)) %>% dplyr::ungroup()
      
      
      output$down_cereri_plata <- downloadHandler( filename = function() { 
        paste0( "cereri_plata_", input$data_cerere_plata,".csv" ) },
        content = function(file) { readr::write_csv(x =  cereri_plata %>% 
                                                      dplyr::filter(Data_cerere_plata > input$select_year_portof & Data_cerere_plata <= input$data_cerere_plata) %>%
                                                      dplyr::select(-an_cerere_plata),file = file )   } )
      
      vals_portof_coef$cereri_plata_splitata <- split(
        vals_portof_coef$cereri_plata_filtrata,
        vals_portof_coef$cereri_plata_filtrata$an_cerere_plata )
      
      # I rename columns in cereri_plata_splitata. Rename_col function is to be found in mod_portofoliu_utils
      vals_portof_coef$cereri_plata_splitata_prel <- purrr::map(vals_portof_coef$cereri_plata_splitata,~rename_col(df=.x))
      
    })
    
    
    observeEvent(input$select_year_portof,{
      # I get my filtered dataframe for future processing grupped by CUI
      vals_portof_coef$baza_selectata <-  baza_citita[baza_citita$anul_de_raportare==input$select_year_portof,] %>% 
        dplyr::group_by(`Cod Partener`,categorie_contaminata) %>%  dplyr::summarise(expunere=sum(expunere))
    })
    
    
    
    # Below reactive is used in the final observer where I calculate final tables depending on modifications in baza selectata
    # sau cereri_plata_splitata_prel
    
    #final_calculation <- reactive({  list(vals_portof_coef$cereri_plata_splitata_prel,vals_portof_coef$baza_selectata)  })
    
    
    
    # Final observer
    observeEvent(  c(vals_portof_coef$cereri_plata_splitata_prel,vals_portof_coef$baza_selectata),{ 
      # I will not obtain final filtered process because, for example, if a CUI has cereri_plata during years 2015 and 2016
      # below code will produce cumulated cereri_plata in 2015 and 2016 but will only show 0 for 2017 and subsequent years
      vals_portof_coef$baza_selectata_procesata <-   purrr::map_df(
        .x = vals_portof_coef$cereri_plata_splitata_prel,
        .f = ~ dplyr::left_join(  x = vals_portof_coef$baza_selectata,
          y = dplyr::select(.data = .x,   `Cod Partener`,  dplyr::contains("Cerere_Plata_cumulata")  ),
          by = "Cod Partener" ) ) %>% dplyr::select(`Cod Partener`,   categorie_contaminata, dplyr::contains("Cerere_Plata_cumulata")) %>%
        dplyr::group_by(`Cod Partener`, categorie_contaminata)  %>%
        dplyr::summarise_at(dplyr::vars(-dplyr::group_cols()), sum, na.rm = T) %>% dplyr::ungroup() %>%
        #I add back expunere
        dplyr::left_join(y = vals_portof_coef$baza_selectata %>% dplyr::select(`Cod Partener`, expunere), by = "Cod Partener")   
      
      vals_portof_coef$baza_selectata_final <-
        cbind(
          dplyr::select(
            vals_portof_coef$baza_selectata_procesata,
            `Cod Partener`,
            categorie_contaminata,
            expunere
          ),
          max_col(
            dplyr::select(
              vals_portof_coef$baza_selectata_procesata,
              dplyr::contains("Cerere_plata_cumulata")
            )
          )
        ) # This cumulates Cere_plata_cumulata
      vals_portof_coef$tabel_rata_cerere_plata <-   vals_portof_coef$baza_selectata_final  %>% dplyr::select(-`Cod Partener`) %>% 
        dplyr::filter(categorie_contaminata!="cerere_plata") %>% dplyr::group_by(categorie_contaminata) %>% 
        dplyr::summarise_at(.vars = dplyr::vars(-dplyr::group_cols()),sum) %>% dplyr::ungroup()
      
      output$rata_cereri_plata <- DT::renderDataTable({
        if (is.null(input$view_rata_cereri) ||  input$view_rata_cereri %% 2 == 0) {
          dt_generate_function( df = vals_portof_coef$tabel_rata_cerere_plata, show_buttons=TRUE,
                                round_col = 2:(ncol( vals_portof_coef$baza_selectata_final) - 1),
                                caption = paste0(
                                  "Evolutie rata de transformare in cereri plata a garantiilor in sold la data de ",
                                  as.character(input$select_year_portof) )  )    }
        else {
          dt_generate_function(
            df = cbind(   vals_portof_coef$tabel_rata_cerere_plata[, 1:2],
                          vals_portof_coef$tabel_rata_cerere_plata[, 3:ncol(vals_portof_coef$tabel_rata_cerere_plata)] /
                            vals_portof_coef$tabel_rata_cerere_plata$expunere),
            round_col = 2, show_buttons = TRUE,
            perc_col = 3:ncol(vals_portof_coef$tabel_rata_cerere_plata),
            caption = paste0(
              "Rata de trasformare in cerere de plata a garantiilor in sold la ",
              as.character(input$select_year_portof)  )  )  } 
      })
      
      
      output$down_rata_cereri <- downloadHandler(filename = function(){ paste0("migration_from_",input$select_year_portof,".csv")},
                                                 content = function(file) { readr::write_csv(x = vals_portof_coef$baza_selectata_final,file = file) })
      
      
    })
    
   
    
    
  })
    
}
    
## To be copied in the UI
# mod_coeficienti_portofoliu_ui("coeficienti_portofoliu_ui_1")
    
## To be copied in the server
# mod_coeficienti_portofoliu_server("coeficienti_portofoliu_ui_1")

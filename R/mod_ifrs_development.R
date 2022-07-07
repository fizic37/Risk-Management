#' ifrs_development UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ifrs_development_ui <- function(id){
  ns <- NS(id)
  tagList( br(),
           fluidRow( bs4Dash::box(title = "Model 1Y",status = "info",width = 6,
                                  footer = "Model 1Y genereaza probabilitatile de default la un an. 
                  Au fost generate 2 modele, unul pentru beneficiarii raportati in CRC
                    si unul pentru restul, avand in vedere ca informatiile din CRC au o pondere semnificativa
                                              in estimarea riscului de default",
                                  bs4Dash::accordion(id = ns("accordion_1Y"),
                                                     bs4Dash::accordionItem(
                                                       title = "Training and test data dimensions",
                                                       status = "info",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("sumar_dezvoltare_model_1Y")),
                                                       br(),
                                                       fillRow(flex = c(1,NA),
                                                               downloadButton(ns("down_train1Y_crc"),
                                                                              label = "Training 1Y CRC"),
                                                               downloadButton(ns("down_test1Y_crc"),
                                                                              label = "Testing 1Y CRC"),
                                                               downloadButton(ns("down_train1Y_no_crc"),
                                                                              label = "Training NO crc"),
                                                               downloadButton(ns("down_test1Y_no_crc"),
                                                                              label = "Testing NO crc"))
                                                     ),
                                                     bs4Dash::accordionItem(
                                                       title = "AUC for the 2 models",
                                                       status = "info",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("sumar_rezultate_model_1Y"))),
                                                     bs4Dash::accordionItem(
                                                       title = "Model coefficients with CRC",
                                                       status = "info",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("coeficienti_model_1Y_with_CRC"))),
                                                     bs4Dash::accordionItem(
                                                       title = "Model coefficients NO CRC",
                                                       status = "info",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("coeficienti_model_1Y_no_crc"))),
                                                     bs4Dash::accordionItem(
                                                       title = "Mean encode 1Y",
                                                       status = "info",collapsed = TRUE,
                                                       DT::dataTableOutput(ns("mean_encode_1Y"))))
           ),
           bs4Dash::box(title = "Model 3Y",status = "info",width = 6,
                        footer = "Model 3Y genereaza probabilitatile de default la 3 ani. 
                  Au fost generate 2 modele, unul pentru beneficiarii raportati in CRC
                    si unul pentru restul, avand in vedere ca informatiile din CRC au o pondere semnificativa
                                              in estimarea riscului de default",
                        bs4Dash::accordion(id = ns("accordion_3Y"),
                                           bs4Dash::accordionItem(
                                             title = "Training and test data dimensions",
                                             status = "info",collapsed = TRUE,
                                             DT::dataTableOutput(ns("sumar_dezvoltare_model_3Y"))),
                                           bs4Dash::accordionItem(
                                             title = "AUC for the 2 models",
                                             status = "info",collapsed = TRUE,
                                             DT::dataTableOutput(ns("sumar_rezultate_model_3Y"))),
                                           bs4Dash::accordionItem(
                                             title = "Model coefficients with CRC",
                                             status = "info",collapsed = TRUE,
                                             DT::dataTableOutput(ns("coeficienti_model_3Y_with_CRC"))),
                                           bs4Dash::accordionItem(
                                             title = "Model coefficients NO CRC",
                                             status = "info",collapsed = TRUE,
                                             DT::dataTableOutput(ns("coeficienti_model_3Y_no_crc"))),
                                           bs4Dash::accordionItem(title = "Mean encode 3Y",
                                                                  status = "info",collapsed = TRUE,
                                                                  DT::dataTableOutput(ns("mean_encode_3Y")))
                        )
           )
           
           )
  )
  
}
    
#' ifrs_development Server Functions
#'
#' @noRd 
mod_ifrs_development_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    modelling_results <- readRDS(file = "R/reactivedata/ifrs/modelling_results.rds" )
    
    output$sumar_dezvoltare_model_1Y <-   DT::renderDataTable({
        dt_generate_function(  round_col = 2,
          df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator, pattern = "nr_observatii")) %>%
            dplyr::filter(stringr::str_detect(indicator, pattern = "1Y")) )     })
    
    output$sumar_rezultate_model_1Y <- DT::renderDataTable({
      
      dt_generate_function(digits = 2,  round_col = 2,
                           df = modelling_results %>% dplyr::filter(stringr::str_detect(indicator, pattern = "auc")) %>% 
                             dplyr::filter(stringr::str_detect(indicator, pattern = "1Y"))
      )
    })
  })
}
    
## To be copied in the UI
# mod_ifrs_development_ui("ifrs_development_ui_1")
    
## To be copied in the server
# mod_ifrs_development_server("ifrs_development_ui_1")

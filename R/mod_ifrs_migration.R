#' ifrs_migration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ifrs_migration_ui <- function(id){
  ns <- NS(id)
 
   fluidPage(  br(),
  fluidRow( 
  column(width = 4,
         shinyWidgets::airDatepickerInput(inputId = ns("migration_from_ifrs"), label = "Migration from:",width = "300px",
                     value = as.Date("2022-03-31"),autoClose = TRUE,language = "ro")),
  column(width = 4, 
         shinyWidgets::airDatepickerInput(inputId = ns("migration_to_ifrs"),    label = "Migration to:", 
        value = as.Date("2022-06-30"),autoClose = TRUE,language = "ro")),
  
  column(width = 4, br(), uiOutput(outputId = ns("error_message"))),
  DT::dataTableOutput(ns("portfolio_migration"))
  
  )
  )

}
    
#' ifrs_migration Server Functions
#'
#' @noRd 
mod_ifrs_migration_server <- function(id, database_ifrs, ifrs_dates ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    vals_ifrs <- reactiveValues()
    
    observeEvent(ifrs_dates,{
    shinyWidgets::updateAirDateInput(session = session,inputId = "migration_from_ifrs",
                                    value = ifrs_dates[2])
    
    shinyWidgets::updateAirDateInput(session = session,inputId = "migration_to_ifrs",
                                     value = ifrs_dates[1])
    })
    
    database_ifrs_prelucrata <- eventReactive(database_ifrs,{
      
      database_ifrs %>% dplyr::mutate(stage = stage_prob_max) %>% # I use stage_prob_max for migration
           dplyr::mutate(stage = ifelse(stage=="stage1",1,ifelse(stage=="stage2",2,3))) %>% # I process because I use below stage1 as 1
        dplyr::mutate( Expunere = 1000000*expunere_mil_lei ) %>% 
        dplyr::select("Cod Partener",Beneficiar, contragarantii, data_raport, Expunere, stage)
      })
    
    portofoliu_perioada_curenta <- eventReactive(input$migration_to_ifrs, { req(database_ifrs_prelucrata())
      database_ifrs_prelucrata() %>% dplyr::filter(data_raport == input$migration_to_ifrs) %>%
        dplyr::rename_at(.vars = c("Expunere", "stage"), ~paste0(c("Expunere_","stage_"),input$migration_to_ifrs))
      
    })
    
    
    portofoliu_perioada_anterioara <- eventReactive(input$migration_from_ifrs, { 
      req( database_ifrs_prelucrata(), portofoliu_perioada_curenta() ) 
      database_ifrs_prelucrata() %>% dplyr::filter(data_raport == input$migration_from_ifrs) %>%
        dplyr::rename_at(.vars = c("Expunere", "stage"), ~paste0(c("Expunere_","stage_"),input$migration_from_ifrs)) %>%
      dplyr::left_join(y= dplyr::select( portofoliu_perioada_curenta(),"Cod Partener",
                dplyr::starts_with("Expunere"),  dplyr::starts_with("stage")), by = "Cod Partener")
    })
    
    observeEvent(c( portofoliu_perioada_curenta(),portofoliu_perioada_anterioara() ),{
      shiny::validate(shiny::need(expr = nrow(portofoliu_perioada_curenta()) > 0,
                                  message = paste0("Nu am baza de date IFRS la data de ", input$migration_to_ifrs)))
      
      shiny::validate(shiny::need( nrow(portofoliu_perioada_anterioara()) > 0,
            message = paste0("Nu am baza de date IFRS la data de ", input$migration_from_ifrs)))
      # I need to define below column names because they are reactive due to rename_col function applied to the list of dataframes
      # I will need to process with dplyr these column names
      vals_ifrs$stage_anterior <- paste0("stage_",input$migration_from_ifrs)
      vals_ifrs$stage_curent <- paste0("stage_",input$migration_to_ifrs)
      vals_ifrs$expunere_anterioara <- paste0("Expunere_",input$migration_from_ifrs)
      vals_ifrs$expunere_curenta <- paste0("Expunere_",input$migration_to_ifrs)
      
      vals_ifrs$tabel_brut_migration_expunere <- portofoliu_perioada_anterioara() %>% 
        dplyr::group_by(!!rlang::sym(vals_ifrs$stage_anterior),    !!rlang::sym(vals_ifrs$stage_curent)) %>% 
        dplyr::summarise("vals_ifrs$expunere_anterioara" = sum(!!rlang::sym(vals_ifrs$expunere_anterioara)),
          'vals_ifrs$expunere_curenta'=sum(!!rlang::sym(vals_ifrs$expunere_curenta))) %>% dplyr::ungroup() %>%
        dplyr::rename_at(.vars = 3:4,.funs = ~c(vals_ifrs$expunere_anterioara,vals_ifrs$expunere_curenta)) %>%
        dplyr::mutate(Derecunoscute_rambursate = ifelse(is.na(.[[2]]),-.[[3]],.[[4]] - .[[3]]),
                      Transferuri_stage3 = ifelse(.[[2]] == 3 & .[[1]] != 3,   .[[4]],  0),
                      Transferuri_from_stage3 = ifelse(.[[2]] == 3 & .[[1]] != 3, -.[[4]],  0),
                      Transferuri_stage2 = ifelse(.[[2]] == 2 & .[[1]] != 2,  .[[4]],    0),
                      Transferuri_from_stage2 = ifelse(.[[2]] == 2 & .[[1]] != 2, -.[[4]],  0),
                      Transferuri_stage1 = ifelse(.[[2]] == 1 & .[[1]] != 1, .[[4]],    0),
                      Transferuri_from_stage1 = ifelse(.[[2]] == 1 & .[[1]] != 1,  -.[[4]],  0 ))
      
      vals_ifrs$tabel1_migration <-  vals_ifrs$tabel_brut_migration_expunere  %>% 
        dplyr::group_by(!!rlang::sym(vals_ifrs$stage_anterior)) %>% 
        dplyr::summarise(Sold_initial = sum(!!rlang::sym(vals_ifrs$expunere_anterioara)),
                         Derecunoscute_rambursate=sum(Derecunoscute_rambursate),
                         Transfers_stage3 = sum(Transferuri_from_stage3,na.rm=T),   
                         Transfers_stage2=sum(Transferuri_from_stage2,na.rm=T),
                         Transfers_stage1 = sum(Transferuri_from_stage1,na.rm = TRUE)) %>% tidyr::pivot_longer(cols = -1)   %>% 
        tidyr::pivot_wider(names_from =  !!rlang::sym(vals_ifrs$stage_anterior),names_prefix = "stage_")
      
      # I get my transfers from a category to another one (the same table as table1_migration but with minus exposures)
      vals_ifrs$tabel2_migration <- vals_ifrs$tabel_brut_migration_expunere  %>% dplyr::filter(!is.na(.[[2]])) %>% 
        dplyr::group_by(!!rlang::sym(vals_ifrs$stage_curent)) %>% 
        dplyr::summarise(Transfers_stage3 = sum(Transferuri_stage3,na.rm=T), 
                         Transfers_stage2 = sum(Transferuri_stage2,na.rm = T),
                         Transfers_stage1 = sum(Transferuri_stage1,na.rm = TRUE)) %>% 
        tidyr::pivot_longer(cols = -1)   %>% tidyr::pivot_wider(names_from =  !!rlang::sym(vals_ifrs$stage_curent),
                                                                names_prefix = "stage_")
      
      # I get category of my new exposures categories
      vals_ifrs$tabel3_new_exposures_migration <- dplyr::left_join(x = portofoliu_perioada_curenta(),
        y = dplyr::select( portofoliu_perioada_anterioara(),`Cod Partener`,!!rlang::sym(vals_ifrs$expunere_anterioara),
            !!rlang::sym(vals_ifrs$stage_anterior)),by="Cod Partener") %>% 
        dplyr::filter(is.na(!!rlang::sym(vals_ifrs$stage_anterior))) %>% 
        dplyr::group_by(!!rlang::sym(vals_ifrs$stage_curent)) %>% 
        dplyr::summarise(Acordate_efectuate_in_timpul_anului=sum(!!rlang::sym(vals_ifrs$expunere_curenta))) %>% 
        tidyr::pivot_longer(cols = -1) %>% tidyr::pivot_wider(names_from =  !!rlang::sym(vals_ifrs$stage_curent),
                                                              names_prefix = "stage_")
      
      # I produce my final migration_table
      vals_ifrs$tabel_migration_final <- dplyr::bind_rows(vals_ifrs$tabel1_migration,
          vals_ifrs$tabel2_migration,vals_ifrs$tabel3_new_exposures_migration) %>% 
        dplyr::group_by(name) %>% 
        dplyr:: summarise_all(.funs = ~sum(.,na.rm=T)) %>% dplyr::slice(c(3,1,2,4:6)) %>% dplyr::mutate(Total=rowSums(.[2:4])) %>% 
        dplyr::bind_rows(apply(X = dplyr::select(.,-1),MARGIN = 2,FUN=sum)) %>% tidyr::replace_na(replace = list(name="Total")) %>% 
        dplyr::rename_at(.vars = 1,~'Miscari_in_timpul_Perioadei')
      
      output$portfolio_migration <- DT::renderDataTable({
        req( nrow(portofoliu_perioada_curenta()) > 0, nrow(portofoliu_perioada_anterioara()) > 0)
        dt_generate_function(df=vals_ifrs$tabel_migration_final,
            round_col = 2:5,show_buttons = TRUE,
            caption = "Migratia portofoliului de garantii in functie de categoria contaminata a beneficiarului:")  })
     
    })
    
     
    output$error_message <- renderUI( { req(any(nrow(portofoliu_perioada_curenta()) == 0, 
                nrow(portofoliu_perioada_anterioara()) == 0))
      
      div(style="color: #c92052; padding-top: 12px;",
          paste0("Nu exista info IFRS9 pentru data selectata")) })
  
    
    
    
  })
}
    
## To be copied in the UI
# mod_ifrs_migration_ui("ifrs_migration_ui_1")
    
## To be copied in the server
# mod_ifrs_migration_server("ifrs_migration_ui_1")

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
         selectInput(inputId = ns("migration_from_ifrs"), label = "Migration from:",width = "300px",
                     choices = as.Date("2018-12-31"),
                     selected = as.Date("2018-12-31"))),
  column(width = 4, 
         selectInput(inputId = ns("migration_to_ifrs"),    label = "Migration to:", 
                     choices = as.Date("2019-12-31"), width="300px",
                     selected = as.Date("2019-12-31"))),
  column(width = 4, br(), actionButton(inputId = ns("start_migration"),width = "250px",
                                       label = "Perform portfolio migration",icon = icon("play"))),
  DT::dataTableOutput(ns("portfolio_migration"))
  )
  )

}
    
#' ifrs_migration Server Functions
#'
#' @noRd 
mod_ifrs_migration_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    portofoliu_ifrs <- readRDS("R/reactivedata/ifrs/portofoliu_ifrs.rds")
    
    vals_ifrs <- reactiveValues(portofoliu_ifrs = portofoliu_ifrs)
    
    observeEvent(input$start_migration,{
      vals_portofoliu$lista_provizion_non_ifrs <- readRDS("R/reactivedata/portofoliu/portof_database.rds") %>% 
        dplyr::group_by(`Cod Partener`,anul_de_raportare,categorie_contaminata) %>% 
        dplyr::summarise(expunere = sum(expunere),provizion_contabil=sum(provizion_contabil)) %>% dplyr::ungroup() %>%
        dplyr::group_split(anul_de_raportare,.keep=TRUE) %>% purrr::map(~rename_col_nonifrs(.x))
      
      vals_portofoliu$portofoliu_perioada_curenta <- vals_portofoliu$lista_provizion_non_ifrs %>% 
        purrr::keep(.p = ~unique(.x$anul_de_raportare) == input$migration_to_nonifrs) %>% purrr::flatten_df()
      
      
      vals_portofoliu$portofoliu_perioada_anterioara <- vals_portofoliu$lista_provizion_non_ifrs %>% 
        purrr::keep(.p = ~unique(.x$anul_de_raportare) == input$migration_from_nonifrs) %>% purrr::flatten_df() %>% 
        dplyr::left_join(y= dplyr::select(vals_portofoliu$portofoliu_perioada_curenta,
                                          dplyr::matches("Expunere|Cod Partener|categorie_contaminata|provizion_contabil")), by = "Cod Partener")
      
      # I need to define below column names because they are reactive due to rename_col function applied to the list of dataframes
      # I will need to process with dplyr these column names
      vals_portofoliu$categorie_anterioara <- paste0("categorie_contaminata_",input$migration_from_nonifrs)
      vals_portofoliu$categorie_curenta <- paste0("categorie_contaminata_",input$migration_to_nonifrs)
      vals_portofoliu$expunere_anterioara <- paste0("Expunere_",input$migration_from_nonifrs)
      vals_portofoliu$expunere_curenta <- paste0("Expunere_",input$migration_to_nonifrs)
      vals_portofoliu$provizion_anterior <- paste0("Provizion_contabil_", input$migration_from_nonifrs)
      vals_portofoliu$provizion_curent <- paste0("Provizion_contabil_",input$migration_to_nonifrs)
      
      # I calculate my main indicators here of exposure migration
      vals_portofoliu$tabel_brut_migration_expunere <- vals_portofoliu$portofoliu_perioada_anterioara %>% 
        dplyr::group_by(!!rlang::sym(vals_portofoliu$categorie_anterioara),    !!rlang::sym(vals_portofoliu$categorie_curenta)) %>% 
        dplyr::summarise("vals_portofoliu$expunere_anterioara" = sum(!!rlang::sym(vals_portofoliu$expunere_anterioara)),
                         'vals_portofoliu$expunere_curenta'=sum(!!rlang::sym(vals_portofoliu$expunere_curenta))) %>% dplyr::ungroup() %>%
        dplyr::rename_at(.vars = 3:4,.funs = ~c(vals_portofoliu$expunere_anterioara,vals_portofoliu$expunere_curenta)) %>%
        dplyr::mutate(Derecunoscute_rambursate = ifelse(is.na(.[[2]]),-.[[3]],.[[4]] - .[[3]]),
                      Transferuri_cerere_plata = ifelse(.[[2]] == 'cerere_plata' & .[[1]] != 'cerere_plata',   .[[4]],  0),
                      Transferuri_from_cerere_plata = ifelse(.[[2]] == 'cerere_plata' & .[[1]] != 'cerere_plata', -.[[4]],  0),
                      Transferuri_insolventa = ifelse(.[[2]] == 'insolventa' & .[[1]] != 'insolventa',  .[[4]],    0),
                      Transferuri_from_insolventa = ifelse(.[[2]] == 'insolventa' & .[[1]] != 'insolventa', -.[[4]],  0),
                      Transferuri_instiintare = ifelse(.[[2]] == 'instiintare_neplata' & .[[1]] != 'instiintare_neplata', .[[4]],    0),
                      Transferuri_from_instiintare = ifelse(.[[2]] == 'instiintare_neplata' & .[[1]] != 'instiintare_neplata',  -.[[4]],    0 ),
                      Transferuri_standard = ifelse(.[[2]] == 'standard' & .[[1]] != 'standard', .[[4]],    0),
                      Transferuri_from_standard = ifelse(.[[2]] == 'standard' & .[[1]] != 'standard',  -.[[4]],    0 ))
      
      
      # I get my transfers to a category (from another one)
      vals_portofoliu$tabel1_migration <-  vals_portofoliu$tabel_brut_migration_expunere  %>% 
        dplyr::group_by(!!rlang::sym(vals_portofoliu$categorie_anterioara)) %>% 
        dplyr::summarise(Sold_initial = sum(!!rlang::sym(vals_portofoliu$expunere_anterioara)),
                         Derecunoscute_rambursate=sum(Derecunoscute_rambursate),
                         Transfers_cerere_plata = sum(Transferuri_from_cerere_plata,na.rm=T),   
                         Transfers_insolventa=sum(Transferuri_from_insolventa,na.rm=T),
                         Transfers_instiintare=sum(Transferuri_from_instiintare,na.rm=T),
                         Transfers_standard = sum(Transferuri_from_standard,na.rm = TRUE)) %>% tidyr::pivot_longer(cols = -1)   %>% 
        tidyr::pivot_wider(names_from =  !!rlang::sym(vals_portofoliu$categorie_anterioara),names_prefix = "categorie_contaminata_") 
      
      # I get my transfers from a category to another one (the same table as table1_migration but with minus exposures)
      vals_portofoliu$tabel2_migration <- vals_portofoliu$tabel_brut_migration_expunere  %>% dplyr::filter(!is.na(.[[2]])) %>% 
        dplyr::group_by(!!rlang::sym(vals_portofoliu$categorie_curenta)) %>% 
        dplyr::summarise(Transfers_cerere_plata = sum(Transferuri_cerere_plata,na.rm=T), 
                         Transfers_insolventa = sum(Transferuri_insolventa,na.rm = T),
                         Transfers_instiintare=sum(Transferuri_instiintare,na.rm=T),
                         Transfers_standard = sum(Transferuri_standard,na.rm = TRUE)) %>% 
        tidyr::pivot_longer(cols = -1)   %>% tidyr::pivot_wider(names_from =  !!rlang::sym(vals_portofoliu$categorie_curenta),
                                                                names_prefix = "categorie_contaminata_")
      
      # I get category of my new exposures categories
      vals_portofoliu$tabel3_new_exposures_migration <- dplyr::left_join(x = vals_portofoliu$portofoliu_perioada_curenta,
                                                                         y = dplyr::select(vals_portofoliu$portofoliu_perioada_anterioara,`Cod Partener`,!!rlang::sym(vals_portofoliu$expunere_anterioara),
                                                                                           !!rlang::sym(vals_portofoliu$categorie_anterioara)),by="Cod Partener") %>% 
        dplyr::filter(is.na(!!rlang::sym(vals_portofoliu$categorie_anterioara))) %>% 
        dplyr::group_by(!!rlang::sym(vals_portofoliu$categorie_curenta)) %>% 
        dplyr::summarise(Acordate_efectuate_in_timpul_anului=sum(!!rlang::sym(vals_portofoliu$expunere_curenta))) %>% 
        tidyr::pivot_longer(cols = -1) %>% tidyr::pivot_wider(names_from =  !!rlang::sym(vals_portofoliu$categorie_curenta),
                                                              names_prefix = "categorie_contaminata_")
      
      # I produce my final migration_table
      vals_portofoliu$tabel_migration_final <- dplyr::bind_rows(vals_portofoliu$tabel1_migration,
                                                                vals_portofoliu$tabel2_migration,vals_portofoliu$tabel3_new_exposures_migration) %>% 
        dplyr::group_by(name) %>% 
        dplyr:: summarise_all(.funs = ~sum(.,na.rm=T)) %>% dplyr::slice(c(3,1,2,4:6)) %>% dplyr::mutate(Total=rowSums(.[2:5])) %>% 
        dplyr::bind_rows(apply(X = dplyr::select(.,-1),MARGIN = 2,FUN=sum)) %>% tidyr::replace_na(replace = list(name="Total")) %>% 
        dplyr::rename_at(.vars = 1,~'Miscari_in_timpul_Perioadei')
      
      output$portfolio_migration <- DT::renderDataTable({dt_generate_function(df=vals_portofoliu$tabel_migration_final,
                                                                              round_col = 2:6,show_buttons = TRUE,
                                                                              caption = "Migratia portofoliului de garantii in functie de categoria contaminata a beneficiarului:")  })
    })
    
    
    
  })
}
    
## To be copied in the UI
# mod_ifrs_migration_ui("ifrs_migration_ui_1")
    
## To be copied in the server
# mod_ifrs_migration_server("ifrs_migration_ui_1")

#' regularizare_portofoliu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_regularizare_portofoliu_ui <- function(id){
  ns <- NS(id)
   fluidRow(
                 column(width = 6,  shinyWidgets::airDatepickerInput( inputId = ns("migration_from_nonifrs"),
                              label = "Migration from:", width = "300px",
                              value =   as.Date("2020-12-31"),view = "months",autoClose = T,language = "ro") ),
                       
                 column(width = 6,   shinyWidgets::airDatepickerInput(inputId = ns("migration_to_nonifrs"), 
                              label = "Migration to:", width = "300px",
                                    value =   as.Date("2021-12-31"),view = "months",autoClose = T,language = "ro")),
               
                 
                 column(width = 4, shinyWidgets::downloadBttn(outputId =ns("down_intrari_deprec"),label = 
                                  "Download intrari depreciate", style = "stretch",color = "success" ) ),
                 column(width = 4, shinyWidgets::downloadBttn(outputId =ns("down_iesiri_deprec"),label = 
                                  "Download iesiri depreciate", style = "stretch",color = "success" ) ),
                 column(width = 4 , shinyWidgets::actionBttn(inputId = ns("start_migration"),
                         label = "Genereaza matricea de migratie a portofoliului",
                          icon = icon("play"),style = "stretch",color = "danger")),
                
                DT::dataTableOutput(ns("portfolio_migration")), br(),
                uiOutput(outputId = ns("show_down_button"))
               
  )
}
    
#' regularizare_portofoliu Server Functions
#'
#' @noRd 
mod_regularizare_portofoliu_server <- function(id, vals_portofoliu){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    portofoliu_database <- readRDS("R/external_volumes/portofoliu/portof_database.rds")
    
    
    ## Observer to update migration dates
    observeEvent(vals_portofoliu$view_portofoliu,{
      vals_portofoliu$unique_dates <-  vals_portofoliu$view_portofoliu$anul_de_raportare
      
      shinyWidgets::updateAirDateInput(session, 'migration_from_nonifrs',value = vals_portofoliu$unique_dates [2] )
      
      shinyWidgets::updateAirDateInput(session, 'migration_to_nonifrs',value = vals_portofoliu$unique_dates [1] )
      
      })
    
    
    # Produce intrari/iesiri depreciate - la nivel de contract
    
    vals_portofoliu$depreciate_curenta <- portofoliu_database %>% 
      dplyr::filter(anul_de_raportare == input$migration_to_nonifrs, categorie_contaminata != "standard")
    
    vals_portofoliu$depreciate_anterioara <- portofoliu_database %>% 
      dplyr::filter(anul_de_raportare == input$migration_from_nonifrs, categorie_contaminata != "standard")
    
    bi_cui_existent <- readRDS("R/reactivedata/bi_cui.rds")
    
    
    output$down_intrari_deprec <-  downloadHandler( filename = function() { "intrari_deprec.csv"  },
                        content = function(file) { readr::write_csv(   file = file,
           x =  vals_portofoliu$depreciate_curenta %>%
           dplyr::filter(  !DocumentId %in% vals_portofoliu$depreciate_anterioara$DocumentId   ) %>%
           dplyr::select(  Banca,    Beneficiar,   DocumentId,  DocumentNumber = `Nr contract`, `Cod Partener` ) %>%
            dplyr::left_join(bi_cui_existent, by = c("Cod Partener" = "Code")) %>%
            dplyr::select(Banca, Beneficiar, DocumentId, DocumentNumber, CUI = `Unique ID`)   )    }  )
    
    output$down_iesiri_deprec <- downloadHandler( filename = function() { "iesiri_deprec.csv"  },
                            content = function(file) {  readr::write_csv(   file = file,
            x =  vals_portofoliu$depreciate_anterioara %>%
            dplyr::filter( !DocumentId %in% vals_portofoliu$depreciate_curenta$DocumentId ) %>%
            dplyr::select(  Banca, Beneficiar, DocumentId, DocumentNumber = `Nr contract`, `Cod Partener` ) %>%
            dplyr::left_join(bi_cui_existent, by = c("Cod Partener" = "Code")) %>%
            dplyr::select(Banca, Beneficiar, DocumentId, DocumentNumber, CUI = `Unique ID`) )  }  )
    
    
    #### MIGRATION
    
    
    observeEvent( input$start_migration,{
      
      # I wrap portofolio migartion inside tryCatch, I do not want to risk loosing intrari/iesiri depreciate
      
      tryCatch(expr = {
        
        vals_portofoliu$lista_provizion_non_ifrs <- portofoliu_database %>% 
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
        
        
        ### Provisions Migration - not finished
        # This table produces provision migration for all CUI that are in perioada curenta (it does not matter if they are in perioada anterioara or not)
        vals_portofoliu$tabel_variatie_provizioane <- dplyr::left_join(x = dplyr::select(vals_portofoliu$portofoliu_perioada_curenta,
                                                                                         `Cod Partener`,   !!rlang::sym(vals_portofoliu$categorie_curenta), !!rlang::sym(vals_portofoliu$provizion_curent)), 
                                                                       y = dplyr::select(vals_portofoliu$portofoliu_perioada_anterioara,`Cod Partener`,!!rlang::sym(vals_portofoliu$categorie_anterioara),
                                                                                         !!rlang::sym(vals_portofoliu$provizion_anterior)),by="Cod Partener") %>%
          dplyr::mutate_at(.vars = 5,~replace(x = .,list = which(is.na(.)),values = 0)) %>% 
          dplyr::mutate(variatie_provizion = .[[3]] - .[[5]])
        
        # This table produces provisions for all CUI that are in perioada_anterioara and are not in perioada curenta
        vals_portofoliu$tabel_beneficiari_iesiti <- dplyr::left_join(y = dplyr::select(vals_portofoliu$portofoliu_perioada_curenta,`Cod Partener`,
                                                                                       !!rlang::sym(vals_portofoliu$categorie_curenta),!!rlang::sym(vals_portofoliu$provizion_curent)),
                                                                     x = dplyr::select(vals_portofoliu$portofoliu_perioada_anterioara,`Cod Partener`,
                                                                                       !!rlang::sym(vals_portofoliu$categorie_anterioara), !!rlang::sym(vals_portofoliu$provizion_anterior)),
                                                                     by = "Cod Partener") %>%  dplyr::filter(is.na(!!rlang::sym(vals_portofoliu$categorie_curenta))) %>% 
          dplyr::mutate(variatie_provizion = .[[3]] * (-1))
        
        # I produce my final regularisation provisions
        vals_portofoliu$regularizare_provizioane_non_ifrs <- dplyr::bind_rows(vals_portofoliu$tabel_beneficiari_iesiti,
                                  vals_portofoliu$tabel_variatie_provizioane)
        
        
        output$show_down_button <- renderUI({ req( vals_portofoliu$regularizare_provizioane_non_ifrs )
                   shinyWidgets::downloadBttn(outputId = ns("down_regularizare_portof"),label = 
            "Regularizarea provizioanelor depreciate", style = "stretch",color = "success") })
        
        output$down_regularizare_portof <- downloadHandler(filename = function() {"regularizare_proviz_deprec.csv"},
          content = function(file) {readr::write_csv(x =  vals_portofoliu$regularizare_provizioane_non_ifrs, file = file)})
        
        
      }, error = function(e) {output$portfolio_migration <- DT::renderDataTable({dt_generate_function(
        df=data.frame(), caption = "Sorry, ceva nu a mers bine cand am incercat sa produc matricea de migratie")  }) } )
      
    })
 
  })
}
    
## To be copied in the UI
# mod_regularizare_portofoliu_ui("regularizare_portofoliu_1")
    
## To be copied in the server
# mod_regularizare_portofoliu_server("regularizare_portofoliu_1")

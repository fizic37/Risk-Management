#' portofoliu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_portofoliu_ui <- function(id){
  ns <- NS(id)
  fluidPage( selectInput(inputId = ns("select_year_portof"), label = "Selecteaza data raportului",
                           choices = as.Date("2019-12-31"),
                           selected = as.Date("2019-12-31"),width = "300px"),
               DT::dataTableOutput(ns("rata_cereri_plata")),
               br(),
               fillRow(flex = c(1,NA),actionButton(inputId = ns("view_rata_cereri"),label = "Change view - percentage/brute",
                                                   icon = icon("toggle-on"),width = "300px"),
                       downloadButton(outputId = ns("down_rata_cereri"), label = "Download baza detaliata")),
               br(),
               br(),
               DT::dataTableOutput(ns("audit_rata_cereri_table")))
         
}
    
#' portofoliu Server Function
#'
#' @noRd 
mod_portofoliu_server <- function(input, output, session){
  ns <- session$ns
  
  vals <- reactiveValues()
  #observe({if (nrow(vals$portofoliu_database)==0) {portof_database <- readRDS(file = "R/reactivedata/portof_database.rds")}
    
    #else {portof_database <- vals$portofoliu_database}})
  
  #portof_database <- readRDS(file = 'R/reactivedata/portof_database.rds')
  
  #vals$portofoliu_database<- portof_database
  
  #observe({vals$portofoliu_database <- portof_database})
  
  #output$diverse_portofoliu <- renderPrint({str(vals$portofoliu_database)})
  
  baza_citita <-  portof_database %>%  dplyr::filter(anul_de_raportare > as.Date("2009-12-31"))
  
  load('data/cereri_plata.rda')
  #cereri_plata <- readRDS(file = 'R/cereri_plata.rds')
  
  ani_raportare_portofoliu <- unique(baza_citita$anul_de_raportare) %>% sort()
  
  choices_data_indicatori_cereri_plata <- ani_raportare_portofoliu[which(ani_raportare_portofoliu<=max(cereri_plata$Data_cerere_plata))]
  
  updateSelectInput(session = session,inputId ='select_year_portof',choices = choices_data_indicatori_cereri_plata,
                    selected = max(choices_data_indicatori_cereri_plata))
  
  sinteza_portofoliu <- baza_citita %>% dplyr::group_by(anul_de_raportare,categorie_contaminata) %>% dplyr::summarise(expunere=sum(expunere)) %>%
    tidyr::pivot_wider(names_from = categorie_contaminata,values_from = expunere) %>% dplyr::ungroup()  %>%
    dplyr::mutate(Total_sold=rowSums(x = dplyr::select(.,-1),na.rm = TRUE))
  
  
  
  sinteza_portofoliu_procentual <- cbind(sinteza_portofoliu[,1],sinteza_portofoliu[,2:6]/sinteza_portofoliu$Total_sold)
  
  sinteza_portofoliu_provizion <-  baza_citita %>% dplyr::group_by(anul_de_raportare,categorie_contaminata) %>% 
    dplyr::summarise(provizion_contabil=sum(provizion_contabil)) %>% tidyr::pivot_wider(names_from = categorie_contaminata,values_from = provizion_contabil) %>% 
    dplyr::ungroup()  %>%   dplyr::mutate(Total_Provizion_Contabil=rowSums(x = dplyr::select(.,-1),na.rm = TRUE))
  
  
  
  output$sumar_portofoliu <- DT::renderDataTable({
    if(is.null(input$view_sumar_portofoliu) || input$view_sumar_portofoliu%%3==0) {
      dt_generate_function(df=sinteza_portofoliu,round_col = 2:6,caption = "Sinteza baza date portofoliu, expuneri:")}
    
    else if (input$view_sumar_portofoliu%%2==0) {dt_generate_function(sinteza_portofoliu_provizion,
                              round_col = 2:6,caption = "Sinteza baza date portofoliu, provizioane:") }
    
    else {dt_generate_function(sinteza_portofoliu_procentual,round_col = NULL,perc_col = 2:6,
                               caption = "Sinteza baza date portofoliu, pondere expuneri:")}    })
  
  
  
  cereri_plata <- cereri_plata %>% dplyr::mutate(an_cerere_plata = lubridate::year(Data_cerere_plata)) %>% 
    
    dplyr::select(DocumentId,`Cod Partener`,Cerere_Plata,Data_cerere_plata,an_cerere_plata,
                  plata_aprobata, dplyr::matches("litigiu|poprire|somatie")) %>% 
    dplyr::mutate(coef_final = apply(dplyr::select(.data = ., dplyr::contains("coef")), 1, function(x) (ifelse(is.na(x) %>% all(),0,max(x,na.rm = TRUE)))),
                  litigiu_final = apply(dplyr::select(.data = ., poprire_neplatita,   suma_litigiu,    valoare_somatie),1,max,  na.rm = TRUE)) %>% 
    dplyr::mutate(Plata_litigii = ifelse(is.infinite(coef_final) | coef_final <= 0.25, plata_aprobata,  plata_aprobata + litigiu_final)) 
  
  rata_transf_plati <- cereri_plata %>% dplyr::filter(Data_cerere_plata >=as.Date("2011-01-01") & Data_cerere_plata<=as.Date("2019-09-30")) %>% 
    dplyr::summarise(Cereri_plata_primite=sum(Cerere_Plata),Plati_aprobate=sum(plata_aprobata),
                     Plati_si_litiii_necastigate=sum(Plata_litigii))
  
  
 
  
  
  observeEvent(input$select_year_portof,{
    # I produce my rata_trans_plati table
    vals$cereri_plata_filtrata <- cereri_plata %>% dplyr::filter(Data_cerere_plata > input$select_year_portof) %>% 
      dplyr::group_by(`Cod Partener`,an_cerere_plata) %>% 
      dplyr::summarise(Cerere_Plata=sum(Cerere_Plata)) %>% 
      dplyr::mutate(Cerere_Plata_cumulata=cumsum(Cerere_Plata)) %>% dplyr::ungroup()
    
    vals$cereri_plata_splitata <-  split(vals$cereri_plata_filtrata,vals$cereri_plata_filtrata$an_cerere_plata)
    
    
    # I rename columns in cereri_plata_splitata. Rename_col function is to be found in mod_portofoliu_utils
    vals$cereri_plata_splitata_prel <- purrr::map(vals$cereri_plata_splitata,~rename_col(df=.x))
    
    # I get my filtered dataframe for future processing grupped by CUI
    vals$baza_selectata <-  baza_citita[baza_citita$anul_de_raportare==input$select_year_portof,] %>% 
      dplyr::group_by(`Cod Partener`,categorie_contaminata) %>%  dplyr::summarise(expunere=sum(expunere)) 
    
    # I process my filtered dataframe in order to obtain cumulated cerere_Plata according to year.
    # I will not obtain final filtered process because, for example, if a CUI has cereri_plata during years 2015 and 2016
    # below code will produce cumulated cereri_plata in 2015 and 2016 but will only show 0 for 2017 and subsequent years
    vals$baza_selectata_procesata <- purrr::map_df(.x = vals$cereri_plata_splitata_prel,
                                                   .f = ~ dplyr::left_join(x = vals$baza_selectata,  y = dplyr::select(.data = .x, `Cod Partener`, 
                                                                                                                       dplyr::contains("Cerere_Plata_cumulata")), by = "Cod Partener" )) %>%  
      dplyr::select(`Cod Partener`,   categorie_contaminata, dplyr::contains("Cerere_Plata_cumulata")) %>% 
      dplyr::group_by(`Cod Partener`, categorie_contaminata) %>%    
      dplyr::summarise_at(dplyr::vars(-dplyr::group_cols()), sum, na.rm = T) %>% dplyr::ungroup() %>%
      #I add back expunere
      dplyr::left_join(y = vals$baza_selectata[, c("Cod Partener", "expunere")], by = "Cod Partener")
    
    
    
    # I get my final dataframe with cumulated Cerere_plata_cumulata
    vals$baza_selectata_final <- cbind(dplyr::select(vals$baza_selectata_procesata,`Cod Partener`,categorie_contaminata,expunere),
                                       max_col(dplyr::select(vals$baza_selectata_procesata,dplyr::contains("Cerere_plata_cumulata")))) # This cumulates Cere_plata_cumulata
    
    vals$tabel_rata_cerere_plata <-   vals$baza_selectata_final  %>% dplyr::select(-`Cod Partener`) %>% 
      dplyr::filter(categorie_contaminata!="cerere_plata") %>% dplyr::group_by(categorie_contaminata) %>% 
      dplyr::summarise_at(.vars = dplyr::vars(-dplyr::group_cols()),sum) %>% dplyr::ungroup()
    
    output$rata_cereri_plata <- DT::renderDataTable({
      if (is.null(input$view_rata_cereri) || input$view_rata_cereri %% 2 == 0) {
        dt_generate_function(df = vals$tabel_rata_cerere_plata, round_col = 2:(ncol(vals$baza_selectata_final)-1),
                             caption = paste0("Evolutie rata de transformare in cereri plata a garantiilor in sold la data de ",
                                              as.character(input$select_year_portof))) }
      else {dt_generate_function(df = cbind(vals$tabel_rata_cerere_plata[,1:2],
                                            vals$tabel_rata_cerere_plata[,3:ncol(vals$tabel_rata_cerere_plata)]/vals$tabel_rata_cerere_plata$expunere),
                                 round_col = 2,perc_col = 3:ncol(vals$tabel_rata_cerere_plata),
                                 caption = paste0("Rata de trasformare in cerere de plata a garantiilor in sold la ",as.character(input$select_year_portof))) }
    })
    
    output$down_rata_cereri <- downloadHandler(filename = function(){"baza_rata_cereri_plata.csv"},content = function(file){
      readr::write_csv(x = vals$baza_selectata_final,path = file) })
    
  })
  
  ### Migration table
  
  # I have to group baza citita to every CUI
  portof_database_CUI_grouped <- baza_citita %>% dplyr::group_by(`Cod Partener`,anul_de_raportare,categorie_contaminata) %>% 
    dplyr::summarise(expunere = sum(expunere),provizion_contabil=sum(provizion_contabil)) %>% dplyr::ungroup()
  
  lista_provizion_non_ifrs <- split(portof_database_CUI_grouped,portof_database_CUI_grouped$anul_de_raportare) %>% 
    purrr::map(~rename_col_nonifrs(df=.x))
  
  updateSelectInput(session = session,inputId ='migration_from_nonifrs',choices = ani_raportare_portofoliu,
                    selected = ani_raportare_portofoliu[(length(ani_raportare_portofoliu)-1)])
  
  updateSelectInput(session = session,inputId ='migration_to_nonifrs',choices = ani_raportare_portofoliu,
                    selected = max(ani_raportare_portofoliu))
                    
  observe({
    vals$portofoliu_perioada_curenta <- lista_provizion_non_ifrs[which(names(lista_provizion_non_ifrs) == 
                                                                         as.character(input$migration_to_nonifrs))][[1]]
    
    vals$portofoliu_perioada_anterioara <- lista_provizion_non_ifrs[which(names(lista_provizion_non_ifrs) == 
                                                                            as.character(input$migration_from_nonifrs))][[1]] %>%
      dplyr::left_join(y= dplyr::select(vals$portofoliu_perioada_curenta,
                                        dplyr::matches("Expunere|Cod Partener|categorie_contaminata|provizion_contabil")), by = "Cod Partener")
    
    # I need to define below column names because they are reactive due to rename_col function applied to the list of dataframes
    # I will need to process with dplyr these column names
    vals$categorie_anterioara <- paste0("categorie_contaminata_",input$migration_from_nonifrs)
    vals$categorie_curenta <- paste0("categorie_contaminata_",input$migration_to_nonifrs)
    vals$expunere_anterioara <- paste0("Expunere_",input$migration_from_nonifrs)
    vals$expunere_curenta <- paste0("Expunere_",input$migration_to_nonifrs)
    vals$provizion_anterior <- paste0("Provizion_contabil_", input$migration_from_nonifrs)
    vals$provizion_curent <- paste0("Provizion_contabil_",input$migration_to_nonifrs)
    
    # I calculate my main indicators here of exposure migration
    vals$tabel_brut_migration_expunere <- vals$portofoliu_perioada_anterioara %>% dplyr::group_by(!!rlang::sym(vals$categorie_anterioara),
                                                                                                  !!rlang::sym(vals$categorie_curenta)) %>% 
      dplyr::summarise("vals$expunere_anterioara" = sum(!!rlang::sym(vals$expunere_anterioara)),
                       'vals$expunere_curenta'=sum(!!rlang::sym(vals$expunere_curenta))) %>% dplyr::ungroup() %>%
      dplyr::rename_at(.vars = 3:4,.funs = ~c(vals$expunere_anterioara,vals$expunere_curenta)) %>%
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
    vals$tabel1_migration <-  vals$tabel_brut_migration_expunere  %>% dplyr::group_by(!!rlang::sym(vals$categorie_anterioara)) %>% 
      dplyr::summarise(Sold_initial = sum(!!rlang::sym(vals$expunere_anterioara)),Derecunoscute_rambursate=sum(Derecunoscute_rambursate),
                       Transfers_cerere_plata = sum(Transferuri_from_cerere_plata,na.rm=T),   Transfers_insolventa=sum(Transferuri_from_insolventa,na.rm=T),
                       Transfers_instiintare=sum(Transferuri_from_instiintare,na.rm=T),
                       Transfers_standard = sum(Transferuri_from_standard,na.rm = TRUE)) %>% tidyr::pivot_longer(cols = -1)   %>% 
      tidyr::pivot_wider(names_from =  !!rlang::sym(vals$categorie_anterioara),names_prefix = "categorie_contaminata_") 
    
    # I get my transfers from a category to another one (the same table as table1_migration but with minus exposures)
    vals$tabel2_migration <- vals$tabel_brut_migration_expunere  %>% dplyr::filter(!is.na(.[[2]])) %>% 
      dplyr::group_by(!!rlang::sym(vals$categorie_curenta)) %>% 
      dplyr::summarise(Transfers_cerere_plata = sum(Transferuri_cerere_plata,na.rm=T), Transfers_insolventa = sum(Transferuri_insolventa,na.rm = T),
                       Transfers_instiintare=sum(Transferuri_instiintare,na.rm=T),Transfers_standard = sum(Transferuri_standard,na.rm = TRUE)) %>% 
      tidyr::pivot_longer(cols = -1)   %>% tidyr::pivot_wider(names_from =  !!rlang::sym(vals$categorie_curenta),
                                                              names_prefix = "categorie_contaminata_")
    # I get category of my new exposures categories
    vals$tabel3_new_exposures_migration <- dplyr::left_join(x = vals$portofoliu_perioada_curenta,
                                                            y = dplyr::select(vals$portofoliu_perioada_anterioara,`Cod Partener`,!!rlang::sym(vals$expunere_anterioara),
                                                                              !!rlang::sym(vals$categorie_anterioara)),by="Cod Partener") %>% 
      dplyr::filter(is.na(!!rlang::sym(vals$categorie_anterioara))) %>% dplyr::group_by(!!rlang::sym(vals$categorie_curenta)) %>% 
      dplyr::summarise(Acordate_efectuate_in_timpul_anului=sum(!!rlang::sym(vals$expunere_curenta))) %>% 
      tidyr::pivot_longer(cols = -1) %>% tidyr::pivot_wider(names_from =  !!rlang::sym(vals$categorie_curenta),
                                                            names_prefix = "categorie_contaminata_")
    # I produce my final migration_table
    vals$tabel_migration_final <- dplyr::bind_rows(vals$tabel1_migration,vals$tabel2_migration,vals$tabel3_new_exposures_migration) %>% 
      dplyr::group_by(name) %>% 
      dplyr:: summarise_all(.funs = ~sum(.,na.rm=T)) %>% dplyr::slice(c(3,1,2,4:6)) %>% dplyr::mutate(Total=rowSums(.[2:5])) %>% 
      dplyr::bind_rows(apply(X = dplyr::select(.,-1),MARGIN = 2,FUN=sum)) %>% tidyr::replace_na(replace = list(name="Total")) %>% 
      dplyr::rename_at(.vars = 1,~'Miscari_in_timpul_Perioadei')
    
    output$portfolio_migration <- DT::renderDataTable({dt_generate_function(df=vals$tabel_migration_final,round_col = 2:6,show_buttons = TRUE,
                                                                            caption = "Migratia portofoliului de garantii in functie de categoria contaminata a beneficiarului:")  })
    
    # This table produces provision migration for all CUI that are in perioada curenta (it does not matter if they are in perioada anterioara or not)
    vals$tabel_variatie_provizioane <- dplyr::left_join(x = dplyr::select(vals$portofoliu_perioada_curenta,`Cod Partener`,
                                                                          !!rlang::sym(vals$categorie_curenta), !!rlang::sym(vals$provizion_curent)), 
                                                        y = dplyr::select(vals$portofoliu_perioada_anterioara,`Cod Partener`,!!rlang::sym(vals$categorie_anterioara),
                                                                          !!rlang::sym(vals$provizion_anterior)),by="Cod Partener") %>%
      dplyr::mutate_at(.vars = 5,~replace(x = .,list = which(is.na(.)),values = 0)) %>% 
      dplyr::mutate(variatie_provizion = .[[3]] - .[[5]])
    
    # This table produces provisions for all CUI that are in perioada_anterioara and are not in perioada curenta
    vals$tabel_beneficiari_iesiti <- dplyr::left_join(y = dplyr::select(vals$portofoliu_perioada_curenta,`Cod Partener`,
                                                                        !!rlang::sym(vals$categorie_curenta),!!rlang::sym(vals$provizion_curent)),
                                                      x = dplyr::select(vals$portofoliu_perioada_anterioara,`Cod Partener`,
                                                                        !!rlang::sym(vals$categorie_anterioara), !!rlang::sym(vals$provizion_anterior)),
                                                      by = "Cod Partener") %>%  dplyr::filter(is.na(!!rlang::sym(vals$categorie_curenta))) %>% 
      dplyr::mutate(variatie_provizion = .[[3]] * (-1))
    
    # I produce my final regualrisation provisions
    vals$regularizare_provizioane_non_ifrs <- dplyr::bind_rows(vals$tabel_beneficiari_iesiti,vals$tabel_variatie_provizioane)
    output$down_regularizare_provizioane <- downloadHandler(filename = function(){"regularizare-provizioane.csv"},
                                                            content = function(file){
                                                              readr::write_csv(x = vals$regularizare_provizioane_non_ifrs,path = file) })
    
  })
  
 
  
 
}
    
## To be copied in the UI
# mod_portofoliu_ui("portofoliu_ui_1")
    
## To be copied in the server
# callModule(mod_portofoliu_server, "portofoliu_ui_1")
 

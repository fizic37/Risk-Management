# These commands are used for mod_portofoliu only

# This function aggregates cerere_plata_cumulata in the order of the columns in dataframe
max_col <- function(df){
  if (ncol(df)<2) {return(df)}
  else {for (i in 2:ncol(df)){
    df[,i] = apply(df[,(i-1):i],1,max) } # This works on a row level, and adds 
  }
  return(df)
}

# Function to rename cerere_plata_cumulata according to its year
rename_col <- function(df){
  return(dplyr::rename_at(.tbl = df,
      .vars = "Cerere_Plata_cumulata",~paste0("Cerere_Plata_cumulata_",unique(df$an_cerere_plata)))) }

# I need this function in order to rename columns according to their date of refference (anul_de_raportare): ex: expunere_2018_12_31
# rename_col_ifrs <-  function(df){
#   return(dplyr::rename_at(.tbl = df,.vars = c("Expunere","stage"),
#                           ~paste0(c("Expunere_","stage_"),
#                                   unique(df$data_raport)
#                           )))  
# }
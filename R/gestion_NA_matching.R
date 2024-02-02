library(tidyverse)


clear_NA_to_m = function(d_NA, list_var){
  
  d_NA$h_travail_semaine[is.na(d_NA$h_travail_semaine)] <- 0
  
  list <- c("DIPLOME_pere", "DIPLOME_mere", "CRITREVENU_r", "naiss_parents")
  
  d_NA <- d_NA %>% mutate(    
    across(all_of(list), 
           function(x) fct_explicit_na(x, na_level = "Manquant"))
  )
  
  d_NA <- d_NA[complete.cases(d_NA[list_var]), ]
  
  tmp <- sum(d_NA$POND)/nrow(d_NA) 
  d_NA$POND <- d_NA$POND/tmp
  
  return(d_NA)
}

library(tidyverse)


# nettoyage des NA en vue du matching
# list_var = Variables ne devant plus contenir aucune NA
clear_NA_to_m = function(d_NA, list_var){
  
  # Les na dans heures travail semaine correspondent aux gens qui ne travail pas, solution simple est de remplacer par 0. Sinon on supprime la variable.
  d_NA$h_travail_semaine[is.na(d_NA$h_travail_semaine)] <- 0
  
  # Pour Le revenu, le diplome des parents et le pays de naissance des parents (france ou étranger), il reste pas mal de NA
  # Il me semble que ces variables sont importantes, mais on ne peut se permettre de supprimer autant d'individus.
  # Dans l'attente d'une meilleur solution, je me contente d'explicite NA
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

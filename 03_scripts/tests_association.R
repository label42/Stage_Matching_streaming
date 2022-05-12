
pacman::p_load(tidyverse, dplyr, effsize)


to_dummy = function(data, col) {
  d_dico <- col %>%
    reformulate(intercept = FALSE) %>%
    model.matrix(data = data) %>%
    as.data.frame()
  return(d_dico)
}


# Fonction pour calculer le D de cohen entre une variable numérique et une variable catégorielle non dicotomique

force_lien_d_cohen = function(data, v_num, v_cat){
  data[,v_cat] <- fct_explicit_na(data[,v_cat], na_level = "Non dispo")
  d_dico <- to_dummy(data = data, col = v_cat)
  d_dico <- cbind(data[,v_num], d_dico)
  
  d_dico_names <- colnames(d_dico)
  
  for(i in 2:length(d_dico)){
    formula <- paste(d_dico_names[1], paste("`", d_dico_names[i],"`", sep=""), sep = " ~ ")
    formula <- as.formula(formula)
    res <- cohen.d(formula, data = d_dico)
    print(sprintf("D de cohen entre %s et %s : %f", v_num,  d_dico_names[i], res$estimate))
  }
  
}
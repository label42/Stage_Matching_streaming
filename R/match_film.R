library(MatchIt)
library(here)
library(tidyverse)

load(here("data", "PC18.RData"))
source(here("R", "gestion_NA_matching.R"))
list_var_match_film <- read_csv(here("data", "control_variables.csv")) %>% 
  filter(film) %>% 
  pull(var)

# Clearning NA before matching
PC18_to_m_film <- clear_NA_to_m(PC18, list_var_match_film)

PC18_to_m_film <- subset(PC18_to_m_film, PC18_to_m_film$freq_film != "Jamais")

# Creating formula for matching

model_matching_film <- as.formula(paste0("film_stream_VOD",
                                         " ~ ", 
                                         paste(list_var_match_film, collapse = " + ")))

###########################
#### Template Matching ####
###########################

# Accepted SMD SPD for each covariate
tols_all_var = c(0.005, rep(0.05, times = length(list_var_match_film)-1))

PC18_to_m_film <- droplevels(PC18_to_m_film)

# Performing matching
res_match_template_stream_film_VOD <- matchit(model_matching_film,
                                              data = PC18_to_m_film, s.weights = PC18_to_m_film$POND, 
                                              method = "cardinality",
                                              estimand = "ATT", ratio = NA, discard = "none",  
                                              tols = tols_all_var, std.tols = T, solver = "gurobi", time = 60)


# Normalizing weights
PC18_m_film <- match.data(res_match_template_stream_film_VOD, weights = "POND_m")

tmp <- sum(PC18_m_film$POND_m)/nrow(PC18_m_film) 
PC18_m_film$POND_m <- PC18_m_film$POND_m/tmp

save(PC18_m_film, PC18_to_m_film, res_match_template_stream_film_VOD, file = here("data", "film_matched.RData"))

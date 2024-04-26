library(MatchIt)
library(here)

load(here("data", "PC18.RData"))
source(here("R", "gestion_NA_matching.R"))
list_var_match_serie <- read_csv(here("data", "control_variables.csv")) %>% 
  filter(serie) %>% 
  pull(var)

# Clearning NA before matching
PC18_to_m_serie <- clear_NA_to_m(PC18, list_var_match_serie)

PC18_to_m_serie <- subset(PC18_to_m_serie, PC18_to_m_serie$freq_serie != "Jamais")

PC18_to_m_serie <- droplevels(PC18_to_m_serie)


# Creating formula for matching
model_matching_serie <- as.formula(paste0("serie_stream_VOD",
                                          " ~ ", 
                                          paste(list_var_match_serie, collapse = " + ")))

###########################
#### Template Matching ####
###########################

# Accepted SMD SPD for each covariate
tols_all_var = c(0.005, rep(0.05, times = length(list_var_match_serie)-1))

# Performing matching
res_match_template_stream_serie_VOD <- matchit(model_matching_serie,
                                               data = PC18_to_m_serie, s.weights = PC18_to_m_serie$POND, 
                                               method = "cardinality",
                                               estimand = "ATT", ratio = NA, discard = "none",  
                                               tols = tols_all_var, std.tols = T, solver = "gurobi", time = 60)



# Normalizing weights
PC18_m_serie <- match.data(res_match_template_stream_serie_VOD, weights = "POND_m")

tmp <- sum(PC18_m_serie$POND_m)/nrow(PC18_m_serie) 
PC18_m_serie$POND_m <- PC18_m_serie$POND_m/tmp

save(PC18_m_serie, PC18_to_m_serie, res_match_template_stream_serie_VOD, file = here("data", "serie_matched.RData"))

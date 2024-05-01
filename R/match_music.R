library(MatchIt)
library(here)

load(here("data", "PC18.RData"))
source(here("R", "gestion_NA_matching.R"))
list_var_match_music <- read_csv(here("data", "control_variables.csv")) %>% 
  filter(music) %>% 
  pull(var)

# Clearning NA before matching
PC18_to_m_music <- clear_NA_to_m(PC18, list_var_match_music)

PC18_to_m_music <- subset(PC18_to_m_music, !is.na(stream_spe))

# Creating formula for matching
model_matching_music <- as.formula(paste0("stream_spe",
                                          " ~ ", 
                                          paste(list_var_match_music, collapse = " + ")))

###########################
#### Template Matching ####
###########################

# Accepted SMD SPD for each covariate
tols_all_var = c(0.005, rep(0.05, times = length(list_var_match_music)-1))

PC18_to_m_music <- droplevels(PC18_to_m_music)

# Performing matching
res_match_template_stream_music <- matchit(model_matching_music,
                                       data = PC18_to_m_music, s.weights = PC18_to_m_music$POND, 
                                       method = "cardinality",
                                       estimand = "ATT", ratio = NA, discard = "none",  
                                       tols = tols_all_var, std.tols = T, solver = "gurobi", time = 5*60)




PC18_m_music <- match.data(res_match_template_stream_music, weights = "POND_m")

# Normalizing weights
tmp <- sum(PC18_m_music$POND_m)/nrow(PC18_m_music) 
PC18_m_music$POND_m <- PC18_m_music$POND_m/tmp

save(PC18_m_music, PC18_to_m_music, res_match_template_stream_music, file = here("data", "music_matched.RData"))

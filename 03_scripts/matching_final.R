pacman::p_load(tidyverse, questionr, MatchIt, here)


source(here("03_scripts", "gestion_NA_matching.R"))

# liste de toutes les variables identifiées comme candidat potentielles
list_var_all <- c("SEXE_r", "AGE", "CRITREVENU_r", "PCS_MENAGE", "h_travail_semaine", "DIPLOME_r", "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "CS_mere",
  "sorties_ami", "VITENCOUPLE_r", "logement",
  "music_amateur", 
  "freq_jv", 
  "freq_tv", "equip_tv", "clip_tv",
  "freq_film", "equip_film", "film_stream_VOD", "film_replay", "film_stream_autre", "film_DVD", "film_num", "nbr_genre_film",
  "freq_serie", "equip_serie", "serie_stream_VOD", "serie_replay", "serie_stream_autre", "serie_DVD", "serie_num", "nbr_genre_serie",
  "info_internet",
  "freq_lecture", "equip_lecture",
  "nbr_genre_film_cine", "musee_art_12m", "galerie_12m",
  "ordi", "acces_internet", "freq_internet", "reseaux_sociaux", "culture_en_ligne",
  "tv_enfance", "musique_enfance", "cinema_enfance", "nbr_genre_parent_ecoute", "nbr_genre_ecoute_enfance",
  "audivisuel_nonFR", "autre_langue")

# liste de variable choisi grce à une regression stepwise
list_var_match <- c("stream_spe", "SEXE_r", "AGE_5_r", "CRITREVENU_r", "PCS_MENAGE", "DIPLOME_r",
                    "naiss_parents", "DIPLOME_pere", "DIPLOME_mere", "VITENCOUPLE_r",
                    "music_amateur", "freq_tv", "equip_tv", "clip_tv", "freq_film",
                    "film_stream_VOD", "film_stream_autre", "nbr_genre_film", "equip_serie",
                    "serie_stream_VOD", "serie_replay", "info_internet", "musee_art_12m",
                    "galerie_12m", "freq_internet", "reseaux_sociaux", "culture_en_ligne",
                    "tv_enfance", "audivisuel_nonFR")


PC18_to_m <- clear_NA_to_m(PC18, list_var_match)



#########################################
##### Matching par propensity score #####
#########################################
# 
# list_var_match <- c("stream_spe", "SEXE_r", "AGE", "CRITREVENU_r", "PCS_MENAGE", "DIPLOME_r", 
#                     "naiss_parents", "DIPLOME_pere", "DIPLOME_mere", "VITENCOUPLE_r", 
#                     "music_amateur", "freq_tv", "equip_tv", "clip_tv", "freq_film", 
#                     "film_stream_VOD", "film_stream_autre", "nbr_genre_film", "equip_serie", 
#                     "serie_stream_VOD", "serie_replay", "info_internet", "musee_art_12m", 
#                     "galerie_12m", "freq_internet", "reseaux_sociaux", "culture_en_ligne", 
#                     "tv_enfance", "audivisuel_nonFR")

# covar_formula <- "SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + 
#                          naiss_parents + DIPLOME_pere + DIPLOME_mere + VITENCOUPLE_r + 
#                          music_amateur + freq_tv + equip_tv + clip_tv + freq_film + 
#                          film_stream_VOD + film_stream_autre + nbr_genre_film + equip_serie + 
#                          serie_stream_VOD + serie_replay + info_internet + musee_art_12m + 
#                          galerie_12m + freq_internet + reseaux_sociaux + culture_en_ligne + 
#                          tv_enfance + audivisuel_nonFR"
# 
# model_matching <- as.formula(paste("stream_spe", covar_formula, sep = " ~ "))

# #Le matching retenu ici est avec replacement . Ratio 5:1. Caliper 2 sur l'age. Supression 
# # des individus traités hors du support commun (n = 25).
# res_match_1to1_cali <- matchit(model_matching
#                                   , data = PC18_to_m, 
#                                   method = "nearest", distance = "glm", replace = T,
#                                   ratio = 5, caliper = c("AGE" = 2), std.caliper = F, discard = "treated")
# 
# res_match_1to1_cali <- add_s.weights(res_match_1to1_cali, s.weights = "POND")
# 
# PC18_m <- match.data(res_match_1to1_re5max_cali, weights = "POND_m")
# 
# # Pour obtenir la pondération post-matching, on multipli la pondération propre au matching, avec la pondération
# # officiel PC18
# #PC18_m$POND_m <- PC18_m$weights * PC18_m$POND

##################################
#### Matching par cardinality ####
# ##################################



covar_formula <- "SEXE_r + AGE_5_r + CRITREVENU_r + PCS_MENAGE + DIPLOME_r +
                         naiss_parents + DIPLOME_pere + DIPLOME_mere + VITENCOUPLE_r +
                         music_amateur + freq_tv + equip_tv + clip_tv + freq_film +
                         film_stream_VOD + film_stream_autre + nbr_genre_film + equip_serie +
                         serie_stream_VOD + serie_replay + info_internet + musee_art_12m +
                         galerie_12m + freq_internet + reseaux_sociaux + culture_en_ligne +
                         tv_enfance + audivisuel_nonFR"

model_matching <- as.formula(paste("stream_spe", covar_formula, sep = " ~ "))

# avec suppression d'une partie des traité
tols_all_var = c(0.05, 0.02, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)

std.tols_all_var = c(T, T, T, T, T, T, T, T, T, T,
                     T, T, T, T, T, T, T, T, T, T,
                     T, T, T, T, T, T, T, T)

res_match <- matchit(model_matching,
                                       data = PC18_to_m, s.weights = PC18_to_m$POND, 
                                       method = "cardinality",
                                       estimand = "ATT", ratio = NA, discard = "none",  
                                       tols = tols_all_var, std.tols = std.tols_all_var, solver = "gurobi", time = 60)



res_match

PC18_m <- match.data(res_match, weights = "POND_m")

# normalisation des poids pour que mean(POND) = 1
tmp <- sum(PC18_m$POND_m)/nrow(PC18_m) 
PC18_m$POND_m <- PC18_m$POND_m/tmp


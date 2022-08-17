library(tidyverse)
library(questionr)
library(MatchIt)
library(here)

source(here("03_scripts", "gestion_NA_matching.R"))

list_var_match_film <- c("film_stream_VOD", "SEXE_r", "AGE_5_r", "CRITREVENU_r", "PCS_MENAGE", "h_travail_semaine", "DIPLOME_r", 
                    "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "CS_mere","sorties_ami", "VITENCOUPLE_r", 
                    "logement", "freq_jv", "clip_tv",
                    "music_amateur", "music_12m", "music_ellememe", "music_manque", "stream_spe", "cd_ou_cass",
                    "radio", "nbr_genre_music", "nbr_artiste_ecoute", "aime_clas_f", "detest_clas_f",
                    "musee_art_12m", "galerie_12m", "acces_internet", "info_internet", "freq_info", "freq_lecture", 
                    "equip_lecture", "lecture_nonFR","freq_internet", "reseaux_sociaux",
                    "culture_en_ligne", "musique_enfance", "cinema_enfance", "tv_enfance",
                    "nbr_genre_parent_ecoute", "nbr_genre_ecoute_enfance", "autre_langue")

PC18_to_m_film <- clear_NA_to_m(PC18, list_var_match_film)

PC18_to_m_film <- subset(PC18_to_m_film, PC18_to_m_film$freq_film != "Jamais")


model_matching_film <- as.formula("film_stream_VOD ~ SEXE_r + AGE_5_r + CRITREVENU_r + PCS_MENAGE + 
                                  h_travail_semaine + DIPLOME_r + naiss_parents + DIPLOME_pere + 
                                  CS_pere + DIPLOME_mere + CS_mere + sorties_ami + VITENCOUPLE_r + 
                                  logement + freq_jv + clip_tv + music_amateur + music_12m + 
                                  music_ellememe + music_manque + stream_spe + cd_ou_cass + radio + 
                                  nbr_genre_music + nbr_artiste_ecoute + aime_clas_f + detest_clas_f + 
                                  musee_art_12m + galerie_12m + acces_internet + info_internet + 
                                  freq_info + freq_lecture + equip_lecture + lecture_nonFR + 
                                  freq_internet + reseaux_sociaux + culture_en_ligne + 
                                  musique_enfance + cinema_enfance + tv_enfance + 
                                  nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance + autre_langue")


tols_all_var = c(0.05, 0.005, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05)

PC18_to_m_film <- droplevels(PC18_to_m_film)

res_match_template_stream_film_VOD <- matchit(model_matching_film,
                                              data = PC18_to_m_film, s.weights = PC18_to_m_film$POND, 
                                              method = "cardinality",
                                              estimand = "ATT", ratio = NA, discard = "none",  
                                              tols = tols_all_var, std.tols = T, solver = "gurobi", time = 60)



PC18_m_film <- match.data(res_match_template_stream_film_VOD, weights = "POND_m")

tmp <- sum(PC18_m_film$POND_m)/nrow(PC18_m_film) 
PC18_m_film$POND_m <- PC18_m_film$POND_m/tmp

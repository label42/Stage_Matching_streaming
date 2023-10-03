library(tidyverse)
library(questionr)
library(MatchIt)
library(here)

source(here("03_scripts", "gestion_NA_matching.R"))

list_var_match_music <- c("stream_spe", "SEXE_r", "AGE", "CRITREVENU_r", "h_travail_semaine", "DIPLOME_r", 
                          "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "CS_mere","sorties_ami", "VITENCOUPLE_r", 
                          "logement", "freq_jv", "freq_tv", "equip_tv", "clip_tv", "equip_film", "film_stream_VOD", 
                          "film_stream_autre", "film_DVD", "film_num", "nbr_genre_film", "freq_serie", "equip_serie", 
                          "serie_stream_VOD", "serie_stream_autre", "nbr_genre_serie", "info_internet", "freq_lecture", 
                          "equip_lecture", "nbr_genre_film_cine", "musee_art_12m", "galerie_12m", "ordi", "acces_internet", 
                          "freq_internet", "reseaux_sociaux", "culture_en_ligne", "musique_enfance", "cinema_enfance", 
                          "nbr_genre_parent_ecoute", "nbr_genre_ecoute_enfance", "audivisuel_nonFR", "autre_langue")

model_matching_music <- as.formula("stream_spe ~ SEXE_r + AGE_5_r + CRITREVENU_r + h_travail_semaine + 
    DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere + 
    sorties_ami + VITENCOUPLE_r + logement + 
    freq_jv + freq_tv + equip_tv + clip_tv + equip_film + film_stream_VOD + 
    film_stream_autre + film_DVD + film_num + nbr_genre_film + 
    freq_serie + equip_serie + serie_stream_VOD + serie_stream_autre + 
    nbr_genre_serie + info_internet + freq_lecture + equip_lecture + 
    nbr_genre_film_cine + musee_art_12m + galerie_12m + acces_internet + ordi + 
    freq_internet + reseaux_sociaux + culture_en_ligne + musique_enfance + 
    cinema_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance + 
    audivisuel_nonFR + autre_langue")

##############################
#### Matching upper class ####
##############################

PC18_to_m_music_upper <- filter(PC18, !is.na(stream_spe), class == "Upper class")

PC18_to_m_music_upper <- clear_NA_to_m(PC18_to_m_music_upper, list_var_match_music)

tols_all_var = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)

res_match_template_stream_music_upper <- matchit(model_matching_music,
                                                   data = PC18_to_m_music_upper, s.weights = PC18_to_m_music_upper$POND, 
                                                   method = "cardinality",
                                                   estimand = "ATT", ratio = NA, discard = "none",  
                                                   tols = tols_all_var, std.tols = T, solver = "gurobi", time = 60)

PC18_m_music_upper <- match.data(res_match_template_stream_music_upper, weights = "POND_m")

# normalisation des poids pour que mean(POND) = 1
tmp <- sum(PC18_m_music_upper$POND_m)/nrow(PC18_m_music_upper) 
PC18_m_music_upper$POND_m <- PC18_m_music_upper$POND_m/tmp

##############################
#### Matching middle class ####
##############################

PC18_to_m_music_middle <- filter(PC18, !is.na(stream_spe), class == "Middle class")

PC18_to_m_music_middle <- clear_NA_to_m(PC18_to_m_music_middle, list_var_match_music)

tols_all_var = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

res_match_template_stream_music_middle <- matchit(model_matching_music,
                                                   data = PC18_to_m_music_middle, s.weights = PC18_to_m_music_middle$POND, 
                                                   method = "cardinality",
                                                   estimand = "ATT", ratio = NA, discard = "none",  
                                                   tols = tols_all_var, std.tols = T, solver = "gurobi", time = 60)

PC18_m_music_middle <- match.data(res_match_template_stream_music_middle, weights = "POND_m")

# normalisation des poids pour que mean(POND) = 1
tmp <- sum(PC18_m_music_middle$POND_m)/nrow(PC18_m_music_middle) 
PC18_m_music_middle$POND_m <- PC18_m_music_middle$POND_m/tmp


##############################
#### Matching working class ####
##############################

PC18_to_m_music_working <- filter(PC18, !is.na(stream_spe), class == "Working class")

PC18_to_m_music_working <- clear_NA_to_m(PC18_to_m_music_working, list_var_match_music)

tols_all_var = c(0.05, 0.005, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)

res_match_template_stream_music_working <- matchit(model_matching_music,
                                           data = PC18_to_m_music_working, s.weights = PC18_to_m_music_working$POND, 
                                           method = "cardinality",
                                           estimand = "ATT", ratio = NA, discard = "none",  
                                           tols = tols_all_var, std.tols = T, solver = "gurobi", time = 30)

PC18_m_music_working <- match.data(res_match_template_stream_music_working, weights = "POND_m")

# normalisation des poids pour que mean(POND) = 1
tmp <- sum(PC18_m_music_working$POND_m)/nrow(PC18_m_music_working) 
PC18_m_music_working$POND_m <- PC18_m_music_working$POND_m/tmp

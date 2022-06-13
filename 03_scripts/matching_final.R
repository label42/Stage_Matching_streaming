pacman::p_load(tidyverse, questionr, MatchIt, here)


source(here("03_scripts", "gestion_NA_matching.R"))

# liste de toutes les variables identifiées comme candidat potentielles
list_var_all <- c("SEXE_r", "AGE", "CRITREVENU_r", "PCS_MENAGE", "h_travail_semaine", "DIPLOME_r", "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "CS_mere",
  "sorties_ami", "VITENCOUPLE_r", "logement",
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

list_var_match <- c("stream_spe", "SEXE_r", "AGE", "CRITREVENU_r", "PCS_MENAGE", "h_travail_semaine", "DIPLOME_r", 
                    "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "sorties_ami", "VITENCOUPLE_r", 
                    "logement", "freq_jv", "freq_tv", "equip_tv", "clip_tv", "equip_film", "film_stream_VOD", 
                    "film_stream_autre", "film_DVD", "film_num", "nbr_genre_film", "freq_serie", "equip_serie", 
                    "serie_stream_VOD", "serie_stream_autre", "nbr_genre_serie", "info_internet", "freq_lecture", 
                    "equip_lecture", "nbr_genre_film_cine", "musee_art_12m", "galerie_12m", "acces_internet", 
                    "freq_internet", "reseaux_sociaux", "culture_en_ligne", "musique_enfance", "cinema_enfance", 
                    "nbr_genre_parent_ecoute", "nbr_genre_ecoute_enfance", "audivisuel_nonFR", "autre_langue")

PC18_to_m <- clear_NA_to_m(PC18, list_var_match)

model_matching <- as.formula("stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + 
    DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + 
    sorties_ami + VITENCOUPLE_r + logement + 
    freq_jv + freq_tv + equip_tv + clip_tv + equip_film + film_stream_VOD + 
    film_stream_autre + film_DVD + film_num + nbr_genre_film + 
    freq_serie + equip_serie + serie_stream_VOD + serie_stream_autre + 
    nbr_genre_serie + info_internet + freq_lecture + equip_lecture + 
    nbr_genre_film_cine + musee_art_12m + galerie_12m + acces_internet + 
    freq_internet + reseaux_sociaux + culture_en_ligne + musique_enfance + 
    cinema_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance + 
    audivisuel_nonFR + autre_langue")

model_matching_age_5 <- as.formula("stream_spe ~ SEXE_r + AGE_5_r + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + 
    DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + 
    sorties_ami + VITENCOUPLE_r + logement + 
    freq_jv + freq_tv + equip_tv + clip_tv + equip_film + film_stream_VOD + 
    film_stream_autre + film_DVD + film_num + nbr_genre_film + 
    freq_serie + equip_serie + serie_stream_VOD + serie_stream_autre + 
    nbr_genre_serie + info_internet + freq_lecture + equip_lecture + 
    nbr_genre_film_cine + musee_art_12m + galerie_12m + acces_internet + 
    freq_internet + reseaux_sociaux + culture_en_ligne + musique_enfance + 
    cinema_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance + 
    audivisuel_nonFR + autre_langue")



#########################################
##### Matching par propensity score #####
#########################################
#
res_match_1to1_re_cali <- matchit(model_matching
                                  , data = PC18_to_m, s.weights = PC18_to_m$POND,
                                  method = "nearest", distance = "glm", replace = T, 
                                  ratio = 1, caliper = c("AGE" = 2), std.caliper = F, discard = "none"
)


PC18_m_1to1 <- match.data(res_match_1to1_re_cali, weights = "POND_m")

# normalisation des poids pour que mean(POND) = 1
tmp <- sum(PC18_m_1to1$POND_m)/nrow(PC18_m_1to1) 
PC18_m_1to1$POND_m <- PC18_m_1to1$POND_m/tmp


##################################
#### Matching par cardinality ####
# ##################################




# avec suppression d'une partie des traité
tols_all_var = c(0.05, 0.005, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05)

res_match_template_w_before <- matchit(model_matching_age_5,
                                       data = PC18_to_m, s.weights = PC18_to_m$POND, 
                                       method = "cardinality",
                                       estimand = "ATT", ratio = NA, discard = "none",  
                                       tols = tols_all_var, std.tols = T, solver = "gurobi", time = 30)




PC18_m <- match.data(res_match_template_w_before, weights = "POND_m")

# normalisation des poids pour que mean(POND) = 1
tmp <- sum(PC18_m$POND_m)/nrow(PC18_m) 
PC18_m$POND_m <- PC18_m$POND_m/tmp


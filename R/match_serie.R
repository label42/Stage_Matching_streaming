library(MatchIt)
library(here)

load(here("data", "PC18.RData"))
source(here("R", "gestion_NA_matching.R"))

list_var_match_serie <- c("serie_stream_VOD", "SEXE_r", "AGE_5_r", "CRITREVENU_r", "PCS_MENAGE", "h_travail_semaine", "DIPLOME_r", 
                    "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "CS_mere","sorties_ami", "VITENCOUPLE_r", 
                    "logement", "freq_jv", "clip_tv", "music_amateur", "music_12m", 
                    "nbr_genre_jeuxvideo", "nbr_genre_livre",
                    "music_ellememe", "music_manque", "stream_spe", "cd_ou_cass",
                    "radio", "nbr_genre_music", "nbr_artiste_ecoute", "aime_clas", "detest_clas",
                    "musee_art_12m", "galerie_12m", "acces_internet", "info_internet", "freq_info", "freq_lecture", 
                    "equip_lecture", "lecture_nonFR", "ordi", "freq_internet", "reseaux_sociaux",
                    "culture_en_ligne", "musique_enfance", "cinema_enfance", "tv_enfance",
                    "nbr_genre_parent_ecoute", "nbr_genre_ecoute_enfance", "autre_langue")

# Clearning NA before matching
PC18_to_m_serie <- clear_NA_to_m(PC18, list_var_match_serie)

PC18_to_m_serie <- subset(PC18_to_m_serie, PC18_to_m_serie$freq_serie != "Jamais")

PC18_to_m_serie <- droplevels(PC18_to_m_serie)


# Creating formula for matching
model_matching_serie <- as.formula("serie_stream_VOD ~ SEXE_r + AGE_5_r + 
                                   CRITREVENU_r + PCS_MENAGE + h_travail_semaine + 
                                   DIPLOME_r + naiss_parents + DIPLOME_pere + 
                                   CS_pere + DIPLOME_mere + CS_mere + sorties_ami + 
                                   VITENCOUPLE_r + logement + freq_jv + clip_tv + 
                                   music_amateur + music_12m + nbr_genre_jeuxvideo + 
                                   nbr_genre_livre + music_ellememe + music_manque + 
                                   stream_spe + cd_ou_cass + radio + nbr_genre_music + 
                                   nbr_artiste_ecoute + aime_clas + detest_clas + 
                                   musee_art_12m + galerie_12m + ordi + acces_internet + 
                                   info_internet + freq_info + freq_lecture + equip_lecture + 
                                   lecture_nonFR + freq_internet + reseaux_sociaux + 
                                   culture_en_ligne + musique_enfance + cinema_enfance + 
                                   tv_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance + 
                                   autre_langue")



###########################
#### Template Matching ####
###########################

# Accepted SMD SPD for each covariate
tols_all_var = c(0.05, 0.005, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)

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

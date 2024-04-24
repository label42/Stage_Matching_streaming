library(MatchIt)
library(here)

load(here("data", "PC18.RData"))
source(here("R", "gestion_NA_matching.R"))

list_var_match_music <- c("stream_spe", "SEXE_r", "AGE", "CRITREVENU_r", "PCS_MENAGE", "h_travail_semaine", "DIPLOME_r", 
                    "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "CS_mere","sorties_ami", "VITENCOUPLE_r", 
                    "nbr_genre_jeuxvideo", "nbr_genre_livre",
                    "logement", "freq_jv", "freq_tv", "equip_tv", "clip_tv", "equip_film", "film_stream_VOD", 
                    "film_stream_autre", "film_DVD", "film_num", "nbr_genre_film", "freq_serie", "equip_serie", 
                    "serie_stream_VOD", "serie_stream_autre", "nbr_genre_serie", "info_internet", "freq_lecture", 
                    "equip_lecture", "nbr_genre_film_cine", "musee_art_12m", "galerie_12m", "ordi", "acces_internet", 
                    "freq_internet", "reseaux_sociaux", "culture_en_ligne", "musique_enfance", "cinema_enfance", 
                    "nbr_genre_parent_ecoute", "nbr_genre_ecoute_enfance", "audivisuel_nonFR", "autre_langue")

PC18_to_m_music <- subset(PC18, !is.na(stream_spe))

# Clearning NA before matching
PC18_to_m_music <- clear_NA_to_m(PC18, list_var_match_music)

# Creating formula for matching
model_matching_music <- as.formula("stream_spe ~ SEXE_r + AGE_5_r + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + 
    DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere + 
    sorties_ami + VITENCOUPLE_r + logement + 
    nbr_genre_jeuxvideo + nbr_genre_livre +
    freq_jv + freq_tv + equip_tv + clip_tv + equip_film + film_stream_VOD + 
    film_stream_autre + film_DVD + film_num + nbr_genre_film + 
    freq_serie + equip_serie + serie_stream_VOD + serie_stream_autre + 
    nbr_genre_serie + info_internet + freq_lecture + equip_lecture + 
    nbr_genre_film_cine + musee_art_12m + galerie_12m + acces_internet + ordi + 
    freq_internet + reseaux_sociaux + culture_en_ligne + musique_enfance + 
    cinema_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance + 
    audivisuel_nonFR + autre_langue")


###########################
#### Template Matching ####
###########################

# Accepted SMD SPD for each covariate
tols_all_var = c(0.05, 0.005, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05,
                 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)

# Performing matching
res_match_template_stream_music <- matchit(model_matching_music,
                                       data = PC18_to_m_music, s.weights = PC18_to_m_music$POND, 
                                       method = "cardinality",
                                       estimand = "ATT", ratio = NA, discard = "none",  
                                       tols = tols_all_var, std.tols = T, solver = "gurobi", time = 30)




PC18_m_music <- match.data(res_match_template_stream_music, weights = "POND_m")

# Normalizing weights
tmp <- sum(PC18_m_music$POND_m)/nrow(PC18_m_music) 
PC18_m_music$POND_m <- PC18_m_music$POND_m/tmp

save(PC18_m_music, PC18_to_m_music, res_match_template_stream_music, file = here("data", "music_matched.RData"))

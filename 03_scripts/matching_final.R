pacman::p_load(tidyverse, questionr, MatchIt, here)


source(here("03_scripts", "gestion_NA_matching.R"))



#variables à utliser dans le modèle en vue du matching
list_var <- c("nbr_genre_music","stream_spe", "SEXE_r", "AGE", "AGE_5_r", "CRITREVENU_r", "PCS_MENAGE", "h_travail_semaine", "DIPLOME_r", "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "CS_mere",
              "music_amateur",
              "freq_tv", "equip_tv", "clip_tv",
              "freq_film", "film_stream_VOD", "film_stream_autre", "nbr_genre_film",
              "freq_serie", "equip_serie", "serie_stream_VOD", "serie_replay", "serie_stream_autre",  "nbr_genre_serie",
              "info_internet",
              "freq_lecture",
              "musee_art_12m", "galerie_12m",
              "freq_internet", "reseaux_sociaux", "culture_en_ligne",
              "tv_enfance", "musique_enfance", "nbr_genre_parent_ecoute", "nbr_genre_ecoute_enfance",
              "audivisuel_nonFR")

#Gestio ligne contenant des NA pour les variable servant à l'estimation du propensity score
PC18_to_m <- clear_NA_to_m(PC18, list_var)

res_match_3to1_replace <- matchit(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
                                    music_amateur + 
                                    freq_tv + equip_tv + clip_tv +
                                    freq_film + film_stream_VOD + film_stream_autre + nbr_genre_film +
                                    freq_serie + equip_serie + serie_stream_VOD + serie_replay + serie_stream_autre +
                                    info_internet +
                                    freq_lecture +
                                    musee_art_12m + galerie_12m +
                                    freq_internet + reseaux_sociaux + culture_en_ligne +
                                    tv_enfance + musique_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance +
                                    audivisuel_nonFR
                                  , data = PC18_to_m, 
                                  method = "nearest", distance = "glm", replace = TRUE, ratio = 3, caliper = c(0.01, "AGE" = 2),std.caliper = F
)

PC18_m <- get_matches(res_match_3to1_replace)

pacman::p_load(tidyverse, questionr, MatchIt, greybox, 
               dplyr, lmtest, sandwich, boot, survival, R.utils, cobalt, sjPlot, DescTools)

d <- read.csv("../Data/pc18_quetelet_octobre2021.csv",sep=";")

source("../recodage_matching.R")

d_reg <- d %>% mutate_if(is.factor,
                         fct_explicit_na,
                         na_level = "Manquant")
d_reg <- d_reg %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
d_reg <- d_reg %>% mutate_if(is.character, ~replace(., is.na(.), "Manquant"))

res <- glm(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + DIPLOME_r + matiere_diplome + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
             sorties_ami + 
             music_amateur + 
             freq_jv + 
             freq_tv + equip_tv + clip_tv +
             freq_film + equip_film + film_stream_VOD + film_replay + film_stream_autre + film_DVD + film_num + nbr_genre_film +
             freq_serie + equip_serie + serie_stream_VOD + serie_replay + serie_stream_autre + serie_DVD + serie_num + nbr_genre_serie +
             info_internet +
             freq_lecture + equip_lecture +
             nbr_genre_film_cine + musee_art_12m + galerie_12m +
             ordi + acces_internet + freq_internet + reseaux_sociaux + culture_en_ligne +
             tv_enfance + musique_enfance + cinema_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance +
             audivisuel_nonFR + autre_langue, 
           data = d_reg, family = "binomial")

summary(res)

PseudoR2(res, which = "all")

res <- lm(nbr_genre_music ~ stream_spe + 
            SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + DIPLOME_r + matiere_diplome + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
             sorties_ami + 
             music_amateur + 
             freq_jv + 
             freq_tv + equip_tv + clip_tv +
             freq_film + equip_film + film_stream_VOD + film_replay + film_stream_autre + film_DVD + film_num + nbr_genre_film +
             freq_serie + equip_serie + serie_stream_VOD + serie_replay + serie_stream_autre + serie_DVD + serie_num + nbr_genre_serie +
             info_internet +
             freq_lecture + equip_lecture +
             nbr_genre_film_cine + musee_art_12m + galerie_12m +
             ordi + acces_internet + freq_internet + reseaux_sociaux + culture_en_ligne +
             tv_enfance + musique_enfance + cinema_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance +
             audivisuel_nonFR + autre_langue, 
           data = d_reg, )

summary(res)

PseudoR2(res, which = "all")

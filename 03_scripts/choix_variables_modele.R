library(glmulti, here)



sink(here("04_analyses_techniques","meilleur_modele_utilisation_plateformes.txt"), append=TRUE)

results <- glmulti(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + 
                     naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere,
          data = PC18_to_m,
          level = 1,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 10,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "glm", # glm function
          includeobjects = F,
          family = binomial)       # binomial family for logistic regression


## Show 5 best models (Use @ instead of $ for an S4 object)
results@formulas

summary(results)

sink()

plot(results, type="s")

glmulti(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
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
        data = d_reg, family = "binomial", level = 1, report = T, plotty = F, chunk = 8)
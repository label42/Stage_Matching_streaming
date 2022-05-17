library(leaps)
library(MASS)


stream_glm <- glm(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
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
        data = PC18_to_m, family = "binomial")

summary(stream_glm)

sink(here("04_analyses_techniques","meilleur_modele_utilisation_plateformes_restraint.txt"), append=TRUE)

#Commande prenant plusieurs dizines de minutes à s'executer
stream_glm_step <- stepAIC(stream_glm, steps = 1000, trace = 1, scope = c(lower = as.formula("stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere")))

sink()

# résultat

best_glm_stream <- glm(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + 
                     naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere + 
                     music_amateur + freq_tv + equip_tv + clip_tv + freq_film + 
                     film_stream_VOD + film_stream_autre + nbr_genre_film + equip_serie + 
                     serie_stream_VOD + serie_replay + info_internet + musee_art_12m + 
                     galerie_12m + freq_internet + reseaux_sociaux + culture_en_ligne + 
                     tv_enfance + audivisuel_nonFR, data = PC18_to_m, family = "binomial")

summary(best_glm_stream)

DescTools::PseudoR2(best_glm_stream, which = "all")

## Sans contrainte de variable

sink(here("04_analyses_techniques","meilleur_modele_utilisation_plateformes.txt"), append=TRUE)

stream_glm_step <- stepAIC(stream_glm, steps = 1000, trace = 1)

sink()

# resultat

best_glm_stream_all_var <- glm(stream_spe ~ SEXE_r + AGE + DIPLOME_r + naiss_parents + music_amateur + 
  freq_tv + equip_tv + clip_tv + freq_film + film_stream_VOD + 
  film_stream_autre + nbr_genre_film + equip_serie + serie_stream_VOD + 
  serie_replay + info_internet + musee_art_12m + galerie_12m + 
  freq_internet + reseaux_sociaux + culture_en_ligne + tv_enfance + 
  cinema_enfance + audivisuel_nonFR, data = PC18_to_m, family = "binomial")

summary(best_glm_stream_all_var)

DescTools::PseudoR2(best_glm_stream_all_var, which = "all")

##################################################
#### méthode exhaustive prenant trop de temps ####
##################################################

## Prend trop de temps. proche du milliard d'itération
sink(here("04_analyses_techniques","meilleur_modele_utilisation_plateformes.txt"), append=TRUE)

results <- glmulti(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
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
                   data = PC18_to_m,
                   level = 1,               # No interaction considered
                   method = "h",            # Exhaustive approach
                   crit = "aic",            # AIC as criteria
                   confsetsize = 10,         # Keep 5 best models
                   plotty = F, report = T,  # No plot or interim reports
                   fitfunction = "glm", # glm function
                   includeobjects = F,
                   family = binomial)       # binomial family for logistic regression


## Show 5 best models (Use @ instead of $ for an S4 object)
results@formulas

summary(results)

sink()


pacman::p_load(tidyverse, questionr, MatchIt, greybox, 
               dplyr, lmtest, sandwich, boot, survival, R.utils, cobalt, sjPlot, DescTools)

d <- read.csv("Data/pc18_quetelet_octobre2021.csv",sep=";")

source("recodage_matching.R")

##################################
#### nettoyage pour matching #####
##################################

#observer le nombre de NA pour chacune de nos variables

list_var <- c("nbr_genre_music","stream_spe", "SEXE_r", "AGE", "AGE_5_r", "CRITREVENU_r", "PCS_MENAGE", "h_travail_semaine", "DIPLOME_r", "matiere_diplome", "naiss_parents", "DIPLOME_pere", "CS_pere", "DIPLOME_mere", "CS_mere",
              "sorties_ami", "music_amateur", "freq_jv", 
              "freq_tv", "equip_tv", "clip_tv",
                "freq_film", "equip_film", "film_stream_VOD", "film_replay", "film_stream_autre", "film_DVD", "film_num", "nbr_genre_film",
                "freq_serie", "equip_serie", "serie_stream_VOD", "serie_replay", "serie_stream_autre", "serie_DVD", "serie_num", "nbr_genre_serie",
                "info_internet",
                "freq_lecture", "equip_lecture",
                "nbr_genre_film_cine", "musee_art_12m", "galerie_12m",
                "ordi", "acces_internet", "freq_internet", "reseaux_sociaux", "culture_en_ligne",
                "tv_enfance", "musique_enfance", "cinema_enfance", "nbr_genre_parent_ecoute", "nbr_genre_ecoute_enfance",
                "audivisuel_nonFR", "autre_langue", "lecture_nonFR", "info_nonFR")
d_reg <- d[list_var]

na_count <- sapply(d_reg, function(y) sum(is.na(y)))
na_count <- data.frame(na_count)
View(na_count)

# On voit que matière diplome contient 6000+ NA, surement pas utile de garder la variable.
d_reg <- subset(d_reg, select = -c(matiere_diplome))


# Tous les NA de stream_spe correspondent aux gens qui n'écoutent pas de musique, on peut supprimer ces individus.
d_reg <- subset(d_reg, !is.na(stream_spe))

# Les na dans heures travail semaine correspondent aux gens qui ne travail pas, solution simple est de remplacer par 0. Sinon on supprime la variable.
d_reg$h_travail_semaine[is.na(d_reg$h_travail_semaine)] <- 0

# Pour Le revenu, le diplome des parents et le pays de naissance des parents (france ou étranger), il reste pas mal de NA
# Il me semble que ces variables sont importantes, mais on ne peut se permettre de supprimer autant d'individus.
# Dans l'attente d'une meilleur solution, je me contente d'explicite NA
list <- c("DIPLOME_pere", "DIPLOME_mere", "CRITREVENU_r", "naiss_parents")

d_reg <- d_reg %>% mutate(    
  across(all_of(list), 
         function(x) fct_explicit_na(x, na_level = "Manquant"))
)

# A la suite de ces opération il nous reste 233 lignes contenant au moins une NA environ. On s'en sépare ?


###########################################
#### Modèle de reg servant au matching ####
###########################################

res <- glm(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
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
#pseudo R2 autour de 0,3/0,4 selon les méthodes de calcul
PseudoR2(res, which = "all")

#Prosition de variables à enlever car aucun coeffcient significatifs ou proche de l'être (je garde par contre toutes les socio démo) :
# h_travail_semaine
# sorties_ami
# freq_jv
# equip_film (prabablement redondant avec support pour regarder des films)
# film_dvd
# film_num
# film_replay
# serie_DVD
# serie_num
# nbr_genre_serie
# equip_lecture
# nbr_genre_film_cine
# ordi
# accès_internet (redondant fréquence internet)
# cinema_enfance (on laisse musique enfance car marche pour prédire la diversité)
# autre_langue (par contre, pourquoi par rajouter d'autres variables cosmopolitisme, car audiovisuel_nonFR fort lien ) (On a testé lecture_nonFR et info_nonFr ça ne donne rien)

res <- glm(stream_spe ~ SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
             music_amateur + 
             freq_tv + equip_tv + clip_tv +
             freq_film + film_stream_VOD + film_stream_autre + nbr_genre_film +
             freq_serie + equip_serie + serie_stream_VOD + serie_replay + serie_stream_autre +
             info_internet +
             freq_lecture +
             musee_art_12m + galerie_12m +
             freq_internet + reseaux_sociaux + culture_en_ligne +
             tv_enfance + musique_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance +
             audivisuel_nonFR, 
           data = d_reg, family = "binomial")

summary(res)
# les varaibles socio démo des parents pas du tout significatif. Garder dans le doute ?
# De manière générale on a tellement de variables directement lié a la culture que le lien propre avec les variables
# sociodémo est faible. Cela nous rapelle que ces variables de positions sociales ne sont qu'un proxi imprecis. 
# L"homologie entre les différentes pratiques culturelle est plus forte que la position sociale grossièrement décrite par 
# salaire, diplome, PCS.

#pseudo R2 toujours autour de 0,3/0,4 selon les méthodes de calcul, malgré les retraits.

PseudoR2(res, which = "all")



# estimation du nombre de styles musicaux écoutés
# avec toutes les variables
res <- lm(nbr_genre_music ~ stream_spe + 
            SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + h_travail_semaine + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
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

#R2 ajusté 0,34
summary(res)

# modèle plus parcimonieux
res <- lm(nbr_genre_music ~ stream_spe + SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
             music_amateur + 
             freq_tv + equip_tv + clip_tv +
             freq_film + film_stream_VOD + film_stream_autre + nbr_genre_film +
             freq_serie + equip_serie + serie_stream_VOD + serie_replay + serie_stream_autre +
             info_internet +
             freq_lecture +
             musee_art_12m + galerie_12m +
             freq_internet + reseaux_sociaux + culture_en_ligne +
             tv_enfance + musique_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance +
             audivisuel_nonFR, 
           data = d_reg)

# r² dimnue d'a peine 0.05
summary(res)


##################
#### Matching ####
##################

d_match <- d_reg[complete.cases(d_reg), ]


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
                           , data = d_match, 
                           method = "nearest", distance = "glm", replace = TRUE, ratio = 3, caliper = c(0.01, "AGE" = 2),std.caliper = F
)

res_match_3to1_replace

summary(res_match_3to1_replace)

love.plot(res_match_3to1_replace, 
          drop.distance = TRUE, 
          var.order = "adjusted",
          abs = TRUE,
          thresholds = c(m = .05), 
          binary = "std",
          continuous = "std")

# Aucune variable de dépasse une différence de 0.05 en moyenne standardisé. On atteint largement les standards de qualité.
bal.tab(res_match_3to1_replace, m.threshold = 0.05, v.threshold = 2)

#Très bon. Petit soucis de support commun à la fin de la distribution qui nous fait perdre quelques individus.
bal.plot(res_match_3to1_replace, var.name = "distance", which = "both",
         type = "histogram", mirror = TRUE)

bal.plot(res_match_3to1_replace, var.name = "distance", which = "both",
         type = "density", mirror = F)


bal.plot(res_match_3to1_replace, var.name = "AGE")

bal.plot(res_match_3to1_replace, var.name = "nbr_genre_parent_ecoute", 
         type = "histogram", mirror = F)

bal.plot(res_match_3to1_replace, var.name = "nbr_genre_parent_ecoute", 
         type = "density", mirror = F)


#########################################
#### Estimation des effets du stream ####
#########################################

res_match_3to1_replace.d_match <- get_matches(res_match_3to1_replace)

## Estimer l'effet du streaming sur le nombe de styles écoutés, sans interaction.
# Sans controler par les variables ayant servit au matching
nbr_styles <- lm(nbr_genre_music ~ stream_spe
                 , data = res_match_3to1_replace.d_match, weights = weights)

coeftest(nbr_styles, vcov. = vcovCL)

## Estimer l'effet du streaming sur le nombe de styles écoutés, sans interaction
nbr_styles <- lm(nbr_genre_music ~ stream_spe + SEXE_r + AGE + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
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
                 , data = res_match_3to1_replace.d_match, weights = weights)

coeftest(nbr_styles, vcov. = vcovCL)


## Estimer l'effet du streaming sur le nombe de styles écoutés, avec interaction AGE * stream_spe
nbr_styles <- lm(nbr_genre_music ~ stream_spe + SEXE_r + AGE_5_r + CRITREVENU_r + PCS_MENAGE + DIPLOME_r + naiss_parents + DIPLOME_pere + CS_pere + DIPLOME_mere + CS_mere +
                   music_amateur + 
                   freq_tv + equip_tv + clip_tv +
                   freq_film + film_stream_VOD + film_stream_autre + nbr_genre_film +
                   freq_serie + equip_serie + serie_stream_VOD + serie_replay + serie_stream_autre +
                   info_internet +
                   freq_lecture +
                   musee_art_12m + galerie_12m +
                   freq_internet + reseaux_sociaux + culture_en_ligne +
                   tv_enfance + musique_enfance + nbr_genre_parent_ecoute + nbr_genre_ecoute_enfance +
                   audivisuel_nonFR + 
                   stream_spe * AGE_5_r
                 , data = res_match_3to1_replace.d_match, weights = weights)

coeftest(nbr_styles, vcov. = vcovCL)


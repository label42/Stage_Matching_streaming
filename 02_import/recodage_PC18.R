

library(tidyverse)
library(questionr)

####################
#### Socio démo ####
####################

# on supprime les individus sans pondération
PC18 <- subset(PC18, !is.na(PC18$POND))


#pour que la pop total soit égale au nombre de ligne dans la base 
#(on divise la pondération par la somme de toutes les pondérations divisées par le nombre d'individus)
tmp <- sum(PC18$POND)/nrow(PC18) 
PC18$POND <- PC18$POND/tmp

PC18 <- PC18 %>% mutate(    
  SEXE_r = case_when(
    SEXE == 1 ~ "Homme",
    SEXE == 2 ~ "Femme"
  ))
PC18$SEXE_r <- as.factor(PC18$SEXE_r)

PC18 <- PC18 %>% mutate(    
  AGE_r = case_when(
    AGE >= 15 & AGE <= 25 ~ "15 - 25",
    AGE > 25 & AGE <= 35 ~ "26 - 35",
    AGE > 35 & AGE <= 45 ~ "36 - 45",
    AGE > 45 & AGE <= 55 ~ "46 - 55",
    AGE > 55 & AGE <= 65 ~ "56 - 65",
    AGE > 65 & AGE <= 75 ~ "66 - 75",
    AGE > 75 ~ "76+"
  ))
PC18$AGE_r <- as.factor(PC18$AGE_r)

PC18 <- PC18 %>% mutate(    
  AGE_5_r = case_when(
    AGE >= 15 & AGE <= 20 ~ "15 - 20",
    AGE > 20 & AGE <= 25 ~ "21 - 25",
    AGE > 25 & AGE <= 30 ~ "26 - 30",
    AGE > 30 & AGE <= 35 ~ "31 - 35",
    AGE > 35 & AGE <= 40 ~ "36 - 40",
    AGE > 40 & AGE <= 45 ~ "41 - 45",
    AGE > 45 & AGE <= 50 ~ "46 - 50",
    AGE > 50 & AGE <= 55 ~ "51 - 55",
    AGE > 55 & AGE <= 60 ~ "56 - 60",
    AGE > 60 & AGE <= 65 ~ "61 - 65",
    AGE > 65 & AGE <= 70 ~ "66 - 70",
    AGE > 70 & AGE <= 75 ~ "71 - 75",
    AGE > 75 & AGE <= 80 ~ "76 - 80",
    AGE > 80 ~ "81+"
  ))
PC18$AGE_5_r <- as.factor(PC18$AGE_5_r)

PC18 <- PC18 %>% mutate(    
  CRITREVENU_r = case_when(
    CRITREVENU == 1 ~ "Moins de 800",
    CRITREVENU == 2 ~ "De 800 à 999" ,
    CRITREVENU == 3 ~ "De 1000 à 1199",
    CRITREVENU == 4 ~ "De 1200 à 1499",
    CRITREVENU == 5 ~ "De 1500 à 1999",
    CRITREVENU == 6 ~ "De 2000 à 2499",
    CRITREVENU == 7 ~ "De 2500 à 2999",
    CRITREVENU == 8 ~ "De 3000 à 3999",
    CRITREVENU == 9 ~ "De 4000 à 5999",
    CRITREVENU == 10 ~ "6000 ou plus"
  ))
PC18$AGE_5_r <- as.factor(PC18$AGE_5_r)

## Recodage de PC18$G_PCS_MENAGE_ en PC18$PCS_MENAGE
PC18$PCS_MENAGE <- fct_recode(PC18$G_PCS_MENAGE_,
                           NULL = "",
                           NULL = "1971",
                           NULL = "2001",
                           "Dominante cadre" = "I",
                           "Dominante intermédiaire" = "II",
                           "Dominante employée" = "III",
                           "Dominante indépendante" = "IV",
                           "Dominante ouvrière" = "V",
                           "Un employé ou ouvrier" = "VI",
                           "Inactifs (hors retraités)" = "VII"
)

#renome h de travail par semaine. 4812 NA, pour les gens qui ne travail pas.
PC18$h_travail_semaine <- PC18$S11_C_1

# Diplome en 3 et 5 postes
PC18 <- PC18 %>% mutate(    
  diplome_3p = case_when(
    DIPLOM == 1 ~ "Inf. Bac",
    DIPLOM == 2 ~ "Inf. Bac",
    DIPLOM == 3 ~ "Inf. Bac",
    DIPLOM == 4 ~ "Inf. Bac",
    DIPLOM == 5 ~ "Inf. Bac",
    DIPLOM == 6 ~ "Inf. Bac",
    DIPLOM == 7 ~ "Bac",
    DIPLOM == 8 ~ "Bac",
    DIPLOM == 9 ~ "Bac",
    DIPLOM == 10 ~ "Sup. Bac",
    DIPLOM == 11 ~ "Sup. Bac",
    DIPLOM == 12 ~ "Sup. Bac",
    DIPLOM == 13 ~ "Sup. Bac"
  ))
PC18$diplome_3p <- as.factor(PC18$diplome_3p)

PC18 <- PC18 %>% mutate(    
  DIPLOME_r = case_when(
    DIPLOM == 1 ~ "Inf. Bac",
    DIPLOM == 2 ~ "Inf. Bac",
    DIPLOM == 3 ~ "Inf. Bac",
    DIPLOM == 4 ~ "Inf. Bac",
    DIPLOM == 5 ~ "Inf. Bac",
    DIPLOM == 6 ~ "Inf. Bac",
    DIPLOM == 7 ~ "Bac",
    DIPLOM == 8 ~ "Bac",
    DIPLOM == 9 ~ "Bac",
    DIPLOM == 10 ~ "Bac +2/3",
    DIPLOM == 11 ~ "Bac +2/3",
    DIPLOM == 12 ~ "Bac +5",
    DIPLOM == 13 ~ "Doctorat"
  ))
PC18$DIPLOME_r <- as.factor(PC18$DIPLOME_r)

#GranPC18 domaine d'étude du diplome le plus haut obtenu

#On retire les deux dernier chiffre du code
PC18$matiere_diplome <- as.numeric(PC18$SPECIAL_CODE_CITE_13)
PC18$matiere_diplome <- trunc(PC18$matiere_diplome/100)
## Recodage de PC18$matiere_diplome
PC18$matiere_diplome <- as.character(PC18$matiere_diplome)
PC18$matiere_diplome <- fct_recode(PC18$matiere_diplome,
  "Certif. generique" = "0",
  "Education" = "1",
  "Lettres et arts" = "2",
  "SHS et journalisme" = "3",
  "Commerce, admin. et droit" = "4",
  "Sciences nat., maths et stats" = "5",
  "TIC" = "6",
  "Ingénierie et construction" = "7",
  "Agriculture et vétérinaire" = "8",
  "Santé" = "9",
  "Services" = "10"
)

# Variable synthétique lieu de naissance des parents
PC18 <- PC18 %>% mutate(    
  naiss_parents = case_when(
    NAIM == 1 & NAIP == 1 ~ "Deux parents nés en France",
    xor(NAIM == 2, NAIP == 2) ~ "Un des deux parents né à l'étranger",
    NAIM == 2 & NAIP == 2 ~ "Deux parents nés à l'étranger"
  ))
PC18$naiss_parents <- as.factor(PC18$naiss_parents)


# Diplome et PCS des parents

#Pere
PC18 <- PC18 %>% mutate(    
  DIPLOME_pere = case_when(
    M19 == 1 ~ "Inf. Bac",
    M19 == 2 ~ "Inf. Bac",
    M19 == 3 ~ "Inf. Bac",
    M19 == 4 ~ "Inf. Bac",
    M19 == 5 ~ "Inf. Bac",
    M19 == 6 ~ "Inf. Bac",
    M19 == 7 ~ "Bac",
    M19 == 8 ~ "Bac",
    M19 == 9 ~ "Bac",
    M19 == 10 ~ "Sup. Bac",
    M19 == 11 ~ "Sup. Bac",
    M19 == 12 ~ "Sup. Bac",
    M19 == 13 ~ "Sup. Bac"
  ))
PC18$DIPLOME_pere <- as.factor(PC18$DIPLOME_pere)


PC18$CS_pere <- trunc(PC18$CSTOT_PER/10)
PC18 <- PC18 %>% mutate(    
  CS_pere = case_when(
    CS_pere == 1 ~ "Agriculteur",
    CS_pere == 2 ~ "Artisant/commerçant",
    CS_pere == 3 ~ "Cadre et prof. intel.",
    CS_pere == 4 ~ "Prof. inter.",
    CS_pere == 5 ~ "Employé",
    CS_pere == 6 ~ "Ouvrier",
    CS_pere == 9 ~ "Autres inactifs"
  ))
PC18$CS_pere <- as.factor(PC18$CS_pere)

# Mere
PC18 <- PC18 %>% mutate(    
  DIPLOME_mere = case_when(
    M21 == 1 ~ "Inf. Bac",
    M21 == 2 ~ "Inf. Bac",
    M21 == 3 ~ "Inf. Bac",
    M21 == 4 ~ "Inf. Bac",
    M21 == 5 ~ "Inf. Bac",
    M21 == 6 ~ "Inf. Bac",
    M21 == 7 ~ "Bac",
    M21 == 8 ~ "Bac",
    M21 == 9 ~ "Bac",
    M21 == 10 ~ "Sup. Bac",
    M21 == 11 ~ "Sup. Bac",
    M21 == 12 ~ "Sup. Bac",
    M21 == 13 ~ "Sup. Bac"
  ))
PC18$DIPLOME_mere <- as.factor(PC18$DIPLOME_mere)



PC18$CS_mere <- trunc(PC18$CSTOT_MER/10)
PC18 <- PC18 %>% mutate(    
  CS_mere = case_when(
    CS_mere == 1 ~ "Agriculteur",
    CS_mere == 2 ~ "Artisant/commerçant",
    CS_mere == 3 ~ "Cadre et prof. intel.",
    CS_mere == 4 ~ "Prof. inter.",
    CS_mere == 5 ~ "Employé",
    CS_mere == 6 ~ "Ouvrier",
    CS_mere == 9 ~ "Autres inactifs"
  ))
PC18$CS_mere <- as.factor(PC18$CS_mere)


## Recodage de PC18$VITENCOUPLE en PC18$VITENCOUPLE_r
PC18$VITENCOUPLE_r <- fct_recode(PC18$VITENCOUPLE,
                              "Oui, vit dans logement" = "1",
                              "Oui, vit pas dans logement" = "2",
                              "Non" = "3",
                              NULL = "4",
                              NULL = "421,421",
                              NULL = "5",
                              NULL = ""
)

## Recodage de PC18$STOC en PC18$logement
PC18$logement <- as.character(PC18$STOC)
PC18$logement <- fct_recode(PC18$logement,
                         "Propriétaire (crédit en cours)" = "1",
                         "Propriétaire (sans crédit)" = "2",
                         "Usufruitier" = "3",
                         "Locataire" = "4",
                         "A titre gratuit" = "5"
)

#########################################
#### Sorties (hors culture legitime) ####
#########################################


## Recodage de PC18$A8 en PC18$sorties_ami
PC18$sorties_ami <- as.character(PC18$A8)
PC18$sorties_ami <- fct_recode(PC18$sorties_ami,
                            "Plusieurs fois/semaine" = "1",
                            "Plusieurs fois/mois" = "2",
                            "Plusieurs fois/mois" = "3",
                            "Une fois par mois" = "4",
                            "Rarement" = "5",
                            "Jamais" = "6",
                            NULL = "7",
                            NULL = "8"
)

# Variable synthétique "est partie a l'étranger pour vacances au cours des 12 derniers mois
PC18 <- PC18 %>% mutate(    
  voyage_etranger = case_when(
    A71 == 1 ~ 1,
    A72 == 1 ~ 1,
    A73 == 1 ~ 0,
    A5 == 2 ~ 0
  ))



############################
#### Pratiques amateurs ####
############################

PC18$music_amateur <- PC18$A1901

######################
#### Jeux vidéos #####
######################

## Recodage de PC18$B2 en PC18$freq_jv
PC18$freq_jv <- as.character(PC18$B2)
PC18$freq_jv <- fct_recode(PC18$freq_jv,
  "Tous les jours" = "1",
  "Une à plusieurs fois/semaine" = "2",
  "Une à plusieurs fois/semaine" = "3",
  "1 à 3 fois/mois" = "4",
  "Plus rarement" = "5",
  "Plus rarement" = "6",
  NULL = "7",
  NULL = "8"
)
PC18$freq_jv <- fct_explicit_na(PC18$freq_jv, "Ne joue pas aux JV")

######################
#### Audiovisuel #####
######################

## Recodage de PC18$C1 en PC18$freq_tv
PC18$freq_tv <- as.character(PC18$C1)
PC18$freq_tv <- fct_recode(PC18$freq_tv,
  "Tous les jours" = "1",
  "1 à 4 jours/semaine" = "2",
  "1 à 4 jours/semaine" = "3",
  "Plus rarement" = "4",
  "Jamais" = "5",
  NULL = "6",
  NULL = "7"
)

# Type d'équipement pour  régarder la TV
PC18 <- PC18 %>% mutate(    
  equip_tv = case_when(
    C61 == 1 & C62 == 0 & C63 == 0 & C64 == 0 ~ "Uniquement écran de TV",
    C61 == 0 ~ "Uniquement numérique",
    C61 == 1 & (C62 == 1 | C63 == 1 | C64 == 1) ~ "Mixte",
    is.na(C61) ~ "Ne regarde pas la TV"
  ))
PC18$equip_tv <- as.factor(PC18$equip_tv)

#Regarder des clip de musique à la TV. Je recode les NA en non, ils correspondent aux gens qui ne regarde pas la TV.

PC18$clip_tv <- PC18$C710
PC18$clip_tv[is.na(PC18$clip_tv)] <- 0

#### FILM

## Recodage de PC18$C19 en PC18$freq_film
PC18$freq_film <- as.character(PC18$C19)
PC18$freq_film <- fct_recode(PC18$freq_film,
  "Tous les jours" = "1",
  "Une fois/semaine mini" = "2",
  "Une fois/mois mini" = "3",
  "Plus rarement" = "4",
  "Jamais" = "5",
  NULL = "6",
  NULL = "7"
)

# Type d'équipement pour  régarder des films
PC18 <- PC18 %>% mutate(    
  equip_film = case_when(
    C211 == 1 & C212 == 0 & C213 == 0 & C214 == 0 & C215 == 0 ~ "Uniquement écran de TV",
    C211 == 0 ~ "Uniquement numérique",
    C211 == 1 & (C212 == 1 | C213 == 1 | C214 == 1 | C215 == 1) ~ "Mixte",
    is.na(C211) ~ "Ne regarde pas de film"
  ))
PC18$equip_film <- as.factor(PC18$equip_film)

# Sur les supports pour regarder des films, difficile de faire une variables synthétique. Veut-on tout garder...?
# Ou alors variable synthétique Stream versus non stream ? Numérique versus TV et DVPC18 ?

# Regarder des des films en streaming plateforme VOPC18 (Netflix etc). 
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de films.
PC18$film_stream_VOD <- PC18$C222
PC18$film_stream_VOD[is.na(PC18$film_stream_VOD)] <- 0

# Regarder des films en replay 
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de films.
PC18$film_replay <- PC18$C224
PC18$film_replay[is.na(PC18$film_replay)] <- 0

# Regarder des films en stream en dehors des plateformes
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de films.
PC18$film_stream_autre <- PC18$C225
PC18$film_stream_autre[is.na(PC18$film_stream_autre)] <- 0

# Regarder des films en DVD
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de films.
PC18$film_DVD <- PC18$C221
PC18$film_DVD[is.na(PC18$film_DVD)] <- 0

# Regarder des films sur fichiers numérique
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de films.
PC18$film_num <- PC18$C226
PC18$film_num[is.na(PC18$film_num)] <- 0


# Nombre de genre cinématographiques regardés. Les gens qui ne regardent pas de film sont codés 0.
list <- c("C2601", "C2602", "C2603", "C2604", "C2605", "C2606", "C2607", 
          "C2608", "C2609", "C2610", "C2611", "C2612", "C2613", "C2614", "C2615", "C2616")

PC18$nbr_genre_film <- rowSums(PC18[,list])
PC18$nbr_genre_film[is.na(PC18$nbr_genre_film)] <- 0


##### Séries 

## Recodage de PC18$C19 en PC18$freq_serie
PC18$freq_serie <- as.character(PC18$C31)
PC18$freq_serie <- fct_recode(PC18$freq_serie,
                          "Tous les jours" = "1",
                          "Une fois/semaine mini" = "2",
                          "Une fois/mois mini" = "3",
                          "Plus rarement" = "4",
                          "Jamais" = "5",
                          NULL = "6",
                          NULL = "7"
)

PC18 <- PC18 %>% mutate(    
  equip_serie = case_when(
    C331 == 1 & C332 == 0 & C333 == 0 & C334 == 0 & C335 == 0 ~ "Uniquement écran de TV",
    C331 == 0 ~ "Uniquement numérique",
    C331 == 1 & (C332 == 1 | C333 == 1 | C334 == 1 | C335 == 1) ~ "Mixte",
    is.na(C331) ~ "Ne regarde pas de serie"
  ))
PC18$equip_serie <- as.factor(PC18$equip_serie)


#Meme remarque que pour les films


# Regarder des series en streaming plateforme VOPC18 (Netflix etc). 
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de series.
PC18$serie_stream_VOD <- PC18$C342
PC18$serie_stream_VOD[is.na(PC18$serie_stream_VOD)] <- 0

# Regarder des series en replay 
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de series.
PC18$serie_replay <- PC18$C344
PC18$serie_replay[is.na(PC18$serie_replay)] <- 0

# Regarder des series en stream en dehors des plateformes
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de series.
PC18$serie_stream_autre <- PC18$C345
PC18$serie_stream_autre[is.na(PC18$serie_stream_autre)] <- 0

# Regarder des series en DVD
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de series.
PC18$serie_DVD <- PC18$C341
PC18$serie_DVD[is.na(PC18$serie_DVD)] <- 0

# Regarder des series sur fichiers numérique
# Je recode les NA en non, ils correspondent aux gens qui ne regarde pas de series.
PC18$serie_num <- PC18$C346
PC18$serie_num[is.na(PC18$serie_num)] <- 0

# Nombre de genre cinématographiques regardés. Les gens qui ne regardent pas de film sont codés 0.
list <- c("C3801", "C3802", "C3803", "C3804", "C3805", "C3806", "C3807", 
          "C3808", "C3809", "C3810", "C3811", "C3812", "C3813", "C3814", "C3815", "C3816")

PC18$nbr_genre_serie <- rowSums(PC18[,list])
PC18$nbr_genre_serie[is.na(PC18$nbr_genre_serie)] <- 0

###################################
#### Information et actualité #####
###################################

# varaible synthétique moyen d'information internet

PC18 <- PC18 %>% mutate(    
  info_internet = case_when(
    D34 == 1 | D35 == 1 | D36 == 1 | D37 == 1 ~ "S'informe en ligne",
    D34 == 0 | D35 == 0 | D36 == 0 | D37 == 0 ~ "Ne s'informe pas en ligne",
    is.na(D34) ~ "Ne suis pas l'actu"
  ))
PC18$info_internet <- as.factor(PC18$info_internet)

## Recodage de PC18$D2 en PC18$D2_rec
PC18$freq_info <- as.character(PC18$D2)
PC18$freq_info <- fct_recode(PC18$freq_info,
                          "Tous les jours" = "1",
                          "Une ou plusieurs/semaines" = "2",
                          "Une ou plusieurs/semaines" = "3",
                          "Plus rarement" = "4",
                          "Plus rarement" = "5",
                          NULL = "6"
)



##################
#### Lecture #####
##################

PC18$autojuge_lecture <- as.character(PC18$F5)
PC18$autojuge_lecture <- fct_recode(PC18$autojuge_lecture,
                                 "Beaucoup" = "1",
                                 "Moyennement" = "2",
                                 "Peu" = "3",
                                 "Pas" = "4",
                                 NULL = "5",
                                 NULL = "6"
)

PC18$freq_lecture <- as.character(PC18$F11)
PC18$freq_lecture <- fct_explicit_na(PC18$freq_lecture, na_level = "Ne lit pas")
PC18$freq_lecture <- fct_recode(PC18$freq_lecture,
                             "Tous les jours" = "1",
                             "1/semaine au moins" = "2",
                             "1/mois au mois" = "3",
                             "Plus rarement" = "4",
                             "Plus rarement" = "5",
                             NULL = "6",
                             NULL = "7"
)

# Variable synthétique lecture sur support numérique

PC18 <- PC18 %>% mutate(    
  equip_lecture = case_when(
    F121 == 1 & F122 == 0 & F123 == 0 & F124 == 0 ~ "Uniquement livre papier",
    F121 == 0 ~ "Uniquement numérique ou autre",
    F121 == 1 & (F122 == 1 | F123 == 1 | F124 == 1) ~ "Mixte",
    is.na(F121) ~ "Ne lit pas de livre"
  ))
PC18$equip_lecture <- as.factor(PC18$equip_lecture)

## Recodage de PC18$F9 en PC18$lecture_nonFR
PC18$lecture_nonFR <- as.character(PC18$F9)
PC18$lecture_nonFR <- fct_recode(PC18$lecture_nonFR,
  "Oui" = "1",
  "Non" = "2",
  NULL = "3",
  NULL = "4"
)
PC18$lecture_nonFR <- fct_explicit_na(PC18$lecture_nonFR, na_level = "Ne lit pas de livre")

#######################################
#### Sortie culturelles legitimes #####
#######################################

# Nombre de genre cinématographiques vue au cinéma. Les gens qui ne vont pas au cinéma 0.
list <- c("G701", "G702", "G703", "G704", "G705", "G706", "G707", 
          "G708", "G709", "G710", "G711", "G712", "G713", "G714", "G715", "G716")

PC18$nbr_genre_film_cine <- rowSums(PC18[,list])
PC18$nbr_genre_film_cine[is.na(PC18$nbr_genre_film_cine)] <- 0

### Faire une variable d'indicateur de sortie culturelles spectacle vivant

# A visité un musée d'art dans les 12 mois

PC18 <- PC18 %>% mutate(    
  musee_art_12m = case_when(
    H301 == 1 | H302 == 1 | H307 == 1 ~ 1,
    H301 == 0 & H302 == 0 & H307 == 0 ~ 0,
    is.na(H301) ~ 0
  ))

# galerie d'art au cours des 12 mois
PC18$galerie_12m <- PC18$H206

###############################
#### Equipement multimédia ####
###############################

# foyer avec un ordinateur ou une tablette

PC18 <- PC18 %>% mutate(    
  ordi = case_when(
    I108 == 1 | I109 == 1 | I110 == 1 ~ 1,
    I108 == 0 & I109 == 0 & I110 == 0 ~ 0
  ))

PC18$acces_internet <- PC18$I112

## Recodage de PC18$I4 en PC18$freq_internet
PC18$freq_internet <- as.character(PC18$I4)
PC18$freq_internet <- fct_recode(PC18$freq_internet,
  "Tous les jours" = "1",
  "Plusieurs fois/semaine" = "2",
  "Plus rarement" = "3",
  "Plus rarement" = "4",
  "Jamais" = "5",
  NULL = "6",
  NULL = "7"
)

## Recodage de PC18$I5 en PC18$reseaux_sociaux
PC18$reseaux_sociaux <- as.character(PC18$I5)
PC18$reseaux_sociaux <- fct_recode(PC18$reseaux_sociaux,
  "0" = "1",
  "1" = "2",
  NULL = "3",
  NULL = "4"
)
PC18$reseaux_sociaux <- as.numeric(as.character(PC18$reseaux_sociaux))

# A assisté à une expo, un concert ou un spectacle sur internet

PC18 <- PC18 %>% mutate(    
  culture_en_ligne = case_when(
    I72 == 1 | I73 == 1 | I74 == 1 | I75 == 1 ~ 1,
    I72 == 0 & I73 == 0 & I74 == 0 ~ 0,
    is.na(I72) ~ 0
  ))

###########################
#### Situation enfance ####
###########################


PC18$tv_enfance <- as.character(PC18$M1_SQ1)
PC18$tv_enfance <- fct_recode(PC18$tv_enfance,
  "Souvent" = "1",
  "De temps en temps" = "2",
  "Rarement" = "3",
  "Jamais" = "4",
  NULL = "5",
  NULL = "6"
)

PC18$musique_enfance <- as.character(PC18$M1_SQ2)
PC18$musique_enfance <- fct_recode(PC18$musique_enfance,
                                "Souvent" = "1",
                                "De temps en temps" = "2",
                                "Rarement" = "3",
                                "Jamais" = "4",
                                NULL = "5",
                                NULL = "6"
)

PC18$cinema_enfance <- as.character(PC18$M1_SQ5)
PC18$cinema_enfance <- fct_recode(PC18$cinema_enfance,
                                "Souvent" = "1",
                                "De temps en temps" = "2",
                                "Rarement" = "3",
                                "Jamais" = "4",
                                NULL = "5",
                                NULL = "6"
)

# variable synthéthique nbr genre écouté par les parents
list <- c("M301", "M302", "M303", "M304", "M305", "M306", "M307", 
          "M308", "M309", "M310", "M311", "M312", "M313")

PC18$nbr_genre_parent_ecoute <- rowSums(PC18[,list])
PC18$nbr_genre_parent_ecoute[PC18$M314 == 1] <- 0

# variable synthéthique nbr genre écouté à 12 ans

list <- c("M201", "M202", "M203", "M204", "M205", "M206", "M207", 
          "M208", "M209", "M210", "M211", "M212", "M213")

PC18$nbr_genre_ecoute_enfance <- rowSums(PC18[,list])
PC18$nbr_genre_ecoute_enfance[PC18$M214 == 1] <- 0

#########################
#### Cosmopolitisme #####
#########################

# Variable synthétique audiovisuel (tv, film série) en langue autre que FR

PC18 <- PC18 %>% mutate(    
  audivisuel_nonFR = case_when(
    C8 == 1 | C8 == 2 | C8 == 3 | C24 == 1 | C36 == 1 ~ 1,
    TRUE ~ 0
  ))

# matrise une autre langue que le francais
PC18$autre_langue <- PC18$M122 

## Recodage de PC18$D6 en PC18$info_nonFR
PC18$info_nonFR <- as.character(PC18$D6)
PC18$info_nonFR <- fct_recode(PC18$info_nonFR,
  "Oui" = "1",
  "Non" = "2",
  NULL = "3",
  NULL = "4"
)
PC18$info_nonFR <- fct_explicit_na(PC18$info_nonFR, na_level = "Ne s'informe pas")



##################
#### Musique #####
##################

## Fréquence d'écoute de musique
PC18$music_12m <- as.character(PC18$E7)
PC18$music_12m <- fct_recode(PC18$music_12m,
                          "Tous les jours" = "1",
                          "Une à plusieurs fois/semaine" = "2",
                          "Une à plusieurs fois/semaine" = "3",
                          "Rarement" = "4",
                          "Rarement" = "5",
                          "Jamais" = "6",
                          NULL = "7",
                          NULL = "8"
)



## Recodage de PC18$E83 en PC18$stream_spe
PC18$stream_spe <- PC18$E83

#Variable synthétique nbr styles écoutés

list <- c("E1001", "E1002", "E1003", "E1004", "E1005", "E1006", "E1007", 
          "E1008", "E1009", "E1010", "E1011", "E1012", "E1013")

PC18$nbr_genre_music <- rowSums(PC18[,list])
PC18$nbr_genre_music[is.na(PC18$nbr_genre_music)] <- 0






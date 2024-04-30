library(tidyverse)
library(here)
library(questionr)
library(mice)
library(gt)
library(gtsummary)
library(cobalt)
library(survey)
library(smd)
library(ggthemes)

##################
# Make base data #
##################


PC18 <- read.csv(here("data", "pc18_quetelet_octobre2021.csv"),sep=";")

# Filter out 20 yo and less
PC18 <- PC18 %>% filter(AGE > 20)

#------------------#
#### Socio démo ####
#------------------#

PC18 <- subset(PC18, !is.na(PC18$POND))


# Scaling pond
tmp <- sum(PC18$POND)/nrow(PC18) 
PC18$POND <- PC18$POND/tmp

PC18 <- PC18 %>% mutate(    
  SEXE_r = case_when(
    SEXE == 1 ~ "Homme",
    SEXE == 2 ~ "Femme"
  ))
PC18$SEXE_r <- as.factor(PC18$SEXE_r)

PC18 <- PC18 %>% mutate(    
  SEXE_femme = case_when(
    SEXE == 1 ~ 0,
    SEXE == 2 ~ 1
  ))

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
                              "Dominante indépendante" = "IV",
                              "Dominante employée" = "III",
                              "Dominante ouvrière" = "V",
                              "Un employé ou ouvrier" = "VI",
                              "Inactifs (hors retraités)" = "VII"
)

## Réordonnancement de PC18$PCS_MENAGE
PC18$PCS_MENAGE <- fct_relevel(
  PC18$PCS_MENAGE,
  "Dominante cadre", "Dominante intermédiaire", "Dominante indépendante",
  "Dominante employée", "Dominante ouvrière", "Un employé ou ouvrier",
  "Inactifs (hors retraités)"
)

PC18 <- PC18 %>% mutate(class = fct_recode(PCS_MENAGE,
                                           "Upper class" = "Dominante cadre",
                                           "Middle class" = "Dominante intermédiaire",
                                           "Middle class" = "Dominante indépendante",
                                           "Working class" = "Dominante employée",
                                           "Working class" = "Dominante ouvrière",
                                           "Working class" = "Un employé ou ouvrier",
                                           "Working class" = "Inactifs (hors retraités)"))


#renome h de travail par semaine. 4812 NA, pour les gens qui ne travail pas.
PC18$h_travail_semaine <- PC18$S11_C_1
PC18$h_travail_semaine[is.na(PC18$h_travail_semaine)] <- 0

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

## Réordonnancement de PC18$diplome_3p
PC18$diplome_3p <- PC18$diplome_3p %>%
  fct_relevel(
    "Inf. Bac", "Bac", "Sup. Bac"
  )

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
    DIPLOM == 13 ~ "Bac +5"
  ))
PC18$DIPLOME_r <- as.factor(PC18$DIPLOME_r)

## Réordonnancement de PC18$DIPLOME_r
PC18$DIPLOME_r <- fct_relevel(
  PC18$DIPLOME_r,
  "Inf. Bac", "Bac", "Bac +2/3", "Bac +5"
)

PC18 <- PC18 %>% mutate(    
  DIPLOME_eng = case_when(
    DIPLOM == 1 ~ "Less than High School",
    DIPLOM == 2 ~ "Less than High School",
    DIPLOM == 3 ~ "Less than High School",
    DIPLOM == 4 ~ "Less than High School",
    DIPLOM == 5 ~ "Less than High School",
    DIPLOM == 6 ~ "Less than High School",
    DIPLOM == 7 ~ "High School",
    DIPLOM == 8 ~ "High School",
    DIPLOM == 9 ~ "High School",
    DIPLOM == 10 ~ "Some college, bachelor degree",
    DIPLOM == 11 ~ "Some college, bachelor degree",
    DIPLOM == 12 ~ "Graduate education",
    DIPLOM == 13 ~ "Graduate education"
  ))
PC18$DIPLOME_eng <- as.factor(PC18$DIPLOME_eng)

## Réordonnancement de PC18$DIPLOME_r
PC18$DIPLOME_eng <- fct_relevel(
  PC18$DIPLOME_eng,
  "Less than High School", "High School", "Some college, bachelor degree", "Graduate education"
)


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

PC18 <- PC18 %>% 
  mutate(SITUA = factor(SITUA, levels = 1L:8L,
                        labels = c('Occupe un emploi',
                                   'Apprenti(e) sous contrat ou stagiaire rémunéré',
                                   'Etudiant(e), élève, en formation ou stagiaire non rémunéré',
                                   'Chômeur (inscrit(e) ou non au Pôle Emploi)',
                                   'Retraité(e) ou retiré(e) des affaires ou en préretraite',
                                   'Femme ou homme au foyer',
                                   'Inactif(ve) pour cause d’invalidité',
                                   'Autre situation d’inactivité')) %>% 
                          fct_collapse(`etudiant apprenti stagiaire` = c('Apprenti(e) sous contrat ou stagiaire rémunéré',
                                                                         'Etudiant(e), élève, en formation ou stagiaire non rémunéré'),
                                       `inactifs hors retraites` = c('Femme ou homme au foyer',
                                                                     'Inactif(ve) pour cause d’invalidité',
                                                                     'Autre situation d’inactivité')
                                       )
         )
PC18 <- PC18 %>% 
  mutate(sante = factor(A15, levels = 1L:5L,
                        labels = c("Très bon", "Bon", "Assez bon", "Mauvais", "Très mauvais")),
         tempslibre = factor(A2, levels = 1L:3L, labels = c("Oui, souvent", "Oui, de temps en temps", "Non, jamais")))


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

#---------------------------------------#
#### Sorties (hors culture legitime) ####
#---------------------------------------#


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



#--------------------------#
#### Pratiques amateurs ####
#--------------------------#

PC18$music_amateur <- PC18$A1901

PC18$numerique_pratiques_amateur <- (1-PC18$A314)
PC18$numerique_pratiques_amateur <- factor(PC18$numerique_pratiques_amateur,
                                           levels =c(0, 1), 
                                           labels = c("non", "oui")) %>% 
  fct_na_value_to_level("pas de pratique amateur")

#--------------------#
#### Jeux vidéos #####
#--------------------#

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
PC18$freq_jv <- fct_na_value_to_level(PC18$freq_jv, "Ne joue pas aux JV")

#--------------------#
#### Audiovisuel #####
#--------------------#

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

#Nombre genre film aimés
list <- c("C2701", "C2702", "C2703", "C2704", "C2705", "C2706", "C2707", 
          "C2708", "C2709", "C2710", "C2711", "C2712", "C2713", "C2714", "C2715", "C2716")

PC18$nbr_genre_film_aime <- rowSums(PC18[,list])
PC18$nbr_genre_film_aime[is.na(PC18$nbr_genre_film_aime)] <- 0

#Nombre genre film detestés
list <- c("C2801", "C2802", "C2803", "C2804", "C2805", "C2806", "C2807", 
          "C2808", "C2809", "C2810", "C2811", "C2812", "C2813", "C2814", "C2815", "C2816")

PC18$nbr_genre_film_deteste <- rowSums(PC18[,list])
PC18$nbr_genre_film_deteste[is.na(PC18$nbr_genre_film_deteste)] <- 0


# nombre de film vue dans la liste

list <- c("C2901", "C2902", "C2903", "C2904", "C2905", "C2906", "C2907", "C2908", "C2909", "C2910", 
          "C2911", "C2912", "C2913", "C2914")

PC18$nbr_film_vu <- rowSums(PC18[,list])
PC18$nbr_film_vu[is.na(PC18$nbr_film_vu)] <- 0


## Recodage de PC18$C30 en PC18$C30_rec
PC18$film_manque <- PC18$C30 %>%
  as.character() %>%
  fct_recode(
    "Beaucoup" = "1",
    "Un peu" = "2",
    "Pas tellement" = "3",
    "Pas du tout" = "4",
    NULL = "5"
  ) %>% 
  fct_na_value_to_level("pas de films")


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


PC18$serie_manque <- PC18$C42 %>%
  as.character() %>%
  fct_recode(
    "Beaucoup" = "1",
    "Un peu" = "2",
    "Pas tellement" = "3",
    "Pas du tout" = "4",
    NULL = "5"
  ) %>% 
  fct_na_value_to_level("pas de series")


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

# nombre de genres particulièrement aimés

list <- c("C3901", "C3902", "C3903", "C3904", "C3905", "C3906", "C3907", 
          "C3908", "C3909", "C3910", "C3911", "C3912", "C3913", "C3914", "C3915", "C3916")

PC18$nbr_genre_serie_aime <- rowSums(PC18[,list])
PC18$nbr_genre_serie_aime[is.na(PC18$nbr_genre_serie_aime)] <- 0

# nombre de genres detestés

list <- c("C4001", "C4002", "C4003", "C4004", "C4005", "C4006", "C4007", 
          "C4008", "C4009", "C4010", "C4011", "C4012", "C4013", "C4014", "C4015", "C4016")

PC18$nbr_genre_serie_deteste <- rowSums(PC18[,list])
PC18$nbr_genre_serie_deteste[is.na(PC18$nbr_genre_serie_deteste)] <- 0

# nombre de série vu dans la liste

list <- c("C4101", "C4102", "C4103", "C4104", "C4105", "C4106", "C4107", 
          "C4108", "C4109", "C4110", "C4111", "C4112", "C4113", "C4114", "C4115")

PC18$nbr_serie_vu <- rowSums(PC18[,list])
PC18$nbr_serie_vu[is.na(PC18$nbr_serie_vu)] <- 0


##--------------------------------#
#### Information et actualité #####
#---------------------------------#

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



##---------------#
#### Lecture #####
##---------------#

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

# Nombre genre littéraires lus

list <- c("F601","F602", "F603", "F604", "F605","F606","F607","F608","F609","F610","F611","F612","F613","F614","F615","F616")

PC18$nbr_genre_livre <- rowSums(PC18[,list])
PC18$nbr_genre_livre[is.na(PC18$nbr_genre_livre)] <- 0

# Nombre genre JV joués

list <- c("B601","B602", "B603", "B604", "B605","B606","B607","B608","B609","B610","B611")

PC18$nbr_genre_jeuxvideo <- rowSums(PC18[,list])
PC18$nbr_genre_jeuxvideo[is.na(PC18$nbr_genre_jeuxvideo)] <- 0

# Nombre genre programme télé regardé
list <- c("C701", "C702", "C703", "C704", "C705", "C706", "C707", "C708", "C709", "C710")

PC18$nbr_genre_prgtele <- rowSums(PC18[,list])
PC18$nbr_genre_prgtele[is.na(PC18$nbr_genre_prgtele)] <- 0

# Nombre genre vidéo sur internet regardées
list <- c("C1701", "C1702", "C1703", "C1704", "C1705", "C1706", "C1707", "C1708", "C1709", "C1710")

PC18$nbr_genre_videointernet <- rowSums(PC18[,list])
PC18$nbr_genre_videointernet[is.na(PC18$nbr_genre_videointernet)] <- 0

# Nombre genre actualité intéressé
list <- c("D101", "D102", "D103", "D104", "D105", "D106", "D107", "D108", "D109", "D110", "D111", "D112", "D113", "D114")

PC18$nbr_genre_actu <- rowSums(PC18[,list])
PC18$nbr_genre_actu[is.na(PC18$nbr_genre_actu)] <- 0

# Nombre genre émissions radio écoutés
list <- c("E501", "E502", "E503", "E504", "E505", "E506", "E507", "E508", "E509", "E510", "E511")

PC18$nbr_genre_radio <- rowSums(PC18[,list])
PC18$nbr_genre_radio[is.na(PC18$nbr_genre_radio)] <- 0

# Nombre genre de musique écoutés à 12 ans
list <- c("M201", "M202", "M203", "M204", "M205", "M206", "M207", "M208", "M209", "M210", "M211", "M212", "M213")

PC18$nbr_genre_musique12ans <- rowSums(PC18[,list])
PC18$nbr_genre_musique12ans[is.na(PC18$nbr_genre_musique12ans)] <- 0

#-------------------------------------#
#### Sortie culturelles legitimes #####
#------------------------------------#

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

#-----------------------------#
#### Equipement multimédia ####
#-----------------------------#

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

#-------------------------#
#### Situation enfance ####
#-------------------------#


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

PC18$livre_enfance <- as.character(PC18$M1_SQ3)
PC18$livre_enfance <- fct_recode(PC18$livre_enfance,
                                  "Souvent" = "1",
                                  "De temps en temps" = "2",
                                  "Rarement" = "3",
                                  "Jamais" = "4",
                                  NULL = "5",
                                  NULL = "6"
)

PC18$musee_enfance <- as.character(PC18$M1_SQ8)
PC18$musee_enfance <- fct_recode(PC18$musee_enfance,
                                 "Souvent" = "1",
                                 "De temps en temps" = "2",
                                 "Rarement" = "3",
                                 "Jamais" = "4",
                                 NULL = "5",
                                 NULL = "6"
)

PC18$theatre_enfance <- as.character(PC18$M1_SQ9)
PC18$theatre_enfance <- fct_recode(PC18$theatre_enfance,
                                 "Souvent" = "1",
                                 "De temps en temps" = "2",
                                 "Rarement" = "3",
                                 "Jamais" = "4",
                                 NULL = "5",
                                 NULL = "6"
)

PC18 <- PC18 %>% 
  mutate(across(starts_with("A20_"), ~as.integer(.x < 16 & !is.na(.x)))) %>% 
  rename(amateur_musique_enfance = A20_musique,
         amateur_romans_enfance = A20_romans,
         amateur_peinture_enfance = A20_peinture,
         amateur_dessin_enfance = A20_dessin,
         amateur_theatre_enfance = A20_theatre,
         amateur_danse_enfance = A20_danse)

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

#-----------------------#
#### Cosmopolitisme #####
#-----------------------#


PC18$music_nonFR <- PC18$E15 %>%
  as.character() %>%
  fct_recode(
    "0" = "2",
    "0" = "3"
  ) %>%
  as.character() %>%
  as.numeric()


## Recodage de PC18$C24 en PC18$film_nonFR
PC18$film_nonFR <- PC18$C24 %>%
  as.character() %>%
  fct_recode(
    "0" = "2",
    "0" = "3"
  ) %>%
  as.character() %>%
  as.numeric()


PC18$serie_nonFR <- PC18$C36 %>%
  as.character() %>%
  fct_recode(
    "0" = "2",
    "0" = "3"
  ) %>%
  as.character() %>%
  as.numeric()

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



#----------------#
#### Musique #####
#----------------#

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

## Recodage de PC18$E7 en PC18$music_TLJ
PC18$music_TLJ <- as.character(PC18$E7)
PC18$music_TLJ <- fct_recode(PC18$music_TLJ,
                             "0" = "2",
                             "0" = "3",
                             "0" = "4",
                             "0" = "5",
                             "0" = "6",
                             NULL = "7",
                             NULL = "8"
)
PC18$music_TLJ <- as.numeric(as.character(PC18$music_TLJ))

## Recodage de PC18$E18 en PC18$E18_rec
PC18$music_ellememe <- PC18$E18 %>%
  as.character() %>%
  fct_recode(
    "Tous les jours" = "1",
    "De temps en temps" = "2",
    "Rarement" = "3",
    "Jamais" = "4",
    NULL = "5"
  )

## Recodage de PC18$E19 en PC18$E19_rec
PC18$music_manque <- PC18$E19 %>%
  as.character() %>%
  fct_recode(
    "Oui, beaucoup" = "1",
    "Oui, un peu" = "2",
    "Non" = "3",
    "Non" = "4",
    NULL = "5"
  )


### Modes d'écoute

PC18$cd_ou_cass <- PC18$E81

PC18$vinyle <- PC18$E82

## Recodage de PC18$E83 en PC18$stream_spe
PC18$stream_spe <- PC18$E83

PC18$stream_autre <- PC18$E84

PC18 <- PC18 %>% mutate(    
  stream_all = case_when(
    stream_spe == 1 | stream_autre == 1 ~ 1,
    stream_spe == 0 & stream_autre ==  0 ~  0
  ))

PC18$fichier_num <- PC18$E85

PC18$radio <- PC18$E86

PC18$tv <- PC18$E87

# type de plateforme
PC18 <- PC18 %>% mutate(    
  type_plateforme_stream = case_when(
    stream_spe == 1 & stream_autre == 1 ~ "Spécialisées et non-spécialisées",
    stream_spe == 1  ~  "Spécialisées uniquement",
    stream_autre == 1 ~ "Non-spécialisées uniquement",
  ))
PC18$type_plateforme_stream <- as.factor(PC18$type_plateforme_stream)




#Variable synthétique nbr styles écoutés

list <- c("E1001", "E1002", "E1003", "E1004", "E1005", "E1006", "E1007", 
          "E1008", "E1009", "E1010", "E1011", "E1012")

PC18$nbr_genre_music <- rowSums(PC18[,list])
PC18$nbr_genre_music[is.na(PC18$nbr_genre_music)] <- 0


PC18$ecoute_variet_inter <- PC18$E1004

PC18$ecoute_rap <- PC18$E1007

PC18$ecoute_classiq <- PC18$E1012

# variable synthétique nombre de genre aimé particulièrement
list <- c("E1201", "E1202", "E1203", "E1204", "E1205", "E1206", "E1207", 
          "E1208", "E1209", "E1210", "E1211", "E1212")

PC18$nbr_genre_aime <- rowSums(PC18[,list])
PC18$nbr_genre_aime[is.na(PC18$nbr_genre_aime)] <- 0


list <- c("E1301", "E1302", "E1303", "E1304", "E1305", "E1306", "E1307", 
          "E1308", "E1309", "E1310", "E1311", "E1312")

PC18$nbr_genre_deteste <- rowSums(PC18[,list])
PC18$nbr_genre_deteste[is.na(PC18$nbr_genre_deteste)] <- 0


# Variable synthétique nombre d'artiste déjà écouté parmis la liste

list <- c("E1401", "E1402", "E1403", "E1404", "E1405", "E1406", "E1407", 
          "E1408", "E1409", "E1410", "E1411", "E1412", "E1413", "E1414", "E1415")

PC18$nbr_artiste_ecoute <- rowSums(PC18[,list])
PC18$nbr_artiste_ecoute[is.na(PC18$nbr_artiste_ecoute)] <- 0


PC18$detest_clas <- PC18$E1312

PC18$aime_clas <- PC18$E1212

# Livre manque, cinema manque

PC18$livre_manque <- PC18$F17 %>%
  as.character() %>%
  fct_recode(
    "Beaucoup" = "1",
    "Un peu" = "2",
    "Pas tellement" = "3",
    "Pas du tout" = "4",
    NULL = "5"
  ) %>% 
  fct_na_value_to_level("pas de livre")

PC18$cinema_manque <- PC18$G111 %>%
  as.character() %>%
  fct_recode(
    "Beaucoup" = "1",
    "Un peu" = "2",
    "Pas tellement" = "3",
    "Pas du tout" = "4",
    NULL = "5",
    NULL = "6"
  ) %>% 
  fct_na_value_to_level("ne va pas au cinema")

# Input missing data for father and mother education
## Commented out: does not change the sample size of 
## matching outcome substantially, does not change
## the results either.
# predictor_matrix <- read_csv("data/missing_predictor_matrix.csv") %>% 
#   as.matrix()
# row.names(predictor_matrix) <- colnames(predictor_matrix)
# 
# missing <- PC18 %>% 
#   select(IDENT18, 
#          DIPLOME_pere, DIPLOME_mere, CS_pere, CS_mere, 
#          AGE_5_r, SEXE_r, naiss_parents, 
#          DIPLOME_r, PCS_MENAGE,
#          nbr_genre_parent_ecoute
#   ) %>% 
#   mice(m = 1, predictorMatrix = predictor_matrix)
# PC18 <- complete(missing) %>% 
#   select(IDENT18, DIPLOME_pere, DIPLOME_mere) %>% 
#   right_join(select(PC18, -DIPLOME_pere, -DIPLOME_mere))

###################################
#### Template Matching: Movies ####
###################################

library(MatchIt)
source(here("R", "gestion_NA_matching.R"))

list_var_match_film <- read_csv(here("data", "control_variables.csv")) %>% 
##################
# HERE IS THE TWEAK 1/3
##################
  filter(film, set != "digital use") %>% 
  pull(var)

# Clearning NA before matching
PC18_to_m_film <- clear_NA_to_m(PC18, list_var_match_film)

PC18_to_m_film <- subset(PC18_to_m_film, PC18_to_m_film$freq_film != "Jamais")

# Creating formula for matching

model_matching_film <- as.formula(paste0("film_stream_VOD",
                                         " ~ ", 
                                         paste(list_var_match_film, collapse = " + ")))


# Accepted SMD SPD for each covariate
tols_all_var = c(0.005, rep(0.05, times = length(list_var_match_film)-1))

PC18_to_m_film <- droplevels(PC18_to_m_film)

# Performing matching
res_match_template_stream_film_VOD <- matchit(model_matching_film,
                                              data = PC18_to_m_film, s.weights = PC18_to_m_film$POND, 
                                              method = "cardinality",
                                              estimand = "ATT", ratio = NA, discard = "none",  
                                              tols = tols_all_var, std.tols = T, solver = "gurobi", time = 60)


# Normalizing weights
PC18_m_film <- match.data(res_match_template_stream_film_VOD, weights = "POND_m")

tmp <- sum(PC18_m_film$POND_m)/nrow(PC18_m_film) 
PC18_m_film$POND_m <- PC18_m_film$POND_m/tmp

##################################
#### Template Matching: Music ####
##################################

list_var_match_music <- read_csv(here("data", "control_variables.csv")) %>% 
##################
# HERE IS THE TWEAK 2/3
##################
  filter(music, set != "digital use") %>% 
  pull(var)

# Clearning NA before matching
PC18_to_m_music <- clear_NA_to_m(PC18, list_var_match_music)

PC18_to_m_music <- subset(PC18_to_m_music, !is.na(stream_spe))

# Creating formula for matching
model_matching_music <- as.formula(paste0("stream_spe",
                                          " ~ ", 
                                          paste(list_var_match_music, collapse = " + ")))

# Accepted SMD SPD for each covariate
tols_all_var = c(0.005, rep(0.05, times = length(list_var_match_music)-1))

PC18_to_m_music <- droplevels(PC18_to_m_music)

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

###################################
#### Template Matching: Series ####
###################################

list_var_match_serie <- read_csv(here("data", "control_variables.csv")) %>% 
##################
# HERE IS THE TWEAK 2/3
##################
  filter(serie, set != "digital use") %>%  
  pull(var)

# Clearning NA before matching
PC18_to_m_serie <- clear_NA_to_m(PC18, list_var_match_serie)

PC18_to_m_serie <- subset(PC18_to_m_serie, PC18_to_m_serie$freq_serie != "Jamais")

PC18_to_m_serie <- droplevels(PC18_to_m_serie)


# Creating formula for matching
model_matching_serie <- as.formula(paste0("serie_stream_VOD",
                                          " ~ ", 
                                          paste(list_var_match_serie, collapse = " + ")))


# Accepted SMD SPD for each covariate
tols_all_var = c(0.005, rep(0.05, times = length(list_var_match_serie)-1))

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

###################
### Main result ###
###################

## Add weights

### music

PC18_to_m_music_survey <- survey::svydesign(id = ~IDENT18, data = PC18_to_m_music %>% 
                                              mutate(stream_spe = as.factor(stream_spe)), 
                                            weights = PC18_to_m_music$POND)


PC18_m_music_survey <- survey::svydesign(id = ~IDENT18, data = PC18_m_music %>% 
                                           mutate(stream_spe = as.factor(stream_spe)), 
                                         weights = PC18_m_music$POND_m)

### film

PC18_m_film_survey <- survey::svydesign(id = ~IDENT18, data = PC18_m_film, weights = PC18_m_film$POND_m)

PC18_to_m_film_survey <- survey::svydesign(id = ~IDENT18, data = PC18_to_m_film, weights = PC18_to_m_film$POND)

### Show

PC18_m_serie_survey <- survey::svydesign(id = ~IDENT18, data = PC18_m_serie, weights = PC18_m_serie$POND_m)

PC18_to_m_serie_survey <- survey::svydesign(id = ~IDENT18, data = PC18_to_m_serie, weights = PC18_to_m_serie$POND)


# Figure 1: Effects of streaming on tastes ------

## Music ------

### Diversity of genre

mean_genre_unm <- PC18_to_m_music_survey %>%
  tbl_svysummary(
    include = c("stream_spe", "nbr_genre_music", "nbr_genre_aime", "nbr_genre_deteste"),
    label = list(nbr_genre_music ~ "Styles musicaux écoutés",
                 nbr_genre_aime ~ "Styles particulièrement aimés",
                 nbr_genre_deteste ~ "Styles detestés"),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  add_difference() %>%
  modify_header(label = "**Nombre de :**",
                update = list(stat_1 ~ gt::html("**Non streameur**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**Streameur**<br> N = {n_unweighted}")))


mean_genre_m <- PC18_m_music_survey %>%
  tbl_svysummary(
    include = c("stream_spe", "nbr_genre_music", "nbr_genre_aime", "nbr_genre_deteste"),
    label = list(nbr_genre_music ~ "Styles musicaux écoutés",
                 nbr_genre_aime ~ "Styles particulièrement aimés",
                 nbr_genre_deteste ~ "Styles detestés"),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  add_difference() %>%
  modify_header(label = "**Nombre de :**",
                update = list(stat_1 ~ gt::html("**Non streameur**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**Streameur**<br> N = {n_unweighted}")))

t_comp_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Echantillon non matché**", "**Echantillon matché**")
) %>% as_gt() %>%
  tab_footnote(
    footnote = "Moyenne et écart type pondérés.",
    locations = cells_body(columns = stat_1_1,
                           rows = 1)
  )

t_comp_m

### Specific genre listened

mean_genre_unm <- PC18_to_m_music_survey %>%
  tbl_svysummary(
    include = c("E1001", "E1002", "E1003", "E1004", "E1005", "E1006", "E1007", 
                "E1008", "E1009", "E1010", "E1011", "E1012"), 
    label = list(E1001 ~ "Chanson",
                 E1002 ~ "World music",
                 E1003 ~ "Folk",
                 E1004 ~ "Pop",
                 E1005 ~ "RnB",
                 E1006 ~ "EDM",
                 E1007 ~ "Hip hop/rap",
                 E1008 ~ "Metal/Hard rock",
                 E1009 ~ "Rock",
                 E1010 ~ "Jazz",
                 E1011 ~ "Opera",
                 E1012 ~ "Classical music"),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0))
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


mean_genre_m <- PC18_m_music_survey %>%
  tbl_svysummary(
    include = c("E1001", "E1002", "E1003", "E1004", "E1005", "E1006", "E1007", 
                "E1008", "E1009", "E1010", "E1011", "E1012"), #
    label = list(E1001 ~ "Chanson",
                 E1002 ~ "World music",
                 E1003 ~ "Folk",
                 E1004 ~ "Pop",
                 E1005 ~ "RnB",
                 E1006 ~ "EDM",
                 E1007 ~ "Hip hop/rap",
                 E1008 ~ "Metal/Hard rock",
                 E1009 ~ "Rock",
                 E1010 ~ "Jazz",
                 E1011 ~ "Opera",
                 E1012 ~ "Classical music"),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0))
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

t_comp_genre_ecoute_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")
) %>% 
  modify_column_hide(columns = stat_2_1) %>% 
  as_gt()

t_comp_genre_ecoute_m


## Film ------

### Diversity of genre

mean_genre_unm <- PC18_to_m_film_survey %>%
  tbl_svysummary(
    include = c("film_stream_VOD", "nbr_genre_film", "nbr_genre_film_aime", "nbr_genre_film_deteste", "film_nonFR"), 
    label = list(nbr_genre_film ~ "Styles cinématographiques regardés",
                 nbr_genre_film_aime ~ "Styles cinématographiques particulièrement aimés",
                 nbr_genre_film_deteste ~ "Styles cinématographiques detestés"),
    by = film_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


mean_genre_m <- PC18_m_film_survey %>%
  tbl_svysummary(
    include = c("film_stream_VOD", "nbr_genre_film", "nbr_genre_film_aime", "nbr_genre_film_deteste", "film_nonFR"), 
    label = list(nbr_genre_film ~ "Styles cinématographiques regardés",
                 nbr_genre_film_aime ~ "Styles cinématographiques particulièrement aimés",
                 nbr_genre_film_deteste ~ "Styles cinématographiques detestés"),
    by = film_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

t_comp_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Echantillon non matché**", "**Echantillon matché**")
) %>% 
  as_gt()

t_comp_m

### Specific genre watched

mean_genre_unm <- PC18_to_m_film_survey %>%
  tbl_svysummary(
    include = c("C2601", "C2602", "C2603", "C2604", "C2605", "C2606", "C2607", 
                "C2608", "C2609", "C2610", "C2611", "C2612", "C2613", "C2614", "C2615", "C2616"), 
    label = list(C2601 ~ "Comedy",
                 C2602 ~ "Action",
                 C2603 ~ "Historical",
                 C2604 ~ "Thrillers",
                 C2605 ~ "Adventure",
                 C2606 ~ "Drama",
                 C2607 ~ "Animated",
                 C2608 ~ "Horror",
                 C2609 ~ "Film d'auteur",
                 C2610 ~ "Documentary",
                 C2611 ~ "Western",
                 C2612 ~ "Erotic",
                 C2613 ~ "Science fiction",
                 C2614 ~ "Romantic comedy",
                 C2615 ~ "Musicals ",
                 C2616 ~ "Political"),
    by = film_stream_VOD, 
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0))
  ) %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


mean_genre_m <- PC18_m_film_survey %>%
  tbl_svysummary(
    include = c("C2601", "C2602", "C2603", "C2604", "C2605", "C2606", "C2607", 
                "C2608", "C2609", "C2610", "C2611", "C2612", "C2613", "C2614", "C2615", "C2616"), 
    label = list(C2601 ~ "Comedy",
                 C2602 ~ "Action",
                 C2603 ~ "Historical",
                 C2604 ~ "Thrillers",
                 C2605 ~ "Adventure",
                 C2606 ~ "Drama",
                 C2607 ~ "Animated",
                 C2608 ~ "Horror",
                 C2609 ~ "Film d'auteur",
                 C2610 ~ "Documentary",
                 C2611 ~ "Western",
                 C2612 ~ "Erotic",
                 C2613 ~ "Science fiction",
                 C2614 ~ "Romantic comedy",
                 C2615 ~ "Musicals ",
                 C2616 ~ "Political"),
    by = film_stream_VOD, 
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0))
  ) %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

t_comp_genre_film_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")) %>% 
  modify_column_hide(columns = stat_2_1) %>% 
  as_gt()

t_comp_genre_film_m


## TV series ------

###Diversity

mean_genre_unm <- PC18_to_m_serie_survey %>%
  tbl_svysummary(
    include = c("serie_stream_VOD", "nbr_genre_serie", "nbr_genre_serie_aime", "nbr_genre_serie_deteste", "serie_nonFR"), 
    label = list(nbr_genre_serie ~ "Styles sériels regardés",
                 nbr_genre_serie_aime ~ "Styles sériels particulièrement aimés",
                 nbr_genre_serie_deteste ~ "Styles sériels detestés"),
    by = serie_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Nombre de :**",
                update = list(stat_1 ~ gt::html("**Non streameur**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**Streameur**<br> N = {n_unweighted}")))

mean_genre_m <- PC18_m_serie_survey %>%
  tbl_svysummary(
    include = c("serie_stream_VOD", "nbr_genre_serie", "nbr_genre_serie_aime", "nbr_genre_serie_deteste", "serie_nonFR"), 
    label = list(nbr_genre_serie ~ "Styles sériels regardés",
                 nbr_genre_serie_aime ~ "Styles sériels particulièrement aimés",
                 nbr_genre_serie_deteste ~ "Styles sériels detestés"),
    by = serie_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Nombre de :**",
                update = list(stat_1 ~ gt::html("**Non streameur**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**Streameur**<br> N = {n_unweighted}")))

t_comp_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Echantillon non matché**", "**Echantillon matché**")
) %>% as_gt() %>%
  tab_footnote(
    footnote = "Moyenne et écart type pondérés.",
    locations = cells_body(columns = stat_1_1,
                           rows = 1)
  )

t_comp_m

### Specific genre watched



mean_genre_unm <- PC18_to_m_serie_survey %>%
  tbl_svysummary(
    include = c("C3801", "C3802", "C3803", "C3804", "C3805", "C3806", "C3807", 
                "C3808", "C3809", "C3810", "C3811", "C3812", "C3813", "C3814", "C3815", "C3816"), 
    label = list(C3801 ~ "Comedy",
                 C3802 ~ "Action",
                 C3803 ~ "Historical",
                 C3804 ~ "Thrillers",
                 C3805 ~ "Adventure",
                 C3806 ~ "Drama",
                 C3807 ~ "Animated",
                 C3808 ~ "Horror",
                 C3809 ~ "Serie d'auteur",
                 C3810 ~ "Documentary",
                 C3811 ~ "Western",
                 C3812 ~ "Erotique",
                 C3813 ~ "Science fiction",
                 C3814 ~ "Romantic comedy",
                 C3815 ~ "Musical",
                 C3816 ~ "Political"),
    by = serie_stream_VOD, 
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0))
  ) %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


mean_genre_m <- PC18_m_serie_survey %>%
  tbl_svysummary(
    include = c("C3801", "C3802", "C3803", "C3804", "C3805", "C3806", "C3807", 
                "C3808", "C3809", "C3810", "C3811", "C3812", "C3813", "C3814", "C3815", "C3816"), 
    label = list(C3801 ~ "Comedy",
                 C3802 ~ "Action",
                 C3803 ~ "Historical",
                 C3804 ~ "Thrillers",
                 C3805 ~ "Adventure",
                 C3806 ~ "Drama",
                 C3807 ~ "Animated",
                 C3808 ~ "Horror",
                 C3809 ~ "Serie d'auteur",
                 C3810 ~ "Documentary",
                 C3811 ~ "Western",
                 C3812 ~ "Erotique",
                 C3813 ~ "Science fiction",
                 C3814 ~ "Romantic comedy",
                 C3815 ~ "Musical",
                 C3816 ~ "Political"),
    by = serie_stream_VOD, 
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0))
  )  %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

t_comp_genre_show_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")) %>% 
  modify_column_hide(columns = stat_2_1) 

t_comp_genre_show_m_gt <- t_comp_genre_show_m %>% 
  as_gt()

t_comp_genre_show_m_gt


## All three ------
## Graphe for music, film & show


diff_unm <- PC18_to_m_music %>% summarize_at(
  .vars = vars(c(nbr_genre_music, nbr_genre_aime, nbr_genre_deteste)),
  .funs = list(smd = ~ smd(., g = stream_spe, w = POND, gref = 2, std.error = T)$estimate,
               std.error = ~ smd(., g = stream_spe, w = POND, gref = 2, std.error = T)$std.error)) %>% 
  bind_cols(PC18_to_m_film %>% summarize_at(
    .vars = vars(c(nbr_genre_film, nbr_genre_film_aime, nbr_genre_film_deteste)),
    .funs = list(smd = ~ smd(., g = film_stream_VOD, w = POND, gref = 2, std.error = T)$estimate,
                 std.error = ~ smd(., g = film_stream_VOD, w = POND, gref = 2, std.error = T)$std.error))
    
  ) %>% 
  bind_cols(PC18_to_m_serie %>% summarize_at(
    .vars = vars(c(nbr_genre_serie, nbr_genre_serie_aime, nbr_genre_serie_deteste)),
    .funs = list(smd = ~ smd(., g = serie_stream_VOD, w = POND, gref = 2, std.error = T)$estimate,
                 std.error = ~ smd(., g = serie_stream_VOD, w = POND, gref = 2, std.error = T)$std.error))
  ) %>%   
  mutate(sample = "unmatched")

diff_m <- PC18_m_music %>% summarize_at(
  .vars = vars(c(nbr_genre_music, nbr_genre_aime, nbr_genre_deteste)),
  .funs = list(smd = ~ smd(., g = stream_spe, w = POND_m, gref = 2, std.error = T)$estimate,
               std.error = ~ smd(., g = stream_spe, w = POND_m, gref = 2, std.error = T)$std.error)) %>% 
  bind_cols(PC18_m_film %>% summarize_at(
    .vars = vars(c(nbr_genre_film, nbr_genre_film_aime, nbr_genre_film_deteste)),
    .funs = list(smd = ~ smd(., g = film_stream_VOD, w = POND_m, gref = 2, std.error = T)$estimate,
                 std.error = ~ smd(., g = film_stream_VOD, w = POND_m, gref = 2, std.error = T)$std.error))
    
  ) %>% 
  bind_cols(PC18_m_serie %>% summarize_at(
    .vars = vars(c(nbr_genre_serie, nbr_genre_serie_aime, nbr_genre_serie_deteste)),
    .funs = list(smd = ~ smd(., g = serie_stream_VOD, w = POND_m, gref = 2, std.error = T)$estimate,
                 std.error = ~ smd(., g = serie_stream_VOD, w = POND_m, gref = 2, std.error = T)$std.error))
  ) %>% 
  mutate(sample = "matched")


result_to_plot <- bind_rows(diff_unm, diff_m) %>% 
  pivot_longer(-sample) %>%
  mutate(ind = str_extract(name, "(smd|std.error)"),name = str_remove(name, "_(smd|std.error)")) %>%
  pivot_wider(names_from = ind, values_from = value) 

## Detailled means 

plot <- result_to_plot %>% 
  mutate(cat = recode_factor(name, 
                             "nbr_genre_music" = "Music",
                             "nbr_genre_aime" = "Music",
                             "nbr_genre_deteste" = "Music",
                             "nbr_genre_film" = "Movies",
                             "nbr_genre_film_aime" = "Movies",
                             "nbr_genre_film_deteste" = "Movies",
                             "nbr_genre_serie" = "TV series",
                             "nbr_genre_serie_aime" = "TV series",
                             "nbr_genre_serie_deteste" = "TV series"),
         name = recode_factor(name, 
                              "nbr_genre_music" = "Number of genres consumed",
                              "nbr_genre_aime" = "Number of genres liked",
                              "nbr_genre_deteste" = "Number of genres hated",
                              "nbr_genre_film" = "Number of genres consumed",
                              "nbr_genre_film_aime" = "Number of genres liked",
                              "nbr_genre_film_deteste" = "Number of genres hated",
                              "nbr_genre_serie" = "Number of genres consumed",
                              "nbr_genre_serie_aime" = "Number of genres liked",
                              "nbr_genre_serie_deteste" = "Number of genres hated"),
         name = factor(name, c("Number of genres hated", "Number of genres liked", "Number of genres consumed")),
         sample = recode_factor(sample,
                                "matched" = "Matched",
                                "unmatched" = "All population")) %>% 
  ggplot(aes(x = name, y = smd, color = sample)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = smd - 1.96*std.error, 
                    ymax = smd + 1.96*std.error),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  facet_grid(cat ~ .) +
  xlab("") +
  ylab("Standardized mean difference") +
  labs(color="Sample") +
  guides(color = guide_legend(reverse = T)) + 
  scale_color_manual(values=c("#1a9641", "#d7191c")) +
  theme_bw()

plot

ggsave(filename = here("output", "Figure_A11_Without_Digital_Use_SMD_genre_detailed.png"),
       device = "png",
       width = 22,
       height = 23,
       units = "cm")

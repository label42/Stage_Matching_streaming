#Recodage

####################
#### Socio démo ####
####################


## Recodage de d$VITENCOUPLE en d$VITENCOUPLE_r
d$VITENCOUPLE_r <- fct_recode(d$VITENCOUPLE,
                              "Oui, vit dans logement" = "1",
                              "Oui, vit pas dans logement" = "2",
                              "Non" = "3",
                              NULL = "4",
                              NULL = "421,421",
                              NULL = "5",
                              NULL = ""
)

## Fréquence d'écoute de musique
d$music_12m <- as.character(d$E7)
d$music_12m <- fct_recode(d$music_12m,
                          "Tous les jours" = "1",
                          "Une à plusieurs fois/semaine" = "2",
                          "Une à plusieurs fois/semaine" = "3",
                          "Rarement" = "4",
                          "Rarement" = "5",
                          "Jamais" = "6",
                          NULL = "7",
                          NULL = "8"
)



## Recodage de d$E83 en d$stream_spe
d$stream_spe <- d$E83

#Variable synthétique nbr styles écoutés

list <- c("E1001", "E1002", "E1003", "E1004", "E1005", "E1006", "E1007", 
          "E1008", "E1009", "E1010", "E1011", "E1012", "E1013")

d$nbr_genre_music <- rowSums(d[,list])
d$nbr_genre_music[is.na(d$nbr_genre_music)] <- 0

# Variable synthétique "est partie a l'étranger pour vacances au cours des 12 derniers mois
d <- d %>% mutate(    
  voyage_etranger = case_when(
    A71 == 1 ~ 1,
    A72 == 1 ~ 1,
    A73 == 1 ~ 0,
    A5 == 2 ~ 0
  ))

## Recodage de d$A8 en d$sorties_ami
d$sorties_ami <- as.character(d$A8)
d$sorties_ami <- fct_recode(d$sorties_ami,
                            "Plusieurs fois/semaine" = "1",
                            "Plusieurs fois/mois" = "2",
                            "Plusieurs fois/mois" = "3",
                            "Une fois par mois" = "4",
                            "Rarement" = "5",
                            "Jamais" = "6",
                            NULL = "7",
                            NULL = "8"
)

## Recodage de d$D2 en d$D2_rec
d$freq_info <- as.character(d$D2)
d$freq_info <- fct_recode(d$freq_info,
                          "Tous les jours" = "1",
                          "Une ou plusieurs/semaines" = "2",
                          "Une ou plusieurs/semaines" = "3",
                          "Plus rarement" = "4",
                          "Plus rarement" = "5",
                          NULL = "6"
)

## Recodage de d$F5 en d$autojuge_lecture
d$autojuge_lecture <- as.character(d$F5)
d$autojuge_lecture <- fct_recode(d$autojuge_lecture,
                                 "Beaucoup" = "1",
                                 "Moyennement" = "2",
                                 "Peu" = "3",
                                 "Pas" = "4",
                                 NULL = "5",
                                 NULL = "6"
)

d$freq_lecture <- as.character(d$F11)
d$freq_lecture <- fct_explicit_na(d$freq_lecture, na_level = "Ne lit pas")
d$freq_lecture <- fct_recode(d$freq_lecture,
                             "Tous les jours" = "1",
                             "1/semaine au moins" = "2",
                             "1/mois au mois" = "3",
                             "Plus rarement" = "4",
                             "Plus rarement" = "5",
                             NULL = "6",
                             NULL = "7"
)

## Recodage de d$STOC en d$logement
d$logement <- as.character(d$STOC)
d$logement <- fct_recode(d$logement,
  "Propriétaire (crédit en cours)" = "1",
  "Propriétaire (sans crédit)" = "2",
  "Usufruitier" = "3",
  "Locataire" = "4",
  "A titre gratuit" = "5"
)


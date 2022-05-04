#Recodage

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

## Recodage de d$VITENCOUPLE en d$VITENCOUPLE_r
d$VITENCOUPLE_r <- fct_recode(d$VITENCOUPLE,
  "Oui, vit dans logement" = "1",
  "Oui, vit pas dans logement" = "2",
  "Non" = "3",
  NULL = "4",
  NULL = "421,421",
  NULL = "5"
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

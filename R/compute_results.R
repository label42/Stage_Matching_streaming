# Packages ------
library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(cobalt)
library(survey)
library(smd)
library(ggthemes)
library(EValue)

# Load data ------

load(here("data", "PC18.RData"))
load(here("data", "film_matched.RData"))
load(here("data", "music_matched.RData"))
load(here("data", "serie_matched.RData"))

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

# Table 1: size of treated, non-treated, and control groups ------

tmp_music <- bind_rows(PC18_to_m_music %>% select(stream_spe) %>% 
                         mutate(matching_status = 0),
                       PC18_m_music %>% select(stream_spe) %>% 
                         mutate(matching_status = 1)) %>% 
  mutate(medium = "Music") %>% 
  rename("stream" = "stream_spe")

tmp_movie <- bind_rows(PC18_to_m_film %>% select(film_stream_VOD) %>% 
                         mutate(matching_status = 0),
                       PC18_m_film %>% select(film_stream_VOD) %>% 
                         mutate(matching_status = 1)) %>% 
  mutate(medium = "Movie") %>% 
  rename("stream" = "film_stream_VOD")

tmp_series <- bind_rows(PC18_to_m_serie %>% select(serie_stream_VOD) %>% 
                          mutate(matching_status = 0),
                        PC18_m_serie %>% select(serie_stream_VOD) %>% 
                          mutate(matching_status = 1)) %>% 
  mutate(medium = "Series") %>% 
  rename("stream" = "serie_stream_VOD")

overall_summary <- bind_rows(tmp_music, tmp_movie, tmp_series) %>% 
  group_by(matching_status, medium, stream) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  filter(!(matching_status == 1 & stream == 1)) %>% 
  arrange(desc(n)) %>% 
  pivot_wider(names_from = c(matching_status, stream),
              values_from = n) %>% 
  relocate("0_0", .after = "0_1") %>% 
  rename("Domain" = "medium",
         "Size of treated group" = "0_1",
         "Size of non-treated group" = "0_0",
         "Size of control group selected by matching procedure" = "1_0") %>% 
  gt()

overall_summary

gtsave(overall_summary, 
       filename = here("output", "Table 1 Size of treated untreated control.docx"))


# Effects of streaming on tastes: Figure 1 ------

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

# gtsave(t_comp_genre_ecoute_m,
#        filename = "Proportion dif genres music.docx",
#        path = "output")


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

# gtsave(t_comp_genre_film_m,
#        filename = "Proportion dif genres film.docx",
#        path = "output") 



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

# gtsave(t_comp_genre_show_m_gt,
#        filename = "Proportion dif genres show.docx",
#        path = "output")


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


plot <- result_to_plot %>% 
  filter(!grepl('aime|deteste', name)) %>% 
  mutate(name = recode_factor(name, 
                              "nbr_genre_music" = "Music genre",
                              "nbr_genre_film" = "Film genre",
                              "nbr_genre_serie" = "Series genre"),
         name = factor(name, c("Series genre", "Film genre", "Music genre")),
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
  xlab("") +
  ylab("Standardized mean difference") +
  labs(color="Sample") +
  coord_flip() +
  guides(color = guide_legend(reverse = T)) + 
  scale_color_manual(values=c("#1a9641", "#d7191c")) +
  theme_bw()

plot


ggsave(filename = "nbr genre consumed.png",
       path = "output/presentations",
       device = "png",
       width = 22,
       height = 10,
       units = "cm")

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


ggsave(filename = here("output", "Figure 1 SMD genre detailed.png"),
       device = "png",
       width = 22,
       height = 23,
       units = "cm")

## Sensitivity analysis ------
result_to_plot %>% 
  filter(sample == "matched") %>% 
  rowwise() %>% 
  mutate(e_value_m = evalues.MD(smd, se = std.error, true = 0)[2, 1],
         e_value_l = evalues.MD(smd, se = std.error, true = 0)[2, 2],
         e_value_u = evalues.MD(smd, se = std.error, true = 0)[2, 3]) %>% 
  gt() %>% 
  gtsave(here("output", "Table_sensitivity.tex"))

# Foreign language: Figure 2 -------

## Music

foreign_lang_music_unm <- PC18_to_m_music_survey %>%
  tbl_svysummary(
    include = c(music_nonFR), 
    label = list(music_nonFR ~ "Music"),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


foreign_lang_music_m <- PC18_m_music_survey %>%
  tbl_svysummary(
    include = c(music_nonFR), 
    label = list(music_nonFR ~ "Music"),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


music_lang <- tbl_merge(
  tbls = list(
    foreign_lang_music_unm, 
    foreign_lang_music_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")) 

music_lang

d_music_lang <- music_lang$table_body %>% 
  select(label, stat_1_1, stat_1_2, stat_2_2) %>% 
  rename(non_user_unm__v = stat_1_1,
         non_user_m__v = stat_1_2,
         user__v = stat_2_2,
  ) %>%  
  mutate_all(~gsub("%,", "/", .)) %>% 
  mutate_all(~gsub("%", "", .)) %>% 
  mutate_all(~gsub(",", ".", .)) %>% 
  mutate(across(!label, as.numeric)) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("group", "valeur"), 
    names_pattern = "(.*)__(.*)"
  ) %>% 
  pivot_wider(values_from = value,
              names_from = valeur) %>% 
  mutate(
    n = case_when(group == "non_user_unm" ~ 5839,
                  group == "non_user_m" ~ 1368,
                  group == "user" ~ 1922),
    p = v / 100,
    ci_up = (p + 1.96 * sqrt((p * (1 - p)) / n)) * 100,
    ci_down = (p - 1.96 * sqrt((p * (1 - p)) / n)) * 100)


d_music_lang_diff <- d_music_lang %>% 
  select(-v, -ci_up,-ci_down) %>% 
  pivot_wider(names_from = "group",
              values_from = c(n,p)) %>% 
  mutate(diff_unm = p_user - p_non_user_unm,
         diff_m = p_user - p_non_user_m,
         ci_unm = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                                (p_non_user_unm * (1 - p_non_user_unm)) / n_non_user_unm),
         ci_m = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                              (p_non_user_m * (1 - p_non_user_m)) / n_non_user_m)
  ) %>% 
  select(label, contains("diff"), contains("ci")) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("stat", "group"), 
    names_pattern = "(.*)_(.*)"
  ) %>% 
  pivot_wider(names_from = "stat",
              values_from = "value")



## Film

foreign_lang_film_unm <- PC18_to_m_film_survey %>%
  tbl_svysummary(
    include = c(film_nonFR), 
    label = list(film_nonFR ~ "Movie"),
    by = film_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


foreign_lang_film_m <- PC18_m_film_survey %>%
  tbl_svysummary(
    include = c(film_nonFR), 
    label = list(film_nonFR ~ "Movie"),
    by = film_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


film_lang <- tbl_merge(
  tbls = list(
    foreign_lang_film_unm, 
    foreign_lang_film_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")) 

film_lang

d_film_lang <- film_lang$table_body %>% 
  select(label, stat_1_1, stat_1_2, stat_2_2) %>% 
  rename(non_user_unm__v = stat_1_1,
         non_user_m__v = stat_1_2,
         user__v = stat_2_2,
  ) %>%  
  mutate_all(~gsub("%,", "/", .)) %>% 
  mutate_all(~gsub("%", "", .)) %>% 
  mutate_all(~gsub(",", ".", .)) %>% 
  mutate(across(!label, as.numeric)) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("group", "valeur"), 
    names_pattern = "(.*)__(.*)"
  ) %>% 
  pivot_wider(values_from = value,
              names_from = valeur) %>% 
  mutate(
    n = case_when(group == "non_user_unm" ~ 4844,
                  group == "non_user_m" ~ 1350,
                  group == "user" ~ 1925),
    p = v / 100,
    ci_up = (p + 1.96 * sqrt((p * (1 - p)) / n)) * 100,
    ci_down = (p - 1.96 * sqrt((p * (1 - p)) / n)) * 100)


d_film_lang_diff <- d_film_lang %>% 
  select(-v, -ci_up,-ci_down) %>% 
  pivot_wider(names_from = "group",
              values_from = c(n,p)) %>% 
  mutate(diff_unm = p_user - p_non_user_unm,
         diff_m = p_user - p_non_user_m,
         ci_unm = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                                (p_non_user_unm * (1 - p_non_user_unm)) / n_non_user_unm),
         ci_m = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                              (p_non_user_m * (1 - p_non_user_m)) / n_non_user_m)
  ) %>% 
  select(label, contains("diff"), contains("ci")) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("stat", "group"), 
    names_pattern = "(.*)_(.*)"
  ) %>% 
  pivot_wider(names_from = "stat",
              values_from = "value")


## TV Series

foreign_lang_show_unm <- PC18_to_m_serie_survey %>%
  tbl_svysummary(
    include = c(serie_nonFR), 
    label = list(serie_nonFR ~ "Series"),
    by = serie_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


foreign_lang_show_m <- PC18_m_serie_survey %>%
  tbl_svysummary(
    include = c(serie_nonFR), 
    label = list(serie_nonFR ~ "Series"),
    by = serie_stream_VOD,
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


show_lang <- tbl_merge(
  tbls = list(
    foreign_lang_show_unm, 
    foreign_lang_show_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")) 

show_lang

d_show_lang <- show_lang$table_body %>% 
  select(label, stat_1_1, stat_1_2, stat_2_2) %>% 
  rename(non_user_unm__v = stat_1_1,
         non_user_m__v = stat_1_2,
         user__v = stat_2_2,
  ) %>%  
  mutate_all(~gsub("%,", "/", .)) %>% 
  mutate_all(~gsub("%", "", .)) %>% 
  mutate_all(~gsub(",", ".", .)) %>% 
  mutate(across(!label, as.numeric)) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("group", "valeur"), 
    names_pattern = "(.*)__(.*)"
  ) %>% 
  pivot_wider(values_from = value,
              names_from = valeur) %>% 
  mutate(
    n = case_when(group == "non_user_unm" ~ 4122,
                  group == "non_user_m" ~ 1024,
                  group == "user" ~ 1463),
    p = v / 100,
    ci_up = (p + 1.96 * sqrt((p * (1 - p)) / n)) * 100,
    ci_down = (p - 1.96 * sqrt((p * (1 - p)) / n)) * 100)


d_show_lang_diff <- d_show_lang %>% 
  select(-v, -ci_up,-ci_down) %>% 
  pivot_wider(names_from = "group",
              values_from = c(n,p)) %>% 
  mutate(diff_unm = p_user - p_non_user_unm,
         diff_m = p_user - p_non_user_m,
         ci_unm = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                                (p_non_user_unm * (1 - p_non_user_unm)) / n_non_user_unm),
         ci_m = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                              (p_non_user_m * (1 - p_non_user_m)) / n_non_user_m)
  ) %>% 
  select(label, contains("diff"), contains("ci")) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("stat", "group"), 
    names_pattern = "(.*)_(.*)"
  ) %>% 
  pivot_wider(names_from = "stat",
              values_from = "value")

## Graph joining the three

d_three_diff <- dplyr::union(d_music_lang_diff %>% mutate(medium = "Music"),
                             d_film_lang_diff %>% mutate(medium = "Movie")) %>% 
  dplyr::union(d_show_lang_diff %>% mutate(medium = "Series")) %>% 
  mutate(medium = factor(medium, c("Series", "Movie", "Music"), labels = c("Series", "Movie", "Music")),
         group = factor(group, c("m", "unm"), labels = c("Matched", "Unmatched"))) 

plot <- d_three_diff %>% 
  ggplot(aes(x = medium, y = diff, color = group)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = diff - ci, 
                    ymax = diff + ci),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  guides(color = guide_legend(reverse = T)) +
  coord_flip() +
  xlab("") +
  ylab("Difference in proportion") +
  labs(color="Sample") +
  scale_color_manual(values=c("#1a9641", "#d7191c")) +
  theme_bw()

plot

ggsave(filename = "Figure 2 Diff in foreign language.png",
       path = "output",
       device = "png",
       width = 18,
       height = 10,
       units = "cm")


# Table A2: Demographics of streaming users ------

theme_gtsummary_language(language = "en", decimal.mark = ",", big.mark = " ")

PC18_survey <- survey::svydesign(id = ~IDENT18, data = PC18, weights = PC18$POND)

## Music streaming

### soc-dem char X streaming music
t_stream_music_socdem <- PC18_survey %>%
  subset(!is.na(stream_spe)) %>%
  tbl_svysummary(
    include = c("stream_spe", "AGE", "SEXE_femme", "DIPLOME_eng"), #L'ensemble des variables que l'on veut dans le tableau
    label = list(AGE ~ "Age mean (sd)",
                 SEXE_femme ~ "% women",
                 DIPLOME_eng ~ "Education level"),
    by = stream_spe,
    missing = "ifany",
    missing_text = "N missing",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 0)
  ) %>%
  add_overall(last = T
  ) %>% 
  bold_labels() %>%
  modify_header(update = list(
    all_stat_cols() ~ "**{level}**, N = {n_unweighted}",
    stat_1 ~ gt::html("**Non user**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"),
    stat_2 ~ gt::html("**Streaming user**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"),
    stat_0 ~ gt::html("**All music consumers**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"))) %>%
  modify_footnote(update = everything() ~ NA)

t_stream_music_socdem

## Film

### soc-dem X watching film on streaming plateforme


t_stream_film_socdem <- PC18_survey %>%
  subset(freq_film != "Jamais") %>%
  tbl_svysummary(
    include = c("film_stream_VOD", "AGE", "SEXE_femme", "DIPLOME_eng"), #L'ensemble des variables que l'on veut dans le tableau
    label = list(AGE ~ "Age mean (sd)",
                 SEXE_femme ~ "% women",
                 DIPLOME_eng ~ "Education level"),
    by = film_stream_VOD,
    missing = "ifany",
    missing_text = "N missing",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 0)
  ) %>%
  add_overall(last = T
  ) %>% 
  bold_labels() %>%
  modify_header(update = list(
    all_stat_cols() ~ "**{level}**, N = {n_unweighted}",
    stat_1 ~ gt::html("**Non user**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"),
    stat_2 ~ gt::html("**Streaming user**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"),
    stat_0 ~ gt::html("**All movies consumers**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"))) %>%
  modify_footnote(update = everything() ~ NA) 

t_stream_film_socdem

## Show

### soc-dem X watching show on streaming plateformes

t_stream_serie_socdem <- PC18_survey %>%
  subset(freq_serie != "Jamais") %>%
  tbl_svysummary(
    include = c("serie_stream_VOD", "AGE", "SEXE_femme", "DIPLOME_eng"), #L'ensemble des variables que l'on veut dans le tableau
    label = list(AGE ~ "Age mean (sd)",
                 SEXE_femme ~ "% women",
                 DIPLOME_eng ~ "Education level"),
    by = serie_stream_VOD,
    missing = "ifany",
    missing_text = "N missing",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 0)
  ) %>%
  add_overall(last = T
  ) %>% 
  bold_labels() %>%
  modify_header(update = list(
    all_stat_cols() ~ "**{level}**, N = {n_unweighted}",
    stat_1 ~ gt::html("**Non user**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"),
    stat_2 ~ gt::html("**Streaming user**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"),
    stat_0 ~ gt::html("**All series consumers**<br> N = {n_unweighted} <br> {round(p_unweighted * 100, 0)} %"))) %>%
  modify_footnote(update = everything() ~ NA) 

t_stream_serie_socdem


## The three together

tbl_all_stream_socdem <-
  tbl_merge(
    tbls = list(t_stream_music_socdem, t_stream_film_socdem, t_stream_serie_socdem),
    tab_spanner = c("**Music**", "**Movies**", "**Series**")
  ) %>%
  as_gt()

gtsave(tbl_all_stream_socdem, filename = here("output", "Table A2 Descriptive statistics.docx"))


# Figure A1-A6: Evaluate quality of matching ------

## Music ------

tab_eval_music <- bal.tab(res_match_template_stream_music, binary = "std", thresholds = c(m = 0.05))

tab_eval_music

allvar_mod <- c("SEXE_r_Homme",
                "AGE_5_r_15 - 20",
                "AGE_5_r_21 - 25",
                "AGE_5_r_26 - 30",
                "AGE_5_r_31 - 35",
                "AGE_5_r_36 - 40",
                "AGE_5_r_41 - 45",
                "AGE_5_r_46 - 50",
                "AGE_5_r_51 - 55",
                "AGE_5_r_56 - 60",
                "AGE_5_r_61 - 65",
                "AGE_5_r_66 - 70",
                "AGE_5_r_71 - 75",
                "AGE_5_r_76 - 80",
                "AGE_5_r_81+",
                "CRITREVENU_r_6000 ou plus",
                "CRITREVENU_r_De 1000 à 1199",
                "CRITREVENU_r_De 1200 à 1499",
                "CRITREVENU_r_De 1500 à 1999",
                "CRITREVENU_r_De 2000 à 2499",
                "CRITREVENU_r_De 2500 à 2999",
                "CRITREVENU_r_De 3000 à 3999",
                "CRITREVENU_r_De 4000 à 5999",
                "CRITREVENU_r_De 800 à 999",
                "CRITREVENU_r_Moins de 800",
                "CRITREVENU_r_Manquant",
                "PCS_MENAGE_Dominante cadre",
                "PCS_MENAGE_Dominante intermédiaire",
                "PCS_MENAGE_Dominante indépendante",
                "PCS_MENAGE_Dominante employée",
                "PCS_MENAGE_Dominante ouvrière",
                "PCS_MENAGE_Un employé ou ouvrier",
                "PCS_MENAGE_Inactifs (hors retraités)",
                "h_travail_semaine",
                "DIPLOME_r_Inf. Bac",
                "DIPLOME_r_Bac",
                "DIPLOME_r_Bac +2/3",
                "DIPLOME_r_Bac +5",
                "naiss_parents_Deux parents nés à l'étranger",
                "naiss_parents_Deux parents nés en France",
                "naiss_parents_Un des deux parents né à l'étranger",
                "naiss_parents_Manquant",
                "DIPLOME_pere_Bac",
                "DIPLOME_pere_Inf. Bac",
                "DIPLOME_pere_Sup. Bac",
                "DIPLOME_pere_Manquant",
                "CS_pere_Agriculteur",
                "CS_pere_Artisant/commerçant",
                "CS_pere_Autres inactifs",
                "CS_pere_Cadre et prof. intel.",
                "CS_pere_Employé",
                "CS_pere_Ouvrier",
                "CS_pere_Prof. inter.",
                "DIPLOME_mere_Bac",
                "DIPLOME_mere_Inf. Bac",
                "DIPLOME_mere_Sup. Bac",
                "DIPLOME_mere_Manquant",
                "CS_mere_Agriculteur",
                "CS_mere_Artisant/commerçant",
                "CS_mere_Autres inactifs",
                "CS_mere_Cadre et prof. intel.",
                "CS_mere_Employé",
                "CS_mere_Ouvrier",
                "CS_mere_Prof. inter.",
                "sorties_ami_Plusieurs fois/semaine",
                "sorties_ami_Plusieurs fois/mois",
                "sorties_ami_Une fois par mois",
                "sorties_ami_Rarement",
                "sorties_ami_Jamais",
                "VITENCOUPLE_r_Oui, vit dans logement",
                "VITENCOUPLE_r_Oui, vit pas dans logement",
                "VITENCOUPLE_r_Non",
                "logement_Propriétaire (crédit en cours)",
                "logement_Propriétaire (sans crédit)",
                "logement_Usufruitier",
                "logement_Locataire",
                "logement_A titre gratuit",
                "freq_jv_Tous les jours",
                "freq_jv_Une à plusieurs fois/semaine",
                "freq_jv_1 à 3 fois/mois",
                "freq_jv_Plus rarement",
                "freq_jv_Ne joue pas aux JV",
                "freq_tv_Tous les jours",
                "freq_tv_1 à 4 jours/semaine",
                "freq_tv_Plus rarement",
                "freq_tv_Jamais",
                "equip_tv_Mixte",
                "equip_tv_Uniquement écran de TV",
                "equip_tv_Uniquement numérique",
                "clip_tv",
                "equip_film_Mixte",
                "equip_film_Ne regarde pas de film",
                "equip_film_Uniquement écran de TV",
                "equip_film_Uniquement numérique",
                "film_stream_VOD",
                "film_stream_autre",
                "film_DVD",
                "film_num",
                "nbr_genre_film",
                "freq_serie_Tous les jours",
                "freq_serie_Une fois/semaine mini",
                "freq_serie_Une fois/mois mini",
                "freq_serie_Plus rarement",
                "freq_serie_Jamais",
                "equip_serie_Mixte",
                "equip_serie_Uniquement écran de TV",
                "equip_serie_Uniquement numérique",
                "serie_stream_VOD",
                "serie_stream_autre",
                "nbr_genre_serie",
                "info_internet_Ne s'informe pas en ligne",
                "info_internet_Ne suis pas l'actu",
                "info_internet_S'informe en ligne",
                "freq_lecture_Tous les jours",
                "freq_lecture_1/semaine au moins",
                "freq_lecture_1/mois au mois",
                "freq_lecture_Plus rarement",
                "freq_lecture_Ne lit pas",
                "equip_lecture_Mixte",
                "equip_lecture_Ne lit pas de livre",
                "equip_lecture_Uniquement livre papier",
                "equip_lecture_Uniquement numérique ou autre",
                "nbr_genre_film_cine",
                "musee_art_12m",
                "galerie_12m",
                "acces_internet",
                "ordi",
                "freq_internet_Tous les jours",
                "freq_internet_Plusieurs fois/semaine",
                "freq_internet_Plus rarement",
                "freq_internet_Jamais",
                "reseaux_sociaux",
                "culture_en_ligne",
                "musique_enfance_Souvent",
                "musique_enfance_De temps en temps",
                "musique_enfance_Rarement",
                "musique_enfance_Jamais",
                "cinema_enfance_Souvent",
                "cinema_enfance_De temps en temps",
                "cinema_enfance_Rarement",
                "cinema_enfance_Jamais",
                "nbr_genre_parent_ecoute",
                "nbr_genre_ecoute_enfance",
                "audivisuel_nonFR",
                "autre_langue")

love_part1 <- love.plot(res_match_template_stream_music, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Unmatched", "Matched"), 
                        title = NULL) +
  xlim(c(0,1)) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[1:70])) + 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample") +
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#d7191c", "#1a9641")) +
  theme_bw()

love_part1

ggsave(here("output", "Figure A2 music_love_part1.png"), width = 21, height = 29.7, units = "cm", dpi = 300,
       bg = "white", device = "png")



love_part2 <- love.plot(res_match_template_stream_music, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Unmatched", "Matched"),
                        title = NULL) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[71:146]))+ 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample")+
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#d7191c", "#1a9641")) +
  theme_bw()

love_part2

ggsave(here("output", "Figure A2 music_love_part2.png"), width = 21, height = 29.7, units = "cm", dpi = 300,
       bg = "white", device = "png")


### Age variable

bind_rows(
  PC18_to_m_music %>% select(AGE, stream_spe, POND) %>% 
    mutate(matching_status = "Before matching"),
  PC18_m_music %>% select(AGE, stream_spe, POND) %>% 
    mutate(matching_status = "After matching")) %>% 
  mutate(matching_status = factor(matching_status, c("Before matching", "After matching"))) %>% 
  ggplot() + 
  aes(x = AGE, group=stream_spe, fill = as.factor(stream_spe), colour = as.factor(stream_spe),  weight = POND) +
  scale_color_manual(values=c("#d7191c", "#1a9641"),
                     labels = c("0" = "Not treated",
                                "1" ="Treated"), name = "Group") +
  scale_fill_manual(values=c("#d7191c", "#1a9641"), guide="none") +
  guides(color=guide_legend(override.aes=list(fill=c("#d7191c","#1a9641")))) +
  geom_density(alpha = 5/10) +
  facet_grid( ~ matching_status) +
  ylab("Density") +
  xlab("Age") +
  theme_bw()

ggsave(here("output", "Figure A1 music_age.png"), width = 18, height = 10, units = "cm", dpi = 300,
       bg = "white", device = "png")


## Film ------

tab_eval_film <- bal.tab(res_match_template_stream_film_VOD, binary = "std", thresholds = c(m = 0.05))

tab_eval_film

allvar_mod <- c("SEXE_r_Homme",
                "AGE_5_r_15 - 20",
                "AGE_5_r_21 - 25",
                "AGE_5_r_26 - 30",
                "AGE_5_r_31 - 35",
                "AGE_5_r_36 - 40",
                "AGE_5_r_41 - 45",
                "AGE_5_r_46 - 50",
                "AGE_5_r_51 - 55",
                "AGE_5_r_56 - 60",
                "AGE_5_r_61 - 65",
                "AGE_5_r_66 - 70",
                "AGE_5_r_71 - 75",
                "AGE_5_r_76 - 80",
                "AGE_5_r_81+",
                "CRITREVENU_r_6000 ou plus",
                "CRITREVENU_r_De 1000 à 1199",
                "CRITREVENU_r_De 1200 à 1499",
                "CRITREVENU_r_De 1500 à 1999",
                "CRITREVENU_r_De 2000 à 2499",
                "CRITREVENU_r_De 2500 à 2999",
                "CRITREVENU_r_De 3000 à 3999",
                "CRITREVENU_r_De 4000 à 5999",
                "CRITREVENU_r_De 800 à 999",
                "CRITREVENU_r_Moins de 800",
                "CRITREVENU_r_Manquant",
                "PCS_MENAGE_Dominante cadre",
                "PCS_MENAGE_Dominante intermédiaire",
                "PCS_MENAGE_Dominante indépendante",
                "PCS_MENAGE_Dominante employée",
                "PCS_MENAGE_Dominante ouvrière",
                "PCS_MENAGE_Un employé ou ouvrier",
                "PCS_MENAGE_Inactifs (hors retraités)",
                "h_travail_semaine",
                "DIPLOME_r_Inf. Bac",
                "DIPLOME_r_Bac",
                "DIPLOME_r_Bac +2/3",
                "DIPLOME_r_Bac +5",
                "naiss_parents_Deux parents nés à l'étranger",
                "naiss_parents_Deux parents nés en France",
                "naiss_parents_Un des deux parents né à l'étranger",
                "naiss_parents_Manquant",
                "DIPLOME_pere_Bac",
                "DIPLOME_pere_Inf. Bac",
                "DIPLOME_pere_Sup. Bac",
                "DIPLOME_pere_Manquant",
                "CS_pere_Agriculteur",
                "CS_pere_Artisant/commerçant",
                "CS_pere_Autres inactifs",
                "CS_pere_Cadre et prof. intel.",
                "CS_pere_Employé",
                "CS_pere_Ouvrier",
                "CS_pere_Prof. inter.",
                "DIPLOME_mere_Bac",
                "DIPLOME_mere_Inf. Bac",
                "DIPLOME_mere_Sup. Bac",
                "DIPLOME_mere_Manquant",
                "CS_mere_Agriculteur",
                "CS_mere_Artisant/commerçant",
                "CS_mere_Autres inactifs",
                "CS_mere_Cadre et prof. intel.",
                "CS_mere_Employé",
                "CS_mere_Ouvrier",
                "CS_mere_Prof. inter.",
                "sorties_ami_Plusieurs fois/semaine",
                "sorties_ami_Plusieurs fois/mois",
                "sorties_ami_Une fois par mois",
                "sorties_ami_Rarement",
                "sorties_ami_Jamais",
                "VITENCOUPLE_r_Oui, vit dans logement",
                "VITENCOUPLE_r_Oui, vit pas dans logement",
                "VITENCOUPLE_r_Non",
                "logement_Propriétaire (crédit en cours)",
                "logement_Propriétaire (sans crédit)",
                "logement_Usufruitier",
                "logement_Locataire",
                "logement_A titre gratuit",
                "freq_jv_Tous les jours",
                "freq_jv_Une à plusieurs fois/semaine",
                "freq_jv_1 à 3 fois/mois",
                "freq_jv_Plus rarement",
                "freq_jv_Ne joue pas aux JV",
                "clip_tv",
                "music_amateur",                                     
                "music_12m_Tous les jours",                           
                "music_12m_Une à plusieurs fois/semaine",             
                "music_12m_Rarement",                                
                "music_ellememe_Tous les jours",                      
                "music_ellememe_De temps en temps",                  
                "music_ellememe_Rarement",                            
                "music_ellememe_Jamais",                              
                "music_manque_Oui, beaucoup",                        
                "music_manque_Oui, un peu",                         
                "music_manque_Non",                                
                "stream_spe",                                         
                "cd_ou_cass",                                         
                "radio",                                             
                "nbr_genre_music",                                 
                "nbr_artiste_ecoute",                               
                "aime_clas",                                         
                "detest_clas",
                "musee_art_12m",
                "galerie_12m",
                "ordi",
                "acces_internet",
                "info_internet_S'informe en ligne",
                "freq_info_Tous les jours",
                "freq_info_Une ou plusieurs/semaines",
                "freq_info_Plus rarement",
                "freq_lecture_Tous les jours",
                "freq_lecture_1/semaine au moins",
                "freq_lecture_1/mois au mois",
                "freq_lecture_Plus rarement",
                "freq_lecture_Ne lit pas",
                "equip_lecture_Mixte",
                "equip_lecture_Ne lit pas de livre",
                "equip_lecture_Uniquement livre papier",
                "equip_lecture_Uniquement numérique ou autre",
                "lecture_nonFR_Oui",
                "lecture_nonFR_Non",
                "lecture_nonFR_Ne lit pas de livre",
                "freq_internet_Tous les jours",
                "freq_internet_Plusieurs fois/semaine",
                "freq_internet_Plus rarement",
                "freq_internet_Jamais",
                "reseaux_sociaux",
                "culture_en_ligne",
                "musique_enfance_Souvent",
                "musique_enfance_De temps en temps",
                "musique_enfance_Rarement",
                "musique_enfance_Jamais",
                "cinema_enfance_Souvent",
                "cinema_enfance_De temps en temps",
                "cinema_enfance_Rarement",
                "cinema_enfance_Jamais",
                "tv_enfance_Souvent",
                "tv_enfance_De temps en temps",
                "tv_enfance_Rarement",
                "tv_enfance_Jamais",
                "nbr_genre_parent_ecoute",
                "nbr_genre_ecoute_enfance",
                "autre_langue")

love_part1 <- love.plot(res_match_template_stream_film_VOD, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Unmatched", "Matched"), 
                        title = NULL) +
  xlim(c(0,1)) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[1:70])) + 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample") +
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#d7191c", "#1a9641")) +
  theme_bw()

love_part1

ggsave(here("output", "Figure A4 film_love_part1.png"), width = 21, height = 29.7, units = "cm", dpi = 300,
       bg = "white", device = "png")



love_part2 <- love.plot(res_match_template_stream_film_VOD, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Unmatched", "Matched"),
                        title = NULL) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[71:143]))+ 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample")+
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#d7191c", "#1a9641")) +
  theme_bw()

love_part2

ggsave(here("output", "Figure A4 film_love_part2.png"), width = 21, height = 29.7, units = "cm", dpi = 300,
       bg = "white", device = "png")



### Age variable

bind_rows(
  PC18_to_m_film %>% select(AGE, film_stream_VOD, POND) %>% 
    mutate(matching_status = "Before matching"),
  PC18_m_film %>% select(AGE, film_stream_VOD, POND) %>% 
    mutate(matching_status = "After matching")) %>% 
  mutate(matching_status = factor(matching_status, c("Before matching", "After matching"))) %>% 
  ggplot() + 
  aes(x = AGE, group=film_stream_VOD, fill = as.factor(film_stream_VOD), colour = as.factor(film_stream_VOD),  weight = POND) +
  scale_color_manual(values=c("#d7191c", "#1a9641"),
                     labels = c("0" = "Not treated",
                                "1" ="Treated"), name = "Group") +
  scale_fill_manual(values=c("#d7191c", "#1a9641"), guide="none") +
  guides(color=guide_legend(override.aes=list(fill=c("#d7191c","#1a9641")))) +
  geom_density(alpha = 5/10) +
  facet_grid( ~ matching_status) +
  ylab("Density") +
  xlab("Age") +
  theme_bw()

ggsave(here("output", "Figure A3 film_age.png"), width = 18, height = 10, units = "cm", dpi = 300,
       bg = "white", device = "png")



## TV Series -------

tab_eval_serie <- bal.tab(res_match_template_stream_serie_VOD, binary = "std", thresholds = c(m = 0.05))

tab_eval_serie

allvar_mod <- c("SEXE_r_Homme",
                "AGE_5_r_15 - 20",
                "AGE_5_r_21 - 25",
                "AGE_5_r_26 - 30",
                "AGE_5_r_31 - 35",
                "AGE_5_r_36 - 40",
                "AGE_5_r_41 - 45",
                "AGE_5_r_46 - 50",
                "AGE_5_r_51 - 55",
                "AGE_5_r_56 - 60",
                "AGE_5_r_61 - 65",
                "AGE_5_r_66 - 70",
                "AGE_5_r_71 - 75",
                "AGE_5_r_76 - 80",
                "AGE_5_r_81+",
                "CRITREVENU_r_6000 ou plus",
                "CRITREVENU_r_De 1000 à 1199",
                "CRITREVENU_r_De 1200 à 1499",
                "CRITREVENU_r_De 1500 à 1999",
                "CRITREVENU_r_De 2000 à 2499",
                "CRITREVENU_r_De 2500 à 2999",
                "CRITREVENU_r_De 3000 à 3999",
                "CRITREVENU_r_De 4000 à 5999",
                "CRITREVENU_r_De 800 à 999",
                "CRITREVENU_r_Moins de 800",
                "CRITREVENU_r_Manquant",
                "PCS_MENAGE_Dominante cadre",
                "PCS_MENAGE_Dominante intermédiaire",
                "PCS_MENAGE_Dominante indépendante",
                "PCS_MENAGE_Dominante employée",
                "PCS_MENAGE_Dominante ouvrière",
                "PCS_MENAGE_Un employé ou ouvrier",
                "PCS_MENAGE_Inactifs (hors retraités)",
                "h_travail_semaine",
                "DIPLOME_r_Inf. Bac",
                "DIPLOME_r_Bac",
                "DIPLOME_r_Bac +2/3",
                "DIPLOME_r_Bac +5",
                "naiss_parents_Deux parents nés à l'étranger",
                "naiss_parents_Deux parents nés en France",
                "naiss_parents_Un des deux parents né à l'étranger",
                "naiss_parents_Manquant",
                "DIPLOME_pere_Bac",
                "DIPLOME_pere_Inf. Bac",
                "DIPLOME_pere_Sup. Bac",
                "DIPLOME_pere_Manquant",
                "CS_pere_Agriculteur",
                "CS_pere_Artisant/commerçant",
                "CS_pere_Autres inactifs",
                "CS_pere_Cadre et prof. intel.",
                "CS_pere_Employé",
                "CS_pere_Ouvrier",
                "CS_pere_Prof. inter.",
                "DIPLOME_mere_Bac",
                "DIPLOME_mere_Inf. Bac",
                "DIPLOME_mere_Sup. Bac",
                "DIPLOME_mere_Manquant",
                "CS_mere_Agriculteur",
                "CS_mere_Artisant/commerçant",
                "CS_mere_Autres inactifs",
                "CS_mere_Cadre et prof. intel.",
                "CS_mere_Employé",
                "CS_mere_Ouvrier",
                "CS_mere_Prof. inter.",
                "sorties_ami_Plusieurs fois/semaine",
                "sorties_ami_Plusieurs fois/mois",
                "sorties_ami_Une fois par mois",
                "sorties_ami_Rarement",
                "sorties_ami_Jamais",
                "VITENCOUPLE_r_Oui, vit dans logement",
                "VITENCOUPLE_r_Oui, vit pas dans logement",
                "VITENCOUPLE_r_Non",
                "logement_Propriétaire (crédit en cours)",
                "logement_Propriétaire (sans crédit)",
                "logement_Usufruitier",
                "logement_Locataire",
                "logement_A titre gratuit",
                "freq_jv_Tous les jours",
                "freq_jv_Une à plusieurs fois/semaine",
                "freq_jv_1 à 3 fois/mois",
                "freq_jv_Plus rarement",
                "freq_jv_Ne joue pas aux JV",
                "clip_tv",
                "music_amateur",                                     
                "music_12m_Tous les jours",                           
                "music_12m_Une à plusieurs fois/semaine",             
                "music_12m_Rarement",                                
                "music_ellememe_Tous les jours",                      
                "music_ellememe_De temps en temps",                  
                "music_ellememe_Rarement",                            
                "music_ellememe_Jamais",                              
                "music_manque_Oui, beaucoup",                        
                "music_manque_Oui, un peu",                         
                "music_manque_Non",                                
                "stream_spe",                                         
                "cd_ou_cass",                                         
                "radio",                                             
                "nbr_genre_music",                                 
                "nbr_artiste_ecoute",                               
                "aime_clas",                                         
                "detest_clas",
                "musee_art_12m",
                "galerie_12m",
                "ordi",
                "acces_internet",
                "info_internet_S'informe en ligne",
                "freq_info_Tous les jours",
                "freq_info_Une ou plusieurs/semaines",
                "freq_info_Plus rarement",
                "freq_lecture_Tous les jours",
                "freq_lecture_1/semaine au moins",
                "freq_lecture_1/mois au mois",
                "freq_lecture_Plus rarement",
                "freq_lecture_Ne lit pas",
                "equip_lecture_Mixte",
                "equip_lecture_Ne lit pas de livre",
                "equip_lecture_Uniquement livre papier",
                "equip_lecture_Uniquement numérique ou autre",
                "lecture_nonFR_Oui",
                "lecture_nonFR_Non",
                "lecture_nonFR_Ne lit pas de livre",
                "freq_internet_Tous les jours",
                "freq_internet_Plusieurs fois/semaine",
                "freq_internet_Plus rarement",
                "freq_internet_Jamais",
                "reseaux_sociaux",
                "culture_en_ligne",
                "musique_enfance_Souvent",
                "musique_enfance_De temps en temps",
                "musique_enfance_Rarement",
                "musique_enfance_Jamais",
                "cinema_enfance_Souvent",
                "cinema_enfance_De temps en temps",
                "cinema_enfance_Rarement",
                "cinema_enfance_Jamais",
                "tv_enfance_Souvent",
                "tv_enfance_De temps en temps",
                "tv_enfance_Rarement",
                "tv_enfance_Jamais",
                "nbr_genre_parent_ecoute",
                "nbr_genre_ecoute_enfance",
                "autre_langue")

love_part1 <- love.plot(res_match_template_stream_serie_VOD, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Unmatched", "Matched"), 
                        title = NULL) +
  xlim(c(0,1)) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[1:70])) + 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample") +
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#d7191c", "#1a9641")) +
  theme_bw()

love_part1

ggsave(here("output", "Figure A6 serie_love_part1.png"), width = 21, height = 29.7, units = "cm", dpi = 300,
       bg = "white", device = "png")



love_part2 <- love.plot(res_match_template_stream_serie_VOD, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Unmatched", "Matched"),
                        title = NULL) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[71:143]))+ 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample")+
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#d7191c", "#1a9641")) +
  theme_bw()

love_part2

ggsave(here("output", "Figure A6 serie_love_part2.png"), width = 21, height = 29.7, units = "cm", dpi = 300,
       bg = "white", device = "png")



### Age variable

bind_rows(
  PC18_to_m_serie %>% select(AGE, serie_stream_VOD, POND) %>% 
    mutate(matching_status = "Before matching"),
  PC18_m_serie %>% select(AGE, serie_stream_VOD, POND) %>% 
    mutate(matching_status = "After matching")) %>% 
  mutate(matching_status = factor(matching_status, c("Before matching", "After matching"))) %>% 
  ggplot() + 
  aes(x = AGE, group=serie_stream_VOD, fill = as.factor(serie_stream_VOD), colour = as.factor(serie_stream_VOD),  weight = POND) +
  scale_color_manual(values=c("#d7191c", "#1a9641"),
                     labels = c("0" = "Not treated",
                                "1" ="Treated"), name = "Group") +
  scale_fill_manual(values=c("#d7191c", "#1a9641"), guide="none") +
  guides(color=guide_legend(override.aes=list(fill=c("#d7191c","#1a9641")))) +
  geom_density(alpha = 5/10) +
  facet_grid( ~ matching_status) +
  ylab("Density") +
  xlab("Age") +
  theme_bw()

ggsave(here("output", "Figure A5 serie_age.png"), width = 18, height = 10, units = "cm", dpi = 300,
       bg = "white", device = "png")

# Figure A7-A9: Detailed genres supplementary results ------

## Music

music_genre_unm <- PC18_to_m_music_survey %>%
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
                 E1008 ~ "Metal/hard rock",
                 E1009 ~ "Rock",
                 E1010 ~ "Jazz",
                 E1011 ~ "Opera",
                 E1012 ~ "Classical"),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


music_genre_m <- PC18_m_music_survey %>%
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
                 E1008 ~ "Metal/hard rock",
                 E1009 ~ "Rock",
                 E1010 ~ "Jazz",
                 E1011 ~ "Opera",
                 E1012 ~ "Classical"),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Listen to :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

music_detail <- tbl_merge(
  tbls = list(
    music_genre_unm, 
    music_genre_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")) 

d_music_detail <- music_detail$table_body %>% 
  select(label, stat_1_1, stat_1_2, stat_2_2) %>% 
  rename(non_user_unm__v = stat_1_1,
         non_user_m__v = stat_1_2,
         user__v = stat_2_2,
  ) %>%  
  mutate_all(~gsub("%,", "/", .)) %>% 
  mutate_all(~gsub("%", "", .)) %>% 
  mutate_all(~gsub(",", ".", .)) %>% 
  mutate(across(!label, as.numeric)) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("group", "valeur"), 
    names_pattern = "(.*)__(.*)"
  ) %>% 
  pivot_wider(values_from = value,
              names_from = valeur) %>% 
  mutate(
    n = case_when(group == "non_user_unm" ~ 5839,
                  group == "non_user_m" ~ 1368,
                  group == "user" ~ 1922),
    p = v / 100,
    ci_up = (p + 1.96 * sqrt((p * (1 - p)) / n)) * 100,
    ci_down = (p - 1.96 * sqrt((p * (1 - p)) / n)) * 100)

plot <- d_music_detail %>% 
  mutate(group = factor(group, c("user", "non_user_m", "non_user_unm"), labels = c("Users",
                                                                                   "Non-users matched", 
                                                                                   "Non-users unmatched"))) %>% 
  ggplot(aes(x = group, y = v, color = group)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = ci_down, 
                    ymax = ci_up),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  facet_grid(fct_reorder(label, v, .desc = T) ~ ., switch = "y") +
  labs(color = "Groups") +
  guides(color = guide_legend(reverse = T)) +
  xlab("") +
  ylab("Proportion") +
  scale_color_manual(values=c("#1b9e77", "#7570b3", "#d95f02")) +
  theme_bw()

plot

# ggsave(filename = "Proportion music genre detailed.png",
#        path = "output",
#        device = "png",
#        width = 22,
#        height = 23,
#        units = "cm")



### Difference in proportion

d_music_detail_diff <- d_music_detail %>% 
  select(-v, -ci_up,-ci_down) %>% 
  pivot_wider(names_from = "group",
              values_from = c(n,p)) %>% 
  mutate(diff_unm = p_user - p_non_user_unm,
         diff_m = p_user - p_non_user_m,
         ci_unm = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                                (p_non_user_unm * (1 - p_non_user_unm)) / n_non_user_unm),
         ci_m = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                              (p_non_user_m * (1 - p_non_user_m)) / n_non_user_m)
  ) %>% 
  select(label, contains("diff"), contains("ci")) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("stat", "group"), 
    names_pattern = "(.*)_(.*)"
  ) %>% 
  pivot_wider(names_from = "stat",
              values_from = "value")

plot <- d_music_detail_diff %>% 
  mutate(group = factor(group, c("m", "unm"), labels = c("Matched", "Unmatched"))) %>% 
  ggplot(aes(x = fct_reorder(label, diff, .desc = F), y = diff, color = group)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = diff - ci, 
                    ymax = diff + ci),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  guides(color = guide_legend(reverse = T)) +
  xlab("") +
  ylab("Difference in proportion") +
  labs(color="Sample") +
  scale_color_manual(values=c("#1a9641", "#d7191c")) +
  theme_bw()

plot

ggsave(filename = "Figure A7 Diff in proportion music genre detailed.png",
       path = "output",
       device = "png",
       width = 22,
       height = 23,
       units = "cm")


## Film

film_genre_unm <- PC18_to_m_film_survey %>%
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
                 C2615 ~ "Musicals",
                 C2616 ~ "Political"),
    by = film_stream_VOD, 
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


film_genre_m <- PC18_m_film_survey %>%
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
                 C2615 ~ "Musicals",
                 C2616 ~ "Political"),
    by = film_stream_VOD, 
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ans ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(0,0),
                  all_categorical() ~ 3)
  ) %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

film_detail <- tbl_merge(
  tbls = list(
    film_genre_unm, 
    film_genre_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")) 

d_film_detail <- film_detail$table_body %>% 
  select(label, stat_1_1, stat_1_2, stat_2_2) %>% 
  rename(non_user_unm__v = stat_1_1,
         non_user_m__v = stat_1_2,
         user__v = stat_2_2,
  ) %>%  
  mutate_all(~gsub("%,", "/", .)) %>% 
  mutate_all(~gsub("%", "", .)) %>% 
  mutate_all(~gsub(",", ".", .)) %>% 
  mutate(across(!label, as.numeric)) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("group", "valeur"), 
    names_pattern = "(.*)__(.*)"
  ) %>% 
  pivot_wider(values_from = value,
              names_from = valeur) %>% 
  mutate(
    n = case_when(group == "non_user_unm" ~ 4844,
                  group == "non_user_m" ~ 1350,
                  group == "user" ~ 1925),
    p = v / 100,
    ci_up = (p + 1.96 * sqrt((p * (1 - p)) / n)) * 100,
    ci_down = (p - 1.96 * sqrt((p * (1 - p)) / n)) * 100)

plot <- d_film_detail %>% 
  mutate(group = factor(group, c("user", "non_user_m", "non_user_unm"), labels = c("Users",
                                                                                   "Non-users matched", 
                                                                                   "Non-users unmatched"))) %>% 
  ggplot(aes(x = group, y = v, color = group)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = ci_down, 
                    ymax = ci_up),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  facet_grid(fct_reorder(label, v, .desc = T) ~ ., switch = "y") +
  labs(color = "Groups") +
  guides(color = guide_legend(reverse = T)) +
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("#1b9e77", "#7570b3", "#d95f02")) +
  theme_bw()

plot

# ggsave(filename = "Proportion film genre detailed.png",
#        path = "output",
#        device = "png",
#        width = 22,
#        height = 23,
#        units = "cm")

### Difference in proportion

d_film_detail_diff <- d_film_detail %>% 
  select(-v, -ci_up,-ci_down) %>% 
  pivot_wider(names_from = "group",
              values_from = c(n,p)) %>% 
  mutate(diff_unm = p_user - p_non_user_unm,
         diff_m = p_user - p_non_user_m,
         ci_unm = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                                (p_non_user_unm * (1 - p_non_user_unm)) / n_non_user_unm),
         ci_m = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                              (p_non_user_m * (1 - p_non_user_m)) / n_non_user_m)
  ) %>% 
  select(label, contains("diff"), contains("ci")) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("stat", "group"), 
    names_pattern = "(.*)_(.*)"
  ) %>% 
  pivot_wider(names_from = "stat",
              values_from = "value")


plot <- d_film_detail_diff %>% 
  mutate(group = factor(group, c("m", "unm"), labels = c("Matched", "Unmatched"))) %>% 
  ggplot(aes(x = fct_reorder(label, diff, .desc = F), y = diff, color = group)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = diff - ci, 
                    ymax = diff + ci),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  guides(color = guide_legend(reverse = T)) +
  coord_flip() +
  xlab("") +
  ylab("Difference in proportion") +
  labs(color="Sample") +
  scale_color_manual(values=c("#1a9641", "#d7191c")) +
  theme_bw()

plot

ggsave(filename = "Figure A8 Diff in proportion film genre detailed.png",
       path = "output",
       device = "png",
       width = 22,
       height = 23,
       units = "cm")



## TV series


show_genre_unm <- PC18_to_m_serie_survey %>%
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
                 C3809 ~ "Series d'auteur",
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
    digits = list(all_categorical() ~ c(3))
  ) %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


show_genre_m <- PC18_m_serie_survey %>%
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
                 C3809 ~ "Series d'auteur",
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
    digits = list(all_categorical() ~ c(3))
  )  %>%
  modify_header(label = "**Watch :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

show_detail <- tbl_merge(
  tbls = list(
    show_genre_unm, 
    show_genre_m),
  tab_spanner = c("**Unmatched sample**", "**Matched sample**")) 

d_show_detail <- show_detail$table_body %>% 
  select(label, stat_1_1, stat_1_2, stat_2_2) %>% 
  rename(non_user_unm__v = stat_1_1,
         non_user_m__v = stat_1_2,
         user__v = stat_2_2,
  ) %>%  
  mutate_all(~gsub("%,", "/", .)) %>% 
  mutate_all(~gsub("%", "", .)) %>% 
  mutate_all(~gsub(",", ".", .)) %>% 
  mutate(across(!label, as.numeric)) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("group", "valeur"), 
    names_pattern = "(.*)__(.*)"
  ) %>% 
  pivot_wider(values_from = value,
              names_from = valeur) %>% 
  mutate(
    n = case_when(group == "non_user_unm" ~ 4122,
                  group == "non_user_m" ~ 1024,
                  group == "user" ~ 1463),
    p = v / 100,
    ci_up = (p + 1.96 * sqrt((p * (1 - p)) / n)) * 100,
    ci_down = (p - 1.96 * sqrt((p * (1 - p)) / n)) * 100)

plot <- d_show_detail %>% 
  mutate(group = factor(group, c("user", "non_user_m", "non_user_unm"), labels = c("Users",
                                                                                   "Non-users matched", 
                                                                                   "Non-users unmatched"))) %>% 
  ggplot(aes(x = group, y = v, color = group)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = ci_down, 
                    ymax = ci_up),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  facet_grid(fct_reorder(label, v, .desc = T) ~ ., switch = "y") +
  labs(color = "Groups") +
  guides(color = guide_legend(reverse = T)) +
  xlab("") +
  ylab("") +
  scale_color_manual(values=c("#1b9e77", "#7570b3", "#d95f02")) +
  theme_bw()

plot

# ggsave(filename = "Proportion show genre detailed.png",
#        path = "output",
#        device = "png",
#        width = 22,
#        height = 23,
#        units = "cm")



### Difference in proportion

d_show_detail_diff <- d_show_detail %>% 
  select(-v, -ci_up,-ci_down) %>% 
  pivot_wider(names_from = "group",
              values_from = c(n,p)) %>% 
  mutate(diff_unm = p_user - p_non_user_unm,
         diff_m = p_user - p_non_user_m,
         ci_unm = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                                (p_non_user_unm * (1 - p_non_user_unm)) / n_non_user_unm),
         ci_m = 1.96 * sqrt((p_user * (1 - p_user)) / n_user + 
                              (p_non_user_m * (1 - p_non_user_m)) / n_non_user_m)
  ) %>% 
  select(label, contains("diff"), contains("ci")) %>% 
  pivot_longer(
    cols = !label,
    names_to = c("stat", "group"), 
    names_pattern = "(.*)_(.*)"
  ) %>% 
  pivot_wider(names_from = "stat",
              values_from = "value")

plot <- d_show_detail_diff %>% 
  mutate(group = factor(group, c("m", "unm"), labels = c("Matched", "Unmatched"))) %>% 
  ggplot(aes(x = fct_reorder(label, diff, .desc = F), y = diff, color = group)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = diff - ci, 
                    ymax = diff + ci),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  guides(color = guide_legend(reverse = T)) +
  coord_flip() +
  xlab("") +
  ylab("Difference in proportion") +
  labs(color="Sample") +
  scale_color_manual(values=c("#1a9641", "#d7191c")) +
  theme_bw()


plot

ggsave(filename = "Figure A9 Diff in proportion show genre detailed.png",
       path = "output",
       device = "png",
       width = 22,
       height = 23,
       units = "cm")



## the three together

d_three_diff <- dplyr::union(d_music_detail_diff %>% mutate(medium = "Music"),
                             d_film_detail_diff %>% mutate(medium = "Film")) %>% 
  dplyr::union(d_show_detail_diff %>% mutate(medium = "Show")) %>% 
  mutate(medium = factor(medium, c("Music", "Film", "Show"), labels = c("Music", "Film", "Show")),
         group = factor(group, c("m", "unm"), labels = c("Matched", "Unmatched"))) %>% 
  group_by(medium) %>% 
  filter(medium == "Music" & label %in% c("Pop", "EDM", "Rock") |
           medium == "Film" & label %in% c("Comedy", "Action", "Historical", "Thrillers", "Adventure", "Horror", "Science fiction") |
           medium == "Show" & label %in% c("Comedy", "Action", "Historical", "Thrillers", "Adventure", "Drama", "Animated", "Horror", "Author", "Documentary", "Western", "Science fiction")
  )


plot <- d_three_diff %>% 
  ggplot(aes(x = fct_reorder(label, diff, .desc = F), y = diff, color = group)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = diff - ci, 
                    ymax = diff + ci),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  facet_grid(medium ~ ., scales = "free") +
  guides(color = guide_legend(reverse = T)) +
  coord_flip() +
  xlab("") +
  ylab("Difference in proportion") +
  labs(color="Sample",
       title = "Difference in the prevalence of genres consumed beetween users and non-users") +
  scale_color_manual(values=c("#1a9641", "#d7191c")) +
  theme_bw()

plot

ggsave(filename = "Diff in proportion (signif genre only) detailed.png",
       path = "output/presentations",
       device = "png",
       width = 22,
       height = 23,
       units = "cm")


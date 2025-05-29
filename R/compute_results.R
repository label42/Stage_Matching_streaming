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
  mutate(medium = "TV Shows") %>% 
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
       filename = "Table 1 Size of treated untreated control.tex",
       path = here("output"))


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
                 C3812 ~ "Erotic",
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
                 C3812 ~ "Erotic",
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
                              "nbr_genre_serie" = "TV shows genre"),
         name = factor(name, c("TV shows genre", "Film genre", "Music genre")),
         sample = recode_factor(sample,
                                "matched" = "Net difference",
                                "unmatched" = "Raw difference")) %>% 
  ggplot(aes(x = name, y = smd, color = sample)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = smd - 1.96*std.error, 
                    ymax = smd + 1.96*std.error),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  xlab("") +
  ylab("Standardized mean difference") +
  labs(color="") +
  coord_flip() +
  guides(color = guide_legend(reverse = T)) + 
  scale_color_manual(values=c("#722929", "#D09898")) +
  theme_bw()+ 
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0),
        legend.box.margin = margin(t = 0),
        legend.box.spacing = unit(0, "mm"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

plot

ggsave(filename = "Fig pres nbr genre consumed.png",
       path = "output",
       device = "png",
       width = 13,
       height = 5,
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
                             "nbr_genre_serie" = "TV shows",
                             "nbr_genre_serie_aime" = "TV shows",
                             "nbr_genre_serie_deteste" = "TV shows"),
         name = recode_factor(name, 
                              "nbr_genre_music" = "consumed",
                              "nbr_genre_aime" = "liked",
                              "nbr_genre_deteste" = "hated",
                              "nbr_genre_film" = "consumed",
                              "nbr_genre_film_aime" = "liked",
                              "nbr_genre_film_deteste" = "hated",
                              "nbr_genre_serie" = "consumed",
                              "nbr_genre_serie_aime" = "liked",
                              "nbr_genre_serie_deteste" = "hated"),
         name = factor(name, c("hated", "liked", "consumed")),
         sample = recode_factor(sample,
                                "matched" = "Net difference",
                                "unmatched" = "Raw difference")) %>% 
  ggplot(aes(x = name, y = smd, color = sample)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = smd - 1.96*std.error, 
                    ymax = smd + 1.96*std.error),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  facet_grid(cat ~ .) +
  xlab("Number of genres...") +
  ylab("Standardized mean difference") +
  labs(color="") +
  guides(color = guide_legend(reverse = T)) + 
  scale_color_manual(values=c("#722929", "#D09898")) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0),
        legend.box.margin = margin(t = 0),
        legend.box.spacing = unit(0, "mm"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

plot

ggsave(filename = here("output", "Figure 1 SMD genre detailed.png"),
       device = "png",
       width = 9,
       height = 9,
       units = "cm")

# Compute the differences in terms of number of genres rather than just sd:
t1 <- PC18_m_music %>% 
  select(nbr_genre_music, nbr_genre_aime, nbr_genre_deteste, stream_spe, POND_m) %>% 
  pivot_longer(nbr_genre_music:nbr_genre_deteste) %>% 
  group_by(stream_spe, name) %>% 
  summarize(m = sum(POND_m*value)/n()) %>% 
  pivot_wider(names_from = stream_spe, values_from = m) %>% 
  mutate(sample = "matched", domain = "music")

t2 <- PC18_m_film %>% 
  select(nbr_genre_film, nbr_genre_film_aime, nbr_genre_film_deteste, film_stream_VOD, POND_m) %>% 
  pivot_longer(nbr_genre_film:nbr_genre_film_deteste) %>% 
  group_by(film_stream_VOD, name) %>% 
  summarize(m = sum(POND_m*value)/n()) %>% 
  pivot_wider(names_from = film_stream_VOD, values_from = m) %>% 
  mutate(sample = "matched", domain = "film")

t3 <- PC18_m_serie %>% 
  select(nbr_genre_serie, nbr_genre_serie_aime, nbr_genre_serie_deteste, serie_stream_VOD, POND_m) %>% 
  pivot_longer(nbr_genre_serie:nbr_genre_serie_deteste) %>% 
  group_by(serie_stream_VOD, name) %>% 
  summarize(m = sum(POND_m*value)/n()) %>% 
  pivot_wider(names_from = serie_stream_VOD, values_from = m) %>% 
  mutate(sample = "matched", domain = "serie")

bind_rows(t1, t2, t3) %>% 
  rename(nonusers = "0", users = "1") %>% 
  mutate(diff = users-nonusers) %>% 
  gt() %>% 
  gtsave("effect_estimate.tex", path="output")


# Table A??: E-values

result_to_plot %>% 
  filter(sample == "matched") %>% 
  rowwise() %>% 
  mutate(e_value_m = evalues.MD(smd, se = std.error, true = 0)[2, 1],
         e_value_l = evalues.MD(smd, se = std.error, true = 0)[2, 2],
         e_value_u = evalues.MD(smd, se = std.error, true = 0)[2, 3]) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  gt() %>% 
  gtsave(filename = "Table A3 sensitivity.tex",
         path = here("output"))

# Figure 2: Foreign language -------

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
  dplyr::union(d_show_lang_diff %>% mutate(medium = "TV shows")) %>% 
  mutate(medium = factor(medium, c("TV shows", "Movie", "Music"), labels = c("TV shows", "Movie", "Music")),
         group = factor(group, c("m", "unm"), labels = c("Net difference", "Raw difference"))) 

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
  labs(color="Sample")  + 
  scale_color_manual(values=c("#722929", "#D09898")) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0),
        legend.box.margin = margin(t = 0),
        legend.box.spacing = unit(0, "mm"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

plot

ggsave(filename = here("output","Figure 2 Diff in foreign language.png"),
       device = "png",
       width = 9,
       height = 4,
       units = "cm")

# Table A1: Control variables ------

controls <- read_csv(here("data", "control_variables.csv")) %>% 
  filter(film | music | serie) %>% 
  mutate(film = ifelse(film, "F", ""),
         music = ifelse(music, "M", ""),
         serie = ifelse(serie, "S", ""),
         model = paste0(music, film, serie),
         set = factor(set, 
                      levels = c("demographics", "social", "digital use", 
                                 "childhood participation", 
                                 "intensity cultural participation", "cultural diversity"),
                      labels = c("Demographics",
                                 "Social Position and Origin",
                                 "Digital Uses",
                                 "Cultural Participation during Childhood",
                                 "Intensity of Cultural Participation",
                                 "Propensity towards diversity")
         ),
         full_line = paste(varlabel, vartype, 
                           #varorig, 
                           model, sep = " & ") %>% 
           paste(., "\\\\\n")
  ) 
if(file.exists(here("output", "Table A1_controls.tex"))) file.remove(here("output", "Table A1_controls.tex"))
cat("
\\begin{ThreePartTable}
\\centering
\\begingroup\\small
\\begin{TableNotes}
\\item Model indicates whether the variable was used as a control
in the Music (M), Movies (F) and/or TV Shows (S) model.
\\item Type indicates whether the variable is binary (Bin), categorical
(Cat), or numeric (Num).
\\vspace{10pt}
\\end{TableNotes}
\\begin{longtable}{p{4.5cm}p{1cm}p{5cm}p{1cm}}
\\caption{List of control variables}
\\label{tab:controls}\\\\
\\insertTableNotes\\\\
\\hline
\\textbf{Description} & \\textbf{Type} & \\textbf{Orignal question} & \\textbf{Model}\\\\
\\hline
\\endfirsthead
\\multicolumn{4}{c}%
{\\tablename\\ \\thetable\\ -- \\textit{Continued from previous page}} \\\\
\\hline
\\textbf{Description} & \\textbf{Type} & \\textbf{Orignal question} & \\textbf{Model}\\\\
\\hline
\\endhead
\\hline \\multicolumn{4}{r}{\\textit{Continued on next page}} \\\\
\\endfoot
\\hline
\\endlastfoot
    ",
    sep = "",
    file = here("output", "Table A1_controls.tex"),
    append = TRUE
)
for(s in levels(controls$set)){
  x <- controls %>% 
    filter(set == s)
  cat("\n\\vspace{20pt}\\\\\n
\\multicolumn{4}{c}{\\textbf{", s, "}}\\\\*\n", 
      x$full_line, 
      sep = "",
      file = here("output", "Table A1_controls.tex"),
      append = TRUE)
}
cat("
\\end{longtable}
\\endgroup
\\end{ThreePartTable}", 
    sep = "",
    file = here("output", "Table A1_controls.tex"),
    append = TRUE)
# Table 2: Demographics of streaming users ------

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
    tab_spanner = c("**Music**", "**Movies**", "**TV shows**")
  ) %>%
  as_gt()

gtsave(tbl_all_stream_socdem, 
       filename = "Table 2 Descriptive statistics.tex",
       path = here("output"))


# Figure A1-A6: Evaluate quality of matching ------

## Music ------

loveplot_find_cutoff <- function(allvar_mod){
  co <- tibble(allvar_mod = allvar_mod, var = str_remove(allvar_mod, "_[^_]*?$")) %>% 
    mutate(same = var == lag(var), 
           n = row_number(), 
           half = n %/% ((nrow(.)-1)/2)) %>% 
    filter(half == 0, same) %>% 
    filter(n == max(n)) %>% 
    pull(n)
  return(co)
}

tab_eval_music <- bal.tab(res_match_template_stream_music, binary = "std", thresholds = c(m = 0.05))

allvar_mod <- rownames(tab_eval_music$Balance)
cutoff <- loveplot_find_cutoff(allvar_mod)

love_part1 <- love.plot(res_match_template_stream_music, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Before matching", "After matching"), 
                        title = NULL) +
  xlim(c(0,1)) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[1:cutoff])) + 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample") +
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#D09898", "#722929")) +
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
                        sample.names = c("Before matching", "After matching"),
                        title = NULL) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[(cutoff+1):length(allvar_mod)]))+ 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample")+
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#D09898", "#722929")) +
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
  scale_color_manual(values=c("#D09898", "#722929"),
                     labels = c("0" = "Not treated",
                                "1" ="Treated"), name = "Group") +
  scale_fill_manual(values=c("#D09898", "#722929"), guide="none") +
  guides(color=guide_legend(override.aes=list(fill=c("#D09898", "#722929")))) +
  geom_density(alpha = 5/10) +
  facet_grid( ~ matching_status) +
  ylab("Density") +
  xlab("Age") +
  theme_bw()

ggsave(here("output", "Figure A1 music_age.png"), width = 18, height = 10, units = "cm", dpi = 300,
       bg = "white", device = "png")


## Film ------

tab_eval_film <- bal.tab(res_match_template_stream_film_VOD, binary = "std", thresholds = c(m = 0.05))

allvar_mod <- rownames(tab_eval_film$Balance)
cutoff <- loveplot_find_cutoff(allvar_mod)

love_part1 <- love.plot(res_match_template_stream_film_VOD, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Before matching", "After matching"), 
                        title = NULL) +
  xlim(c(0,1)) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[1:cutoff])) + 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample") +
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#D09898", "#722929")) +
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
                        sample.names = c("Before matching", "After matching"), 
                        title = NULL) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[(cutoff+1):length(allvar_mod)]))+ 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample")+
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#D09898", "#722929")) +
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
  scale_color_manual(values=c("#D09898", "#722929"),
                     labels = c("0" = "Not treated",
                                "1" ="Treated"), name = "Group") +
  scale_fill_manual(values=c("#D09898", "#722929"), guide="none") +
  guides(color=guide_legend(override.aes=list(fill=c("#D09898", "#722929")))) +
  geom_density(alpha = 5/10) +
  facet_grid( ~ matching_status) +
  ylab("Density") +
  xlab("Age") +
  theme_bw()

ggsave(here("output", "Figure A3 film_age.png"), width = 18, height = 10, units = "cm", dpi = 300,
       bg = "white", device = "png")



## TV Series -------

tab_eval_serie <- bal.tab(res_match_template_stream_serie_VOD, binary = "std", thresholds = c(m = 0.05))

allvar_mod <- rownames(tab_eval_serie$Balance)
cutoff <- loveplot_find_cutoff(allvar_mod)

love_part1 <- love.plot(res_match_template_stream_serie_VOD, 
                        drop.distance = TRUE, 
                        var.order = "unadjusted",
                        abs = TRUE,
                        thresholds = c(m = .05), 
                        binary = "std",
                        continuous = "std",
                        sample.names = c("Before matching", "After matching"), 
                        title = NULL) +
  xlim(c(0,1)) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[1:cutoff])) + 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample") +
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#D09898", "#722929")) +
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
                        sample.names = c("Before matching", "After matching"),
                        title = NULL) +
  xlab("Standardized Proportion Difference") +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.5)) +
  ylim(rev(allvar_mod[(cutoff+1):length(allvar_mod)]))+ 
  labs(color = "Sample", shape = "Sample", size = "Sample", group = "Sample", stroke = "Sample")+
  theme(axis.text.y = element_text(size = 10)) + 
  scale_color_manual(values=c("#D09898", "#722929")) +
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
  scale_color_manual(values=c("#D09898", "#722929"),
                     labels = c("0" = "Not treated",
                                "1" ="Treated"), name = "Group") +
  scale_fill_manual(values=c("#D09898", "#722929"), guide="none") +
  guides(color=guide_legend(override.aes=list(fill=c("#D09898", "#722929")))) +
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
  mutate(group = factor(group, c("m", "unm"), labels = c("Net difference", "Raw difference"))) %>% 
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
  labs(color="") +
  scale_color_manual(values=c("#722929", "#D09898")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0),
        legend.box.margin = margin(t = 0),
        legend.box.spacing = unit(0, "mm"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

plot

ggsave(filename = here("output", "Figure A7 Diff in proportion music genre detailed.png"),
       device = "png",
       width = 13,
       height = 10,
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
  mutate(group = factor(group, c("m", "unm"), labels = c("Net difference", "Raw difference"))) %>% 
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
  labs(color="") +
  scale_color_manual(values=c("#722929", "#D09898")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0),
        legend.box.margin = margin(t = 0),
        legend.box.spacing = unit(0, "mm"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

plot

ggsave(filename = here(  "output","Figure A8 Diff in proportion film genre detailed.png"),
       device = "png",
       width = 13,
       height = 11,
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
                 C3812 ~ "Erotic",
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
                 C3812 ~ "Erotic",
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
  mutate(group = factor(group, c("m", "unm"), labels = c("Net difference", "Raw difference"))) %>% 
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
  labs(color="") +
  scale_color_manual(values=c("#722929", "#D09898")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0),
        legend.box.margin = margin(t = 0),
        legend.box.spacing = unit(0, "mm"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))


plot

ggsave(filename = here("output","Figure A9 Diff in proportion show genre detailed.png"),
       device = "png",
       width = 13,
       height = 11,
       units = "cm")



## the three together

d_three_diff <- dplyr::union(d_music_detail_diff %>% mutate(medium = "Music"),
                             d_film_detail_diff %>% mutate(medium = "Movies")) %>% 
  dplyr::union(d_show_detail_diff %>% mutate(medium = "TV shows")) %>% 
  mutate(medium = factor(medium, c("Music", "Movies", "TV shows")),
         group = factor(group, c("m", "unm"), labels = c("Matched", "Unmatched"))) %>% 
  group_by(medium) %>% 
  filter(medium == "Music" & label %in% c("Pop", "EDM", "Rock") |
           medium == "Movies" & label %in% c("Comedy", "Action", "Historical", "Thrillers", "Adventure", "Horror", "Science fiction") |
           medium == "TV shows" & label %in% c("Comedy", "Action", "Historical", "Thrillers", "Adventure", "Drama", "Animated", "Horror", "Author", "Documentary", "Western", "Science fiction")
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
  scale_color_manual(values=c("#722929", "#D09898")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0),
        legend.box.margin = margin(t = 0),
        legend.box.spacing = unit(0, "mm"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

plot

ggsave(filename = "Fig_pres Diff in proportion (signif genre only) detailed.png",
       path = "output",
       device = "png",
       width = 22,
       height = 23,
       units = "cm")

# Figure A10: Effects of streaming on unrelated outcomes ------

## Music ------

### Diversity of genre

mean_genre_unm <- PC18_to_m_music_survey %>%
  tbl_svysummary(
    include = c("stream_spe", "nbr_genre_prgtele", "nbr_genre_actu", "nbr_genre_radio"),
    label = list(
      nbr_genre_prgtele ~ "Number of types of TV programs watched",
      nbr_genre_actu ~ "Number of news domain interested in",
      nbr_genre_radio ~ "Number of radio program genres listened"
    ),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Number of :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


mean_genre_m <- PC18_m_music_survey %>%
  tbl_svysummary(
    include = c("stream_spe", "nbr_genre_prgtele", "nbr_genre_actu", "nbr_genre_radio"),
    label = list(
      nbr_genre_prgtele ~ "Number of types of TV programs watched",
      nbr_genre_actu ~ "Number of news domain interested in",
      nbr_genre_radio ~ "Number of radio program genres listened"
    ),
    by = stream_spe, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Number of :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))
t_comp_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Non-matched sample**", "**Matched sample**")
) %>% as_gt() 

t_comp_m

## Film ------

### Diversity of genre

mean_genre_unm <- PC18_to_m_film_survey %>%
  tbl_svysummary(
    include = c("film_stream_VOD", "nbr_genre_prgtele", "nbr_genre_actu", "nbr_genre_radio"),
    label = list(
      nbr_genre_prgtele ~ "Number of types of TV programs watched",
      nbr_genre_actu ~ "Number of news domain interested in",
      nbr_genre_radio ~ "Number of radio program genres listened"
    ),
    by = film_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Number of :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))


mean_genre_m <- PC18_m_film_survey %>%
  tbl_svysummary(
    include = c("film_stream_VOD", "nbr_genre_prgtele", "nbr_genre_actu", "nbr_genre_radio"),
    label = list(
      nbr_genre_prgtele ~ "Number of types of TV programs watched",
      nbr_genre_actu ~ "Number of news domain interested in",
      nbr_genre_radio ~ "Number of radio program genres listened"
    ),
    by = film_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Number of :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

t_comp_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Non-matched sample**", "**Matched sample**")
) %>% 
  as_gt()

t_comp_m

## Series ------

### Diversity of genre

mean_genre_unm <- PC18_to_m_serie_survey %>%
  tbl_svysummary(
    include = c("serie_stream_VOD", "nbr_genre_prgtele", "nbr_genre_actu", "nbr_genre_radio"),
    label = list(
      nbr_genre_prgtele ~ "Number of types of TV programs watched",
      nbr_genre_actu ~ "Number of news domain interested in",
      nbr_genre_radio ~ "Number of radio program genres listened"
    ),
    by = serie_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Number of :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

mean_genre_m <- PC18_m_serie_survey %>%
  tbl_svysummary(
    include = c("serie_stream_VOD", "nbr_genre_prgtele", "nbr_genre_actu", "nbr_genre_radio"),
    label = list(
      nbr_genre_prgtele ~ "Number of types of TV programs watched",
      nbr_genre_actu ~ "Number of news domain interested in",
      nbr_genre_radio ~ "Number of radio program genres listened"
    ),
    by = serie_stream_VOD, 
    missing = "ifany",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p} %"),
    digits = list(all_continuous() ~ c(1,1))
  ) %>%
  modify_header(label = "**Number of :**",
                update = list(stat_1 ~ gt::html("**Non-user**<br> N = {n_unweighted}"),
                              stat_2 ~ gt::html("**User**<br> N = {n_unweighted}")))

t_comp_m <- tbl_merge(
  tbls = list(mean_genre_unm, mean_genre_m),
  tab_spanner = c("**Non-matched sample**", "**Matched sample**")
) %>% as_gt()  

t_comp_m


# graphe of standardized mean diffrence between users and non users

diff_unm <- PC18_to_m_music %>% summarize_at(
  .vars = vars(c(nbr_genre_prgtele, nbr_genre_actu, nbr_genre_radio)),
  .funs = list(smd = ~ smd(., g = stream_spe, w = POND, gref = 2, std.error = T)$estimate,
               std.error = ~ smd(., g = stream_spe, w = POND, gref = 2, std.error = T)$std.error)) %>%
  rename_with(~paste0("Music_", .x)) %>%  
  bind_cols(PC18_to_m_film %>% summarize_at(
    .vars = vars(c(nbr_genre_prgtele, nbr_genre_actu, nbr_genre_radio)),
    .funs = list(smd = ~ smd(., g = film_stream_VOD, w = POND, gref = 2, std.error = T)$estimate,
                 std.error = ~ smd(., g = film_stream_VOD, w = POND, gref = 2, std.error = T)$std.error)) %>%
      rename_with(~paste0("Movies_", .x)) 
    
  ) %>% 
  bind_cols(PC18_to_m_serie %>% summarize_at(
    .vars = vars(c(nbr_genre_prgtele, nbr_genre_actu, nbr_genre_radio)),
    .funs = list(smd = ~ smd(., g = serie_stream_VOD, w = POND, gref = 2, std.error = T)$estimate,
                 std.error = ~ smd(., g = serie_stream_VOD, w = POND, gref = 2, std.error = T)$std.error)) %>%
      rename_with(~paste0("TV shows_", .x)) 
  ) %>%   
  mutate(sample = "unmatched")

diff_m <- PC18_m_music %>% summarize_at(
  .vars = vars(c(nbr_genre_prgtele, nbr_genre_actu, nbr_genre_radio)),
  .funs = list(smd = ~ smd(., g = stream_spe, w = POND_m, gref = 2, std.error = T)$estimate,
               std.error = ~ smd(., g = stream_spe, w = POND_m, gref = 2, std.error = T)$std.error)) %>%
  rename_with(~paste0("Music_", .x)) %>%   
  bind_cols(PC18_m_film %>% summarize_at(
    .vars = vars(c(nbr_genre_prgtele, nbr_genre_actu, nbr_genre_radio)),
    .funs = list(smd = ~ smd(., g = film_stream_VOD, w = POND_m, gref = 2, std.error = T)$estimate,
                 std.error = ~ smd(., g = film_stream_VOD, w = POND_m, gref = 2, std.error = T)$std.error)) %>%
      rename_with(~paste0("Movies_", .x)) 
    
  ) %>% 
  bind_cols(PC18_m_serie %>% summarize_at(
    .vars = vars(c(nbr_genre_prgtele, nbr_genre_actu, nbr_genre_radio)),
    .funs = list(smd = ~ smd(., g = serie_stream_VOD, w = POND_m, gref = 2, std.error = T)$estimate,
                 std.error = ~ smd(., g = serie_stream_VOD, w = POND_m, gref = 2, std.error = T)$std.error)) %>%
      rename_with(~paste0("TV shows_", .x)) 
  ) %>% 
  mutate(sample = "matched")


result_to_plot <- bind_rows(diff_unm, diff_m) %>% 
  pivot_longer(-sample) %>%
  mutate(ind = str_extract(name, "(smd|std.error)"),
         name = str_remove(name, "(smd|std.error)"),
         model = str_extract(name, "^(Music|Movies|TV shows)"),
         name = str_remove(name, "^(Music|Movies|TV shows)_"),
  )  %>%
  pivot_wider(names_from = ind, values_from = value) 


plot <- result_to_plot %>%  
  mutate(sample = recode_factor(sample,
                                "matched" = "Net difference",
                                "unmatched" = "Raw difference"),
         name = recode_factor(name, 
                              "nbr_genre_prgtele_" = "TV programs",
                              "nbr_genre_actu_" = "News",
                              "nbr_genre_radio_" = "Radio")) %>%
  ggplot(aes(x = name, y = smd, color = sample)) +
  geom_point(position = position_dodge(.4)) +
  geom_errorbar(aes(ymin = smd - 1.96*std.error, 
                    ymax = smd + 1.96*std.error),
                width = .2,
                position = position_dodge(.4)) +
  geom_hline(aes(yintercept=0)) +
  xlab("") +
  ylab("Standardized mean difference") +
  facet_wrap(~model) +
  labs(color="") +
  coord_flip() +
  guides(color = guide_legend(reverse = T)) +
  scale_color_manual(values=c("#722929", "#D09898")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0),
        legend.box.margin = margin(t = 0),
        legend.box.spacing = unit(0, "mm"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

plot


ggsave(filename = here("output","Figure A10. Robustness_other_outcomes.png"),
       device = "png",
       width = 13,
       height = 6,
       units = "cm")


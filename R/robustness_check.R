library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(cobalt)
library(survey)
library(smd)
library(ggthemes)

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


# Effects of streaming on unrelated outcomes ------

# Effect of streaming on the diversity of literary genre consumed

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
      rename_with(~paste0("TV series_", .x)) 
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
      rename_with(~paste0("TV series_", .x)) 
  ) %>% 
  mutate(sample = "matched")


result_to_plot <- bind_rows(diff_unm, diff_m) %>% 
  pivot_longer(-sample) %>%
  mutate(ind = str_extract(name, "(smd|std.error)"),
         name = str_remove(name, "(smd|std.error)"),
         model = str_extract(name, "^(Music|Movies|TV series)"),
         name = str_remove(name, "^(Music|Movies|TV series)_"),
  )  %>%
  pivot_wider(names_from = ind, values_from = value) 


plot <- result_to_plot %>%  
  mutate(sample = recode_factor(sample,
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
  facet_wrap(~model) +
  labs(color="Sample") +
  coord_flip() +
  guides(color = guide_legend(reverse = T)) + 
  scale_color_manual(values=c("#1a9641", "#d7191c")) +
  theme_bw()

plot


ggsave(filename = "Robustness_other_outcomes.png",
       path = "output/robustness check",
       device = "png",
       width = 22,
       height = 10,
       units = "cm")

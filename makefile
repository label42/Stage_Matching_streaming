all: R/compute_results.R data/PC18.RData data/film_matched.RData data/music_matched.RData data/serie_matched.RData output/Figure_A11_Without_Digital_Use_SMD_genre_detailed.png output/Figure_A12_With_15-20yo_SMD_genre_detailed.png
	R -f R/compute_results.R

output/Figure_A11_Without_Digital_Use_SMD_genre_detailed.png: R/robustness_without_digital_use_controls.R
	R -f R/robustness_without_digital_use_controls.R

output/Figure_A12_With_15-20yo_SMD_genre_detailed.png: R/robustness_all_respondants.R
	R -f R/robustness_all_respondants.R

data/PC18.RData: data/pc18_quetelet_octobre2021.csv R/make_base_data.R
	R -f R/make_base_data.R

data/film_matched.RData: data/PC18.RData R/gestion_NA_matching.R R/match_film.R data/control_variables.csv
	R -f R/match_film.R

data/music_matched.RData: data/PC18.RData R/gestion_NA_matching.R R/match_music.R data/control_variables.csv
	R -f R/match_music.R

data/serie_matched.RData: data/PC18.RData R/gestion_NA_matching.R R/match_serie.R data/control_variables.csv
	R -f R/match_serie.R

all: R/compute_results.R data/PC18.RData data/film_matched.RData data/music_matched.RData data/serie_matched.RData
	R -f R/compute_results.R

data/PC18.RData: data/pc18_quetelet_octobre2021.csv R/make_base_data.R
	R -f R/make_base_data.R

data/film_matched.RData: data/PC18.RData R/gestion_NA_matching.R R/match_film.R
	R -f R/match_film.R

data/music_matched.RData: data/PC18.RData R/gestion_NA_matching.R R/match_music.R
	R -f R/match_music.R

data/serie_matched.RData: data/PC18.RData R/gestion_NA_matching.R R/match_serie.R
	R -f R/match_serie.R

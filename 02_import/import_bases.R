library(here)


PC18 <- read.csv(here("01_data", "pc18_quetelet_octobre2021.csv"),sep=";")

source(here("02_import","recodage_PC18.R"))

PC18 <- subset(PC18, !is.na(PC18$POND))

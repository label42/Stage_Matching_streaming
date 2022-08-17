library(tidyverse)
library(here)
library(haven)

PC18 <- read.csv(here("01_data", "pc18_quetelet_octobre2021.csv"),sep=";")

source(here("02_import","recodage_PC18.R"))



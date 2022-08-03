library(tidyverse)
library(here)
library(haven)

PC18 <- read.csv(here("01_data", "pc18_quetelet_octobre2021.csv"),sep=";")
pc08 <- read_sas(here("01_data", "pc08_15fev2021.sas7bdat"))

# Créer labels pc08
library(yaml)
labs_pc08 <- read_yaml(here("01_data", "pc08_dic.yaml"))

lmod <- map(labs_pc08, function(x){
  if(!is.null(x$L)) {
    tibble(lab = x$V, L = names(x$L), modlabel = unlist(x$L))
  } else {
    tibble(lab = x$V, L = NA, modlabel = NA)
  }
}) %>% bind_rows()
lvar <- map(labs_pc08, function(x){
  if(!is.null(x$O)) {
    tibble(lab = x$V, O = names(x$O), qcmlabel = unlist(x$O))
  } else {
    tibble(lab = x$V, O = NA, qcmlabel = NA)
  }
}) %>% bind_rows()

# extraire les noms de variables, etc.
# faire un dico des codes propres.

lvar <- separate(lvar, col = lab, into = c("var", "type"), sep = "\\s", remove=FALSE) %>% 
  mutate(
    var = str_remove(var, "\\[.*?\\]"),
    var = ifelse(is.na(O), 
                 var,
                 paste0(var, "_O", O)),
    lab = str_remove(lab, "^.*?\""),
    lab = str_remove_all(lab, "\""),
    lab = str_remove_all(lab, "-{2,}"),
    lab = ifelse(is.na(O), 
                 lab,
                 paste0(var, " : ", qcmlabel))) %>% 
  select(variable = var, varlabel = lab, type)

lmod <- separate(lmod, col = lab, into = c("variable", "type"), sep = "\\s", remove=FALSE) %>% 
  select(-lab) %>% 
  left_join(lvar)

lvar <- filter(lmod, str_detect(type, "M")) %>% 
  mutate(variable = paste0(variable, "_M", L),
         varlabel = paste0(variable, " : ", modlabel)) %>% 
  select(variable, varlabel, type) %>% 
  bind_rows(lvar, .)

lvar <- mutate(lvar, varlabel = str_wrap(varlabel, width = 50))

labs_pc08 <- lvar
mods_pc08 <- lmod
rm(lvar, lmod)


source(here("02_import","recodage_PC18.R"))
source(here("02_import","recodage_PC08.R"))


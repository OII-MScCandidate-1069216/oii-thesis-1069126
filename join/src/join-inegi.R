# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2023, Data Cívica GPL v2 or later
# ===========================================================
# 

# Script 1 

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, readxl, tidyverse, janitor, lubridate, stringr, stringi, fuzzyjoin)


### FUNCTIONS
clean_text <- function(s){
  str_squish(toupper(stri_trans_general(s, "latin-ascii")))
}



### cols to keep 
keep_cols <- c("IDUNICO", "EXPEDIENTE", "STATUS", "FECHA_REPORTE",
               "FISCALIA", "CATEGORIA", "SEXO_D", "EDAD_D", "NACIONALIDAD_D", "TALLA_D",
               "ESTADO_CIVIL_D", "ESCOLARIDAD_D", "OCUPACION_D", "ALGUNA_VEZ_ARRAIGADO_D",
               "FECHA_EVENTO", "MUNICIPIO_EVENTO", "ENTIDAD_EVENTO", "IBA_SOLO", "LOCALIZADO_FECHA", 
               "LUGAR_LOCALIZADO", "ESTADO_LOCALIZADO", "MUNICIPIO_LOCALIZADO",
               "PAIS_LOCALIZADO", "LOCALIZADO_VIVO_MUERTO", "OBSERV_LOCALIZACION",
               "OBSERV_DESAP")



# Data --------------------------------------------------------------------
cobupem_main <- read_excel(here("import", "input", "EXPEDIENTE.xlsx")) %>% 
  remove_empty(which = "cols") %>% 
  select(IDUNICO, EXPEDIENTE, MUNICIPIO_EVENTO) %>% 
  mutate(
    IDUNICO = as.character(IDUNICO),
    MUNICIPIO_EVENTO = replace_na(MUNICIPIO_EVENTO, "UNKNOWN"))  %>% 
  distinct(IDUNICO, .keep_all = TRUE)


geo_catalog <- read_csv(here("join", "input", "geo_catalog.csv")) %>% 
  filter(name_ent == "México") %>% 
  select(-name_ent_short) %>% 
  mutate(inegi = paste0(id_ent, id_mun),
         name_mun = clean_text(name_mun))   



tempo <- stringdist_left_join(cobupem_main, geo_catalog,
                     by = c("MUNICIPIO_EVENTO" = "name_mun"),
                     max_dist = 5,
                     method = "soundex") %>% 
  select(-id_mun) %>% 
  distinct(IDUNICO, .keep_all = TRUE)


write_csv(tempo, here("join", "output", "fuzzy-hand.csv"))

#DONE. 








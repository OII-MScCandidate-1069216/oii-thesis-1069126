# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2023, Data Cívica GPL v2 or later
# ===========================================================
# 

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, DataExplorer, readxl, tidyverse, janitor, lubridate,
               openxlsx, survival, ggsurvfit, tidycmprsk,
               condsurv, survminer, naniar, stringi)


### cols to keep 
keep_cols <- c("IDUNICO", "EXPEDIENTE", "STATUS", "FISCALIA", 
               "CATEGORIA", "SEXO_D", "EDAD_D", "TALLA_D",
               "ESTADO_CIVIL_D", "ESCOLARIDAD_D",
               "FECHA_EVENTO",  "IBA_SOLO", "LOCALIZADO_FECHA", 
               "LOCALIZADO_VIVO_MUERTO", "OBSERV_LOCALIZACION",
               "OBSERV_DESAP", "LUGAR_LOCALIZADO", "MUNICIPIO_EVENTO")



# Limpiar estados both  

# Data --------------------------------------------------------------------
source(here("import", "src", "lists.r"))

cobupem_main <- read_excel(here("import", "input", "EXPEDIENTE.xlsx"))


# Clean  ------------------------------------------------------------------
tempo <- cobupem_main %>% 
  remove_empty(which = "cols") %>% 
  select(all_of(keep_cols)) %>% 
  mutate(
    IDUNICO = as.character(IDUNICO),
    FECHA_EVENTO = convert_to_date(FECHA_EVENTO),
    FECHA_EVENTO = ymd(FECHA_EVENTO),
    YEAR_EVENTO = year(FECHA_EVENTO),
    LOCALIZADO_FECHA = ymd(LOCALIZADO_FECHA),
    YEAR_LOCALIZADO = year(LOCALIZADO_FECHA),
    DIFF_DIAS = difftime(LOCALIZADO_FECHA, FECHA_EVENTO, units = "days"),
    DIFF_DIAS = as.numeric(DIFF_DIAS),
    FISCALIA = case_when(
      FISCALIA %in% c(TOLUCA) ~ "TOLUCA", 
      FISCALIA %in% c(TLALNEPANTLA) ~ "TLALNEPANTLA", 
      FISCALIA %in% c(NEZAHUALCOYOTL) ~ "NEZAHUALCOYOTL", 
      FISCALIA %in% c(ECATEPEC) ~ "ECATEPEC", 
      FISCALIA %in% c(AMECAMECA) ~ "AMECAMECA", 
      FISCALIA %in% c(CUAUTITLAN ) ~ "CUAUTITLAN", 
      FISCALIA %in% c(TEXCOCO) ~ "TEXCOCO", 
      FISCALIA %in% c(ATLACOMULCO) ~ "ATLACOMULCO", 
      FISCALIA %in% c(IXTAPAN_DE_LA_SAL) ~ "IXTAPAN_DE_LA_SAL",
      FISCALIA %in% c(NAUCALPAN) ~ "NAUCALPAN",
      FISCALIA %in% c(TEJUPILCO) ~ "TEJUPILCO",
      FISCALIA %in% c(VALLE_DE_BRAVO) ~ "VALLE_DE_BRAVO",
      FISCALIA %in% c(CHALCO) ~ "CHALCO",
      FISCALIA %in% c(TECAMAC) ~ "TECAMAC",
      FISCALIA %in% c(CHIMALHUACAN) ~ "CHIMALHUACAN",
      FISCALIA %in% c(IXTAPALUCA) ~ "IXTAPALUCA",
      FISCALIA %in% c(TULTITLAN) ~ "TULTITLAN",
      FISCALIA %in% c(ATIZAPAN) ~ "ATIZAPAN",
      FISCALIA %in% c(NICOLAS_ROMERO) ~ "NICOLAS_ROMERO",
      FISCALIA %in% c(JILOTEPEC) ~ "JILOTEPEC",
      FISCALIA %in% c(COACALCO) ~ "COACALCO",
      FISCALIA %in% c(NO_EDOMEX) ~ "NO_EDOMEX",
      FISCALIA %in% c(IXTLAHUACA) ~ "IXTLAHUACA",
      !is.na(FISCALIA) ~ "OTRA_FISCALIA",
      T ~ FISCALIA 
      ),
    SEXO_D = case_when(
    SEXO_D == "FEM TRANS" ~ "FEMENINO",
    SEXO_D == "M" ~ "MASCULINO",
    SEXO_D == "MASCULINOO" ~ "MASCULINO",
    T ~ SEXO_D
  ),
  LOCALIZADO_VIVO_MUERTO = replace_na(LOCALIZADO_VIVO_MUERTO, "SIGUE DESAPARECIDA"),
  CATEGORIA = case_when(
    CATEGORIA %in% c(VOLUNTARIA) ~ "VOLUNTARIA",
    CATEGORIA %in% c(INVOLUNTARIA) ~ "INVOLUNTARIA",
    CATEGORIA %in% c(COMPLEJA) ~ "COMPLEJA",
    CATEGORIA %in% c(NO_ESPECIFICADO_CAT) ~ "NO ESPECIFICADO",
    T ~ CATEGORIA
    ),
  EDAD_D = as.integer(EDAD_D),
  IBA_SOLO = case_when(
    IBA_SOLO %in% c(".", "4", "DE", "MI", "MO", "NP", "NS", "S", "SE", "SN", "YA") ~ NA,
    IBA_SOLO == "N" ~ "NO",
    T ~ IBA_SOLO),
  ESTADO_CIVIL_D  = case_when(
    ESTADO_CIVIL_D %in% c(soltero) ~ "SOLTERO",
    ESTADO_CIVIL_D %in% c(pareja) ~ "PAREJA",
    ESTADO_CIVIL_D %in% c(divorciado) ~ "DIVORCIADO",
    ESTADO_CIVIL_D %in% c(viudo) ~ "VIUDO",
    !is.na(ESTADO_CIVIL_D) ~ "SIN_DAT0",
    T ~ ESTADO_CIVIL_D), 
  ESCOLARIDAD_D = case_when(
    ESCOLARIDAD_D %in% c(preescolar) ~ "PREESCOLAR",
    ESCOLARIDAD_D %in% c(primaria) ~ "PRIMARIA",
    ESCOLARIDAD_D %in% c(secundaria) ~ "SECUNDARIA",
    ESCOLARIDAD_D %in% c(preparatoria) ~ "PREPARATORIA",
    ESCOLARIDAD_D %in% c(tecnica) ~ "TECNICA",
    ESCOLARIDAD_D %in% c(universidad) ~ "UNIVERSIDAD",
    ESCOLARIDAD_D %in% c(sin_escolaridad) ~ "SIN_ESCOLARIDAD",
    ESCOLARIDAD_D %in% c(otra_escolaridad ) ~ "OTRA_ESCOLARIDAD",
    !is.na(ESCOLARIDAD_D) ~ "SIN_DATO",
    T ~ ESCOLARIDAD_D)
  ) %>% 
    unite(col = "TEXT",  c("OBSERV_LOCALIZACION",
               "OBSERV_DESAP", "LUGAR_LOCALIZADO"
               )
          ) %>% 
  select(everything(), YEAR_EVENTO) %>% 
  distinct(IDUNICO, .keep_all = TRUE)
  


# base_line <- tempo %>% select(LOCALIZADO_VIVO_MUERTO, SEXO_D, EDAD_D, 
                              # CATEGORIA, YEAR_EVENTO, DIFF_DIAS)


# base_line_fecha <- tempo %>% select(LOCALIZADO_VIVO_MUERTO, SEXO_D, EDAD_D, 
                              #CATEGORIA, YEAR_EVENTO, DIFF_DIAS, FECHA_EVENTO)
  
#write_rds(base_line, here("import", "output", "baseline.rds"))
#write_rds(base_line_fecha, here("import", "output", "baseline_date.rds"))

write_rds(tempo, here("import", "output", "clean_cobupem.rds"))

### report 
#create_report(tempo)


# DONE. 
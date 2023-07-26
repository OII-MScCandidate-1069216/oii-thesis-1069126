# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2023, Data Cívica GPL v2 or later
# ===========================================================
# 


# Script 4 

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, tidyverse)



# data --------------------------------------------------------------------
cobupem_clean <- read_rds(here("join", "output", "cobupem_clean.rds"))

municipios_mexico <- read_csv(here("join", "input", "mexico-municipios-final.csv")) %>% 
  select(-log_pop_mx, -pct_urban_mx, -pct_obesity_lq, -pct_hypertension_lq, -pct_diabetes_lq,
         -healthcare_expenditure_19, -elxn_soon_mx, -irag_rate_mun, -inf_rate_mun, 
         -healthcare_expenditure_20, -hospBeds_rate_mun)

fosas_results <- read_rds(here("join", "input", "base_modelo2018.rds")) %>% 
  select(inegi, fisc_pred, prensa_pred)

full_text <- read_rds(here("join", "output", "full_text.rds")) %>% 
  ungroup()


# Join --------------------------------------------------------------------
cobupem_clean_final <- left_join(cobupem_clean, municipios_mexico) %>% 
  left_join(., fosas_results) %>% 
  janitor::clean_names() 

cobupem_clean_final <- left_join(cobupem_clean_final, 
                                 full_text, 
                                 by = c("idunico" = "ID_RELACIONADO")) %>% 
  unite(col = "text_cobupem", c("text", "all_text"))



write_rds(cobupem_clean_final, here("join", "output", "cobupem_clean_final.rds"))


# Done. 
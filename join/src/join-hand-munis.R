# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2023, Data Cívica GPL v2 or later
# ===========================================================
# 

# Script 2 

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, tidyverse, readxl, stringr)



# Read data ---------------------------------------------------------------
cobupem_clean <- read_rds(here("import", "output", "clean_cobupem.rds"))

hand_inegi <- read_excel(here("join", "output", "fuzzy-hand.xlsx")) %>% 
  select(IDUNICO, EXPEDIENTE, inegi) %>% 
  mutate(IDUNICO = as.character(IDUNICO))



# Join --------------------------------------------------------------------
cobupem_clean_final <- left_join(cobupem_clean, hand_inegi) %>% 
  mutate(inegi = gsub('\\s+', '', inegi),
         inegi = na_if(inegi, "NA"))

write_rds(cobupem_clean_final, here("join", "output", "cobupem_clean.rds"))


# Done. 
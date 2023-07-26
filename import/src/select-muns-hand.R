# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2023, Data Cívica GPL v2 or later
# ===========================================================
# 

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, readxl, tidyverse, janitor, lubridate)


cobupem_main <- read_excel(here("import", "input", "EXPEDIENTE.xlsx")) %>% 
  select(IDUNICO, MUNICIPIO_EVENTO)


write_csv(cobupem_main, here("import", "output", "muns-hand.csv"))


# DONE. 
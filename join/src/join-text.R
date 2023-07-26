# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2023, Data Cívica GPL v2 or later
# ===========================================================
# 

# Script 3 

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, tidyverse, readxl, stringr)


# Nota informativa: ID_RELACIONADO, CONTENIDO (TEXTO)
# BUSQUEDA_CAMPO: ID_RELACIONADO, ANTECEDENTES, ESTADO_DE_FUERZA, ACCIONES, RESULTADOS
# PLAN_BUSQUEDA: ID_RELACIONADO, OBSERVACIONES
# SEGUIMIENTO_CAB: ID_RELACIONADO, INFORMACION 


# Read data and preprare data --------------------------------------------------------------

### nota 
nota_informativa <- read_excel(here("import", "input", "NOTA_INFORMATIVA.xlsx")) %>% 
  select(ID_RELACIONADO, CONTENIDO)  %>% 
  mutate(ID_RELACIONADO = as.character(ID_RELACIONADO)) %>% 
  group_by(ID_RELACIONADO) %>% 
  mutate(scaleno = row_number()) %>% 
  pivot_wider(names_from = scaleno, values_from = CONTENIDO) %>% 
  unite(col = "TEXT2", c(2:19))


### campo  
busqueda_campo <- read_excel(here("import", "input", "BUSQUEDA_CAMPO.xlsx")) %>% 
  select(ID_RELACIONADO, ANTECEDENTES, ESTADO_DE_FUERZA, ACCIONES,
         RESULTADOS) %>% 
  mutate(ID_RELACIONADO = as.character(ID_RELACIONADO)) %>%   
  group_by(ID_RELACIONADO) %>% 
  mutate(scaleno = row_number()) %>% 
  pivot_wider(names_from = scaleno, values_from = c(ANTECEDENTES, ESTADO_DE_FUERZA, ACCIONES,
                                                    RESULTADOS)) %>% 
  unite(col = "TEXT3", c(2:53))


### plan búsqueda 
plan_busqueda <- read_excel(here("import", "input", "PLAN_BUSQUEDA.xlsx")) %>% 
  select(ID_RELACIONADO, OBSERVACIONES) %>% 
  mutate(ID_RELACIONADO = as.character(ID_RELACIONADO)) %>%   
  group_by(ID_RELACIONADO) %>% 
  mutate(scaleno = row_number()) %>% 
  pivot_wider(names_from = scaleno, values_from = OBSERVACIONES) %>% 
  unite(col = "TEXT4", c(2:9))


### 
seguimiento_cab <- read_excel(here("import", "input", "SEGUIMIENTO_CAB.xlsx")) %>% 
  select(ID_RELACIONADO, INFORMACION) %>% 
  mutate(ID_RELACIONADO = as.character(ID_RELACIONADO)) %>% 
  group_by(ID_RELACIONADO) %>% 
  mutate(scaleno = row_number()) %>% 
  pivot_wider(names_from = scaleno, values_from = INFORMACION) %>% 
  unite(col = "TEXT1", c(2:50))


# Join --------------------------------------------------------------------
text_df <- full_join(seguimiento_cab, plan_busqueda) %>% 
  full_join(., nota_informativa) %>% 
  full_join(., busqueda_campo) %>% 
  unite(col = "all_text", c(2:5))

write_rds(text_df, here("join", "output", "full_text.rds"))

# DONE. 



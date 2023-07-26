# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2023, Data Cívica GPL v2 or later
# ===========================================================
# 

# Impute with miss forest 


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, tidyverse, missForest, beepr)


keep_vars <- c("idunico", "expediente", "municipio_evento", 
               "text_cobupem", "inegi", "fecha_evento",
               "localizado_fecha", "year_localizado", "fiscalia", "status")


# Read data ---------------------------------------------------------------
cobupem_data <- read_rds(here("join", "output", "cobupem_clean_final.rds")) 


### sep imputation data 
impute_vars <- cobupem_data %>% 
  select(-one_of(keep_vars)) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, as.integer) %>% 
  mutate(edad_d = as.numeric(edad_d))%>% 
  as.data.frame() 


id_vars <- cobupem_data %>% 
  select(all_of(keep_vars))



# Impute ------------------------------------------------------------------
doParallel::registerDoParallel(cores = 4) # set based on number of CPU cores
doRNG::registerDoRNG(seed = 123)

#imputed_X <- missForest(X, parallelize = 'forests')$ximp

X_imp <- missForest::missForest(impute_vars, 
                                cutoff = list(c(1, 2, 3, 4),
                                              c(1,2), 1, 1, 
                                              c(1,2,3,4,5), 
                                              c(1,2,3,4,5,6,7,8,9),
                                              c(1,2), c(1,2,3), #8 
                                              1, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1,
                                              1, 1, 1, 1, c(1,2),
                                              c(1,2), c(1,2), c(1,2), 1, 1,
                                              1),
                                parallelize = 'forests')$ximp
beepr::beep(sound = 5)



# Save df imputed ---------------------------------------------------------
cobupem_imputed <- cbind(id_vars, X_imp)


write_rds(cobupem_imputed, here("impute", "output", "cobupem_imputed_forest.rds"))




# Done. 
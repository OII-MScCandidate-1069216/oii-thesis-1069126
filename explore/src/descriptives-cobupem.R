# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2023, Data Cívica GPL v2 or later
# ===========================================================
# 



# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, tidyverse, svglite, datapasta,
               survival, ggsurvfit,  condsurv, survminer, sf,
               patchwork)




# Files -------------------------------------------------------------------
files <- list(fiebre_desp = here("explore", "output", "fiebre_desp."),
              fiebre_desp_genero = here("explore", "output", "fiebre_desp_genero."),
              perfiles_desp = here("explore", "output", "perfiles_desp."),
              survival_plot = here("explore", "output", "survival_plot."),
              map_plot = here("explore", "output", "map_plot."),
              map_plot_gender = here("explore", "output", "map_plot_gender."))




devices <- c("png", "svg")


# Load data ---------------------------------------------------------------
cobupem_data <- read_rds(here("join", "output", "cobupem_clean_final.rds")) %>%
  filter(year_evento >= 2006 & year_evento <= 2022)


pop_edomex <- read_csv(here("explore", "input", "pop-edomex.csv")) %>% 
  mutate(clave = str_pad(clave, 3, pad = "0"),
         clave = as.character(clave),
         estado_clave = "15",
         inegi = paste0(estado_clave, clave))


shp_edomex <- st_read(here("explore", "input", "Municipios_Mx.shp")) %>% 
  filter(CVE_ENT == 15)



# Fiebres -----------------------------------------------------------------
cobupem_data %>% 
  filter(year_evento != 2022) %>% 
  group_by(year_evento) %>% 
  count(year_evento) %>% 
  ggplot(aes(year_evento, n)) +
  geom_line(size = 1.2, colour = "#0F7173") +
  geom_label(aes(label=n), family="Courier New") +
  scale_x_continuous(breaks = seq(from=min(cobupem_data$year_evento), to=max(cobupem_data$year_evento), by=3)) +
  guides(colour = guide_legend(title = NULL)) +
  theme_minimal(base_family = "Courier New") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(face = "bold"),
        legend.position = "top") +
  labs(title = "Cumulative cases of missing persons in Estado de México",
       subtitle = "2006-2021",
       y = NULL, x = NULL)


walk(devices, ~ ggsave(filename = file.path(paste0(files$fiebre_desp, .x)),
                       device = .x, width = 10, height = 8))


cobupem_data %>% 
  filter(year_evento != 2022) %>% 
  group_by(year_evento, sexo_d) %>% 
  count(year_evento) %>% 
  ggplot(aes(year_evento, n, colour = sexo_d)) +
  geom_line(size = 1.4) +
  geom_label(aes(label=n), family="Courier New", colour = "black") +
  facet_wrap(~ sexo_d) +
  scale_color_manual(values= c("#F8E16C", "#0F7173"),
                     labels= c("Women", "Men")) +
  scale_x_continuous(breaks = seq(from=min(cobupem_data$year_evento), to=max(cobupem_data$year_evento), by=3)) +
  guides(colour = guide_legend(title = NULL)) +
  theme_minimal(base_family = "Courier New") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(face = "bold"),
        legend.position = "top") +
  labs(title = "Cumulative cases of missing persons in Estado de México by gender",
       subtitle = "2006-2021",
       y = NULL, x = NULL)


walk(devices, ~ ggsave(filename = file.path(paste0(files$fiebre_desp_genero, .x)),
                       device = .x, width = 10, height = 8))





# Boxplots ----------------------------------------------------------------

### Sexo 
cobupem_data %>% 
  group_by(sexo_d) %>% 
  count()


### categoria 
cobupem_data %>% 
  group_by(categoria) %>% 
 # na.omit() %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))


### Sexo-categoria
cobupem_data %>% 
  group_by(sexo_d,categoria) %>% 
  na.omit() %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

# sexo_d    categoria           n rel.freq
# 1 FEMENINO  COMPLEJA         1607 47%     
# 2 FEMENINO  INVOLUNTARIA      359 10%     
# 3 FEMENINO  NO ESPECIFICADO   687 20%     
# 4 FEMENINO  VOLUNTARIA        773 23%     
# 5 MASCULINO COMPLEJA          877 34%     
# 6 MASCULINO INVOLUNTARIA      292 11%     
# 7 MASCULINO NO ESPECIFICADO   483 19%     
# 8 MASCULINO VOLUNTARIA        946 36%  


### sexo-edad
cobupem_data %>% 
  group_by(sexo_d) %>% 
  summarise(promedio_edad = mean(edad_d, na.rm = T),
            mediana_edad = median(edad_d, na.rm = T))


# sexo_d    promedio_edad mediana_edad
# FEMENINO           20.0           16
# MASCULINO          30.4           27


### sexo-edad-estatus 
cobupem_data %>% 
  group_by(sexo_d, localizado_vivo_muerto) %>% 
  summarise(promedio_edad = mean(edad_d, na.rm = T),
            mediana_edad = median(edad_d, na.rm = T))



### sexo-estatus 
cobupem_data %>% 
  filter(edad_d <= 100) %>% 
  ggplot(aes(sexo_d, edad_d, fill = localizado_vivo_muerto)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(face = "bold"),
        legend.position = "top") +
  labs(x = "GENDER", y = "AGE", 
       title = "Distribution of missing persons in Estado de México",
       subtitle = "Based on the person's status",
       fill = "Status") + 
  scale_fill_manual(name = "Status", 
                      values = c("#E15554", "#F8E16C", "#0F7173"),
                      labels = c("Dead", "Missing", "Alive"))



walk(devices, ~ ggsave(filename = file.path(paste0(files$perfiles_desp, .x)),
                       device = .x, width = 10, height = 8))




# Survival  ---------------------------------------------------------------
cobupem_data %>% 
  select(sexo_d, localizado_vivo_muerto, diff_dias) %>% 
  filter(localizado_vivo_muerto != "SIGUE DESAPARECIDA" & diff_dias >= 0 &
           diff_dias < 32929) %>% 
  summarise(mean_days = mean(diff_dias, na.rm = T),
            median_days = median(diff_dias, na.rm = T))

cobupem_data %>% 
  select(sexo_d, localizado_vivo_muerto, diff_dias) %>% 
  filter(localizado_vivo_muerto != "SIGUE DESAPARECIDA" & diff_dias >= 0 &
           diff_dias < 32929) %>% 
  group_by(localizado_vivo_muerto) %>% 
  summarise(mean_days = mean(diff_dias, na.rm = T),
            median_days = median(diff_dias, na.rm = T))


cobupem_data %>% 
  select(sexo_d, localizado_vivo_muerto, diff_dias) %>% 
  filter(localizado_vivo_muerto != "SIGUE DESAPARECIDA" & diff_dias >= 0 &
           diff_dias < 32929) %>% 
  group_by(sexo_d, localizado_vivo_muerto) %>% 
  summarise(mean_days = mean(diff_dias, na.rm = T),
            median_days = median(diff_dias, na.rm = T))


survival_df <- cobupem_data %>% 
  select(sexo_d, localizado_vivo_muerto, diff_dias) %>% 
  filter(localizado_vivo_muerto != "SIGUE DESAPARECIDA" & diff_dias >= 0 &
           diff_dias < 32929) %>% 
  mutate(status = recode(localizado_vivo_muerto, "VIVO" = 0, "MUERTO" = 1))



Surv(survival_df$diff_dias, survival_df$status)[1:10]

s1 <- survfit(Surv(diff_dias, status) ~ 1, data = survival_df)
summary(s1)
s1

ggsurvplot(s1,
           conf.int = FALSE,
           surv.median.line = "hv",
           legend = "none"
)



### GENDER 
linelistsurv_fit_sex <-  survfit(Surv(diff_dias, status) ~ sexo_d,
                                 data = survival_df)
linelistsurv_fit_sex


survfit2(Surv(diff_dias, status) ~ sexo_d, data = survival_df) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability",
    title = "Estimated probability of locating person alive after #days",
    subtitle = "By gender"
  ) + 
  add_confidence_interval() +
  add_risktable()


### gender 2 
ggsurvplot(linelistsurv_fit_sex,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#F8E16C", "#0F7173")) +
  labs(
    x = "Days",
    y = "Overall survival probability",
    title = "Estimated probability of locating person alive after #days",
    subtitle = "By gender"
  ) 


walk(devices, ~ ggsave(filename = file.path(paste0(files$survival_plot, .x)),
                       device = .x, width = 10, height = 8))




# Mapas  ------------------------------------------------------------------
desp_munis_cobupem <- cobupem_data %>% 
  select(inegi, sexo_d) %>%
  group_by(inegi) %>% 
  count(sort = T) %>% 
  right_join(pop_edomex, by = c("inegi")) %>% 
  mutate(tasa_desp = n*(100000/total_habitantes),
         tasa_desp_round = round(tasa_desp, 2)) %>% 
  select(inegi, nombre, n, tasa_desp, tasa_desp_round)


mapa_edomex <- left_join(shp_edomex, desp_munis_cobupem, by = c("CVEGEO" = "inegi"))



centroides <- st_coordinates(st_centroid(mapa_edomex)) %>% as.tibble()



# totales
mapa_edomex %>% 
  bind_cols(centroides) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = tasa_desp), size = 0.1, color = "black",
          lwd = 0) +
# geom_text(aes(X, Y, label = NOMGEO), size = 2) +
  scale_fill_continuous(low = "#F8E16C", high = "#0F7173", na.value = "white", name = "Per 100k inhabitants") +
  labs(title = "Distribution of missing persons by municipality in \n Estado de México",
       subtitle = "2006-2022") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10),
        legend.position = "right")


walk(devices, ~ ggsave(filename = file.path(paste0(files$map_plot, .x)),
                       device = .x, width = 10, height = 8))



### por sexo 
desp_munis_genero <- cobupem_data %>% 
  select(inegi, sexo_d) %>%
  group_by(inegi, sexo_d) %>% 
  count() %>% 
  pivot_wider(names_from = sexo_d, values_from = n) %>% 
  right_join(pop_edomex, by = c("inegi")) %>% 
  mutate(tasa_desp_mujeres = FEMENINO*(100000/total_mujeres),
         tasa_desp_hombres = MASCULINO*(100000/total_hombres),
         tasa_hombres_round = round(tasa_desp_hombres, 2),
         tasa_mujeres_round = round(tasa_desp_mujeres, 2)) %>% 
  select(inegi, nombre, tasa_hombres_round, tasa_mujeres_round)



mapa_edomex_genero <- left_join(shp_edomex, desp_munis_genero, by = c("CVEGEO" = "inegi"))



mapa_mujeres <- mapa_edomex_genero %>% 
  bind_cols(centroides) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = tasa_desp_mujeres), size = 0.1, color = "black",
          lwd = 0) +
  # geom_text(aes(X, Y, label = NOMGEO), size = 2) +
  scale_fill_continuous(low = "#F8E16C", high = "#0F7173", na.value = "white", name = "Per 100k inhabitants") +
  labs(title = "Distribution of missing women by municipality in \n Estado de México",
       subtitle = "2006-2022") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10),
        legend.position = "right")



mapa_hombres <- mapa_edomex_genero %>% 
  bind_cols(centroides) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = tasa_desp_hombres), size = 0.1, color = "black",
          lwd = 0) +
  # geom_text(aes(X, Y, label = NOMGEO), size = 2) +
  scale_fill_continuous(low = "#F8E16C", high = "#0F7173", na.value = "white", name = "Per 100k inhabitants") +
  labs(title = "Distribution of missing men by municipality in \n Estado de México",
       subtitle = "2006-2022") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10),
        legend.position = "right")



mapa_mujeres + mapa_hombres
walk(devices, ~ ggsave(filename = file.path(paste0(files$map_plot_gender, .x)),
                       device = .x, width = 12, height = 8))


# DONE. 














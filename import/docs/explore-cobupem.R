# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2022, Data Cívica GPL v2 or later
# ===========================================================
# 

# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(here, DataExplorer, readxl, dplyr)


# Data --------------------------------------------------------------------
cobupem_main <- read_excel(here("import", "input", "EXPEDIENTE.xlsx"))


### na plot 
cobupem_main %>% 
  select(1:50) %>% 
plot_missing(ggtheme = theme_minimal())


cobupem_main %>% 
  select(51:100) %>% 
  plot_missing(ggtheme = theme_minimal())


cobupem_main %>% 
  select(101:150) %>% 
  plot_missing(ggtheme = theme_minimal())

cobupem_main %>% 
  select(151:200) %>% 
  plot_missing(ggtheme = theme_minimal())

cobupem_main %>% 
  select(151:200) %>% 
  plot_missing(ggtheme = theme_minimal())

cobupem_main %>% 
  select(201:250) %>% 
  plot_missing(ggtheme = theme_minimal())

cobupem_main %>% 
  select(251:276) %>% 
  plot_missing(ggtheme = theme_minimal())

# Reporte  ----------------------------------------------------------------
create_report(cobupem_main, y = "LOCALIZADO_VIVO_MUERTO")


# Notas de los datos  -----------------------------------------------------
# Se descartan para uso: BUSQUEDA_CAMPOS_ADJUNTOS, BUSQUEDA_CAMPO_LUGARES, DIFUSION_ADJUNTOS
# DIFUSION_DETALLES, DIFUSION, LOCALIZACION_ADJUNTOS, LOCALIZACION_DETALLES, 
# LOCALIZACION (PODRÍA SERVIR, PERO SOLO TIENE 10 OBSERVACIONES), RASTREO_CAB

#SOL_BUS_BD se podría usar pero tal vez no la pasaron bien. 

# Se quedan para uso: 
# BUSQUEDA_CAMPO:
# NOTA_INFORMATIVA: Contiene reportes de diferentes acciones o actualizaciones de información, 
# por parte del personal de la COBUPEM. Hay 2000 mil notas informativas. 
# PLAN_BUSQUEDA: Contiene información sobre acciones en campo o sobre llamadas a familiares que,
# actualizan la info de localziación de la persona desaparecida. 
# RASTREO_CPO: Contiene información de seguimiento por parte de diferentes dependencias de
# EDOMEX al caso, que después son notificadas a COBUPEM. 
# SEGUIMIENTO_CAB: Es el seguimiento que le dan al caso con los familaires de la persona,
# desaparecida. Probablemente la que tiene más info del caso. 


# Done. 
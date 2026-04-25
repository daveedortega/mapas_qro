## Revisión de Mapitas de Secciones Electorales
# DAOA
# 24/04/2026

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, sf)
rm(list = ls())
dev.off()

# Cargar Datos ------------------------------------------------------------

# Elecciones 2024: https://ieeq.mx/contenido/elecciones/2023_2024/resultados/#/home/dip/entidad-map

qro_ayun_24 <- read_csv('input/queretaro/resultados_24/IEEQ_AYUN_QRO/QRO_AYUN_RESULTADOS_2024.csv') %>% clean_names()
qro_dip_24 <- read_csv('input/queretaro/resultados_24/IEEQ_DIP_QRO/QRO_DIP_LOC_RESULTADOS_2024.csv')%>% clean_names()

# Elecciones 21

excel_sheets('input/queretaro/resultados_21/2021_Ayuntamiento.xlsx')
qro_ayun_21 <- read_xlsx('input/queretaro/resultados_21/2021_Ayuntamiento.xlsx', skip = 1, sheet = 2) %>% clean_names()

excel_sheets('input/queretaro/resultados_21/2021_DIPUTACIÓNMR.xlsx')
qro_dip_mr_21 <- read_xlsx('input/queretaro/resultados_21/2021_DIPUTACIÓNMR.xlsx', skip = 1, sheet = 3) %>% clean_names()

excel_sheets('input/queretaro/resultados_21/2021_DIPUTACIÓNRP.xlsx')
qro_dip_rp_21 <- read_xlsx('input/queretaro/resultados_21/2021_DIPUTACIÓNRP.xlsx', skip = 1, sheet = 2) %>% clean_names()

excel_sheets('input/queretaro/resultados_21/2021_Gubernatura.xlsx')
qro_dip_gob_21 <- read_xlsx('input/queretaro/resultados_21/2021_Gubernatura.xlsx', skip = 1, sheet = 6) %>% clean_names()

# Prep 18 (NO HAY RESULTADOS EN BDD)

qro_ayun_18 <- read_csv('input/queretaro/prep_18/20180702_1145_PREP_AYUN_QRO/QRO_AYUN_2018.csv', skip = 5) %>% clean_names()
qro_dip_18 <- read_csv('input/queretaro/prep_18/20180702_1145_PREP_DIP_LOC_QRO/QRO_DIP_LOC_2018.csv', skip = 5) %>% clean_names()

# Cargar Mapas ------------------------------------------------------------

# Mapas de secciones electorales: HANDMADE

# read_sf('input/queretaro/cartografia_electoral/SECCION.shp') %>% as_tibble() %>% count(seccion) 1090???
# queretaro <- read_sf('~/Desktop/Mapas/qro_secciones_loc/qro_dto_loc.shp')
# queretaro <- queretaro %>% rename(seccion = COL5BD53_6)
# queretaro <- queretaro %>% rename(municipio = COL5BD53_5)
# queretaro <- queretaro %>% rename(dto_elect = COL5BD53_4)
# queretaro <- queretaro %>% rename(entidad = COL5BD53_2)
# queretaro %>% glimpse()
# 
# queretaro <- queretaro %>% select(Name, entidad, dto_elect, layer, municipio, seccion)
# 
# queretaro <- queretaro %>% st_make_valid()
# queretaro %>% write_sf('output/mapa_qro/dto_loc_qro_24.shp')

queretaro <- read_sf('output/mapa_qro/dto_loc_qro_24.shp')

# Datos de resultados previos: https://pel.eleccionesqro.mx/retrospectiva2025/index.php?p=base-de-datos

queretaro %>% 
  ggplot()+
  geom_sf(aes(fill = factor(municipio)))


# Transformaciones en panel legible ---------------------------------------


# Ayuntamiento 24 ---------------------------------------------------------

qro_ayun_24 %>% glimpse()

# Participacion
qro_ayun_24 %>% select(seccion, lista_nominal, total_votos) %>% 
  group_by(seccion) %>% 
  summarise(lista_nominal = sum(lista_nominal), total_votos = sum(total_votos)) %>% 
  summarise(lista_nominal = sum(lista_nominal, na.rm = T), 
            total_votos = sum(total_votos, na.rm = T)) %>% mutate(total_votos/lista_nominal*100)

cs_ayun_24 <- qro_ayun_24 %>% select(seccion, 10:37) %>% 
  pivot_longer(cols = 2:27, names_to = 'partido', values_to = 'votos') %>% 
  select(-total_votos, -lista_nominal)

# 1080 acá, raro
queretaro %>% as_tibble() %>% count(seccion)
# 955 secciones?
cs_ayun_24 <- cs_ayun_24 %>% mutate(seccion = as.numeric(seccion))

# Coaliciones
coalicion_qro24 <- c('morena', 'pt', 'pvem',
                     'morena_pt', 'pvem_morena', 'pvem_pt', 'pvem_morena_pt')

cs_ayun_24 %>% count(partido) %>% as.data.frame()

cs_ayun_24 <- cs_ayun_24 %>% 
  mutate(juntos_haremos_historia = ifelse(partido %in% coalicion_qro24, 'Juntos Haremos Historia', 'Otro')) 

# Diputados 24 ------------------------------------------------------------

# Diputados 24
cs_dip_24 <- qro_dip_24 %>% select(seccion, 10:37) %>% 
  pivot_longer(cols = 2:27, names_to = 'partido', values_to = 'votos') %>% select(-lista_nominal, -total_votos)

# 954?
cs_dip_24 <- cs_dip_24 %>% mutate(seccion = as.numeric(seccion))

# Coaliciones
coalicion_qro24 <- c('morena', 'pt', 'pvem',
                     'morena_pt', 'pvem_morena', 'pvem_pt', 'pvem_morena_pt')

cs_dip_24 %>% count(partido) %>% as.data.frame()

cs_dip_24 <- cs_dip_24 %>% 
  mutate(juntos_haremos_historia = ifelse(partido %in% coalicion_qro24, 'Juntos Haremos Historia', 'Otro')) 

# Mapas 24 ----------------------------------------------------------------

cs_ayun_24 %>% filter(juntos_haremos_historia == 'Otro') %>% count(partido)

cs_ayun_24 <- cs_ayun_24 %>% group_by(seccion, juntos_haremos_historia) %>% 
  summarise(votos = sum(votos)) %>% 
  left_join(qro_ayun_24 %>% group_by(seccion = as.numeric(seccion)) %>% 
              summarise(lista_nominal = sum(lista_nominal), total_votos = sum(total_votos)))

cs_dip_24 <- cs_dip_24 %>% group_by(seccion, juntos_haremos_historia) %>% 
  summarise(votos = sum(votos)) %>% 
  left_join(qro_dip_24 %>% group_by(seccion = as.numeric(seccion)) %>% 
              summarise(lista_nominal = sum(lista_nominal), total_votos = sum(total_votos)))

# Mapa Ayuntamiento -------------------------------------------------------

map_data <- cs_ayun_24 %>% filter(juntos_haremos_historia!='Otro') %>% mutate(porcentaje = votos/total_votos*100)
# 954???
map_data 
#1007? Fucked up and idk why
mapped_data <- queretaro %>% 
  mutate(seccion = as.numeric(seccion)) %>% 
  left_join(map_data) 

mapped_data <- mapped_data %>% mutate(porcentaje = round(porcentaje, digits = 1))

library(leaflet)
library(htmltools)

bins <- c(0, 10, 20, 30,50,60, 80, 100, Inf)
pal <- colorBin("YlOrRd", domain = mapped_data$porcentaje, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/> Votos: %g <br/>Porcentaje: %g %%",
  mapped_data$Name, mapped_data$votos, mapped_data$porcentaje
) %>% lapply(htmltools::HTML)


leaflet(mapped_data) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal(porcentaje),
              weight = 0.51,
              opacity = 1,
              color = "black",
              fillOpacity = 0.6, 
              highlightOptions = highlightOptions(
                weight = 1,
                color = "blue",
                fillOpacity = 1,
                bringToFront = TRUE), 
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))


# TOP 100 por lista nominal

map_data <- cs_ayun_24 %>% filter(juntos_haremos_historia!='Otro') %>% mutate(porcentaje = votos/total_votos*100) 

# 954???
map_data 
#1007? Fucked up and idk why
mapped_data <- queretaro %>% 
  mutate(seccion = as.numeric(seccion)) %>% 
  left_join(map_data) %>% 
  filter(!is.na(porcentaje)) %>% 
  ungroup() %>% 
  slice_max(lista_nominal, n = 100)

mapped_data <- mapped_data %>% mutate(porcentaje = round(porcentaje, digits = 1))

library(leaflet)
library(htmltools)

bins <- c(0, 10, 20, 30,50,60, 80, 100, Inf)
pal <- colorBin("YlOrRd", domain = mapped_data$porcentaje, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/> Votos: %g <br/>Porcentaje: %g %%",
  mapped_data$Name, mapped_data$votos, mapped_data$porcentaje
) %>% lapply(htmltools::HTML)


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = queretaro, 
              color = 'black', weight = 0.5) %>%
  addPolygons(data = mapped_data %>% filter(!is.na(porcentaje)),
              fillColor = ~pal(porcentaje),
              weight = 0.51,
              opacity = 1,
              color = "black",
              fillOpacity = 0.6, 
              highlightOptions = highlightOptions(
                weight = 1,
                color = "blue",
                fillOpacity = 1,
                bringToFront = TRUE), 
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))


  

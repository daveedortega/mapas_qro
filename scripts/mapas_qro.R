## Revisión de Mapitas de Secciones Electorales
# DAOA
# 24/04/2026

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, sf, leaflet, htmltools)
rm(list = ls())
dev.off()
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()

# Cargar Datos ------------------------------------------------------------

# Elecciones 2024: https://ieeq.mx/contenido/elecciones/2023_2024/resultados/#/home/dip/entidad-map

qro_ayun_24 <- read_csv('input/queretaro/resultados_24/IEEQ_AYUN_QRO/') %>% clean_names()
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
qro_gob_21 <- read_xlsx('input/queretaro/resultados_21/2021_Gubernatura.xlsx', skip = 1, sheet = 6) %>% clean_names()

# https://ieeq.mx/elecciones/historico

qro_ayun_18 <- read_xlsx('input/queretaro/resultados_18/CASILLAS_AYUNTAMIENTOS.xlsx', skip = 22) %>% clean_names()
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

# queretaro <- read_sf('output/mapa_qro/dto_loc_qro_24.shp')
qro_25 <- read_sf('input/queretaro/cartografia_electoral/SECCION.shp')
qro_24 <- read_sf('output/mapa_qro/dto_loc_qro_24.shp') %>% clean_names()
qro_18 <- read_sf('~/Desktop/Mapas/qro_secciones_18/queretaro_secciones_18.shp') %>% clean_names()

qro_24 %>% glimpse()
qro_18 %>% glimpse()

qro_18 <- qro_18 %>% select(name, description = descriptio, distrito_anterior = distrito_a, distrito_l = distrito_1, 
                  municipio, seccion, layer) %>% mutate(seccion = as.numeric(seccion))

qro_24 <- qro_24 %>% mutate(seccion = as.numeric(seccion))


# Datos de resultados previos: https://pel.eleccionesqro.mx/retrospectiva2025/index.php?p=base-de-datos
# Datos de resultados previos: https://cartografia.ine.mx/sige8/productosCartograficos/bases



queretaro %>% 
  ggplot()+
  geom_sf(aes(fill = factor(municipio)))


# Transformaciones en panel legible ---------------------------------------
qro_ayun_24 %>% glimpse()
qro_ayun_18 %>% glimpse()

qro_ayun_24 %>% filter(municipio == 'Queretaro') %>% mutate(prianrd = pri+pan+prd+pan_pri_prd+pan_pri+pan_prd+pri_prd) %>% 
  summarise(prianrd = sum(prianrd), sum(total_votos))

qro_ayun_24 %>% filter(municipio == 'Queretaro') %>% mutate(jhh = morena+pt+pvem+pvem_morena+pvem_morena_pt+morena_pt+pvem_pt) %>% 
  summarise(jhh = sum(jhh), sum(total_votos))


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

# Cambios 18-24 -----------------------------------------------------------

qro_ayun_18 %>% glimpse()

qro_ayun_18 %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% 
   mutate(across(where(is_logical), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(panprdmc = pan+prd+mc+cc_pan_prd_mc+p_pan_prd+p_pan_mc+p_prd_mc+cc2_pan_prd+cc3_pan_mc, 
         jhh = morena+pt+es+morena_pt_pes+p_morena_pt+p_morena_pes+p_pt_pes) %>% 
  summarise(panprdmc = sum(panprdmc,  na.rm = T), 
            jhh = sum(jhh), 
            total_votos = sum(total_votos))%>% 
  pivot_longer(cols = 1:2, names_to = 'partido', values_to = 'votos') %>% 
  mutate(ano = 2018)
  

qro_ayun_21 %>% glimpse()

cambios_18_24 <- rbind(

  qro_ayun_18 %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% 
    mutate(across(where(is_logical), ~ifelse(is.na(.), 0, .))) %>% 
    mutate(panprdmc = pan+prd+mc+cc_pan_prd_mc+p_pan_prd+p_pan_mc+p_prd_mc+cc2_pan_prd+cc3_pan_mc, 
           jhh = morena+pt+es+morena_pt_pes+p_morena_pt+p_morena_pes+p_pt_pes) %>% 
    summarise(panprdmc = sum(panprdmc,  na.rm = T), 
              jhh = sum(jhh), 
              total_votos = sum(total_votos))%>% 
    pivot_longer(cols = 1:2, names_to = 'partido', values_to = 'votos') %>% 
    mutate(ano = 2018),
  
qro_ayun_21 %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%  
  mutate(pan_plus = pan+qi+pan_qi+prd+pan_prd+pan_prd_qi) %>% 
  summarise(pan_plus = sum(pan_plus), morena = sum(morena), total_votos = sum(total_votos)) %>% 
  pivot_longer(cols = 1:2, names_to = 'partido', values_to = 'votos') %>% mutate(ano = 2021),

qro_ayun_24  %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(prianrd = pri+pan+prd+pan_pri_prd+pan_pri+pan_prd+pri_prd, 
                      jhh = morena+pt+pvem+pvem_morena+pvem_morena_pt+morena_pt+pvem_pt) %>% 
  summarise(prianrd = sum(prianrd), jhh = sum(jhh), total_votos = sum(total_votos)) %>% 
  pivot_longer(cols = 1:2, names_to = 'partido', values_to = 'votos') %>% mutate(ano = 2024)
)

cambios_18_24 <- cambios_18_24 %>% mutate(porcentaje = votos/total_votos * 100)

cambios_18_24 %>% mutate(partido = case_when(partido == 'morena' ~ 'Morena', 
                                             partido == 'jhh' ~ 'Juntos Haremos Historia', 
                                             partido == 'prianrd' ~ 'PAN-PRI-PRD', 
                                             partido == 'pan_plus' ~ 'PAN-PRD-QI', 
                                             T ~ partido
                                             )) %>% ggplot(aes(ano, porcentaje, fill = partido))+
  geom_col(position = position_dodge())+
  geom_label(aes(label = paste0(comma(porcentaje, accuracy = 0.1), '%\n', comma(votos))), 
             position = position_dodge(width = 3), size = 6, color = 'white')+
  labs(x = '', y = '%')+
  theme_minimal()+
  scale_x_continuous(breaks = c(2018 ,2021, 2024))+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 14, color = 'black', face = 'bold'))+
  scale_fill_manual(values = c('Morena' = '#652E2D', 
                               'Juntos Haremos Historia' = 'firebrick',
                               'PAN-PRI-PRD' = '#2F3C75',
                               'PAN-PRD-QI' = 'blue4',
                               'panprdmc' = '#2F3C75'
                               ))


# Elecciones Gobernador 21 ------------------------------------------------

qro_gob_21 %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%  
  mutate(pan_plus = pan+qi+pan_qi) %>% 
  summarise(pan_plus = sum(pan_plus), morena = sum(morena), total_votos = sum(total_votos)) %>% 
  pivot_longer(cols = 1:2, names_to = 'partido', values_to = 'votos') %>% mutate(ano = 2021) %>% 
  mutate(porcentaje = votos/total_votos*100) %>% 
  ggplot(aes(partido, porcentaje, fill = partido))+
  geom_col()+
  geom_label(aes(label = paste0(comma(porcentaje, accuracy = 0.1), '%\n', comma(votos))), 
             position = position_dodge(width = 3), size = 6, color = 'white')+
  labs(x = '', y = '%')+
  theme_minimal()+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 14, color = 'black', face = 'bold'))+
  scale_fill_manual(values = c('morena' = '#652E2D', 
                               'pan_plus' = 'blue4'
  ))


# Desglose Coalición 2024 -------------------------------------------------



qro_ayun_24  %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(prianrd = pri+pan+prd+pan_pri_prd+pan_pri+pan_prd+pri_prd, 
         jhh = morena+pt+pvem+pvem_morena+pvem_morena_pt+morena_pt+pvem_pt) %>% 
  filter(prianrd>=0) %>% 
  summarise_at(.cols = c('pri','pan','prd','pan_pri_prd','pan_pri','pan_prd','pri_prd'), .funs = sum) %>% 
  mutate(total_votos = pri+pan+prd+pan_pri_prd+pan_pri+pan_prd+pri_prd) %>% 
  pivot_longer(cols = 1:7, names_to = 'partido', values_to = 'votos') %>% 
  mutate(porcentaje = votos/total_votos*100) %>% 
  ggplot(aes(reorder(partido, -porcentaje), porcentaje, fill = partido) )+
  geom_col()+
  geom_label(aes(label = paste0(comma(porcentaje, accuracy = 0.1), '%\n', comma(votos))), 
             position = position_dodge(width = 3), size = 6, color = 'white')+
  labs(x = '', y = '%')+
  theme_minimal()+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 14, color = 'black', face = 'bold'))+
  scale_fill_manual(values = c('pan'         = '#003A8C',   # azul PAN
                               'pri'         = '#CC0000',   # rojo PRI
                               'prd'         = '#FFCC00',   # amarillo PRD
                               'pan_pri_prd' = '#8A304D',   # naranja (mezcla tricolor)
                               'pan_pri'     = '#47964A',   # morado (azul + rojo)
                               'pan_prd'     = '#8696C3',   # verde (azul + amarillo)
                               'pri_prd'     = '#E37745'    # naranja rojizo (rojo + amarillo)
  ))

qro_ayun_24  %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(prianrd = pri+pan+prd+pan_pri_prd+pan_pri+pan_prd+pri_prd, 
         jhh = morena+pt+pvem+pvem_morena+pvem_morena_pt+morena_pt+pvem_pt) %>% 
  filter(prianrd>=0) %>% 
  summarise_at(.cols = c('morena','pt','pvem','pvem_morena','pvem_morena_pt','morena_pt','pvem_pt'), .funs = sum) %>% 
  mutate(total_votos = morena+pt+pvem+pvem_morena_pt+pvem_morena+morena_pt+pvem_pt) %>% 
  pivot_longer(cols = 1:7, names_to = 'partido', values_to = 'votos') %>% 
  mutate(porcentaje = votos/total_votos*100) %>% 
  ggplot(aes(reorder(partido, -porcentaje), porcentaje, fill = partido) )+
  geom_col()+
  geom_label(aes(label = paste0(comma(porcentaje, accuracy = 0.1), '%\n', comma(votos))), 
             position = position_dodge(width = 3), size = 6, color = 'white')+
  labs(x = '', y = '%')+
  theme_minimal()+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 12, color = 'black', face = 'bold'))+
  scale_fill_manual(values = c('morena'          = '#B5261E',   # rojo guinda institucional Morena
                               'pt'              = '#CC0000',   # rojo PT
                               'pvem'            = '#5DA832',   # verde PVEM
                               
                               'pvem_morena_pt'  = '#8C1F1A',   # guinda oscuro — Morena dominante
                               'pvem_morena'     = '#7A1C18',   # guinda más apagado — Morena dominante
                               'morena_pt'       = '#A62018',   # guinda levemente más brillante — entre Morena y PT
                               'pvem_pt'         = '#4A8C28'    # verde oscuro — PVEM dominante
  ))


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


# Multi-Map 18-24 ---------------------------------------------------------

res_qro_21 <- qro_gob_21 %>% group_by(seccion) %>%
  summarise(pan_plus_21 = sum(pan, qi, pan_qi),
            morena_21 = sum(morena), 
            total_votos_21 = sum(total_votos), 
            lista_nominal = sum(lista_nominal)) %>% 
  filter(seccion!=0) %>% 
  mutate(p_gob21 = morena_21/total_votos_21*100)

res_qro_24 <- qro_ayun_24 %>% mutate(seccion = as.numeric(seccion)) %>% 
  mutate(prianrd = pri+pan+prd+pan_pri_prd+pan_pri+pan_prd+pri_prd, 
         jhh = morena+pt+pvem+pvem_morena+pvem_morena_pt+morena_pt+pvem_pt) %>% 
  group_by(seccion) %>%
  summarise(prianrd24 = sum(prianrd),
            jhh_24 = sum(jhh), 
            total_votos_24 = sum(total_votos), 
            lista_nominal = sum(lista_nominal)) %>% 
  filter(seccion!=0) %>% 
  mutate(p_mun24 = jhh_24/total_votos_24*100)


mapa_gob18 <- qro_18 %>% left_join(res_qro_21)
mapa_gob18 <- mapa_gob18 %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
mapa_gob18 <- mapa_gob18 %>% st_make_valid()

mapa_mun24 <- qro_24 %>% left_join(res_qro_24)
mapa_mun24 <-mapa_mun24 %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
mapa_mun24 <- mapa_mun24 %>% st_make_valid()

## Plots
# BINS 18
bins_18 <- c(0, 10, 20, 30,50,60, 80, 100, Inf)
pal_18 <- colorBin("YlOrRd", domain = mapa_gob18$p_gob21, bins = bins_18)

# Labels 18
labels_18 <- sprintf(
  "Seccion: <strong>%s</strong><br/> Votos: %g <br/>Porcentaje: %g %%",
  mapa_gob18$seccion, mapa_gob18$morena_21, mapa_gob18$p_gob21
) %>% lapply(htmltools::HTML)

# BINS 24
bins_24 <- c(0, 10, 20, 30,50,60, 80, 100, Inf)
pal_24 <- colorBin("YlOrRd", domain = mapa_mun24$p_mun24, bins = bins_24)
# Labels 25
labels_24 <- sprintf(
  "Seccion: <strong>%s</strong><br/> Votos: %g <br/>Porcentaje: %g %%",
  mapa_mun24$seccion, mapa_mun24$jhh_24, mapa_mun24$p_mun24
) %>% lapply(htmltools::HTML)



leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_gob18,
              fillColor = ~pal_18(p_gob21),
              group = 'Gobernador 2021',
              weight = 0.51,
              opacity = 1,
              color = "black",
              fillOpacity = 0.6, 
              highlightOptions = highlightOptions(
                weight = 1,
                color = "blue",
                fillOpacity = 1,
                bringToFront = TRUE), 
              label = labels_18,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addPolygons(data = mapa_mun24,
              fillColor = ~pal_24(p_mun24),
              group = 'Municipales 2024',
              weight = 0.51,
              opacity = 1,
              color = "black",
              fillOpacity = 0.6, 
              highlightOptions = highlightOptions(
                weight = 1,
                color = "blue",
                fillOpacity = 1,
                bringToFront = TRUE), 
              label = labels_18,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLayersControl(
    baseGroups = c("Gobernador 2021", "Municipales 2024"),  # radio buttons, only one visible at a time
    options = layersControlOptions(collapsed = FALSE)
  )

# Mapas estáticos 21 y 24 -------------------------------------------------


mapa_gob18 %>% ggplot()+
  geom_sf(aes(fill = p_gob21))+
  scale_fill_gradient(
    low  = "yellow",
    high = "red"
  )+
   labs(fill = 'Porcentaje de Votos')
  
mapa_mun24 %>% ggplot()+
  geom_sf(aes(fill = p_mun24))+
  scale_fill_gradient(
    low  = "yellow",
    high = "red"
  )+
   labs(fill = 'Porcentaje de Votos')
  
# Distribución de listas nominales
quantile(mapa_mun24$lista_nominal, probs  = seq(0,1,0.01))

mapa_mun24 %>% as.data.frame() %>% 
  ggplot(aes(x = lista_nominal))+
  geom_density()+
  geom_vline(xintercept = 1405, color = 'green')+
  geom_vline(xintercept = 3134, color = 'blue')+
  geom_vline(xintercept = 4156, color = 'red')+
  theme_minimal()


mapa_mun24 %>% filter(lista_nominal>3000)
mapa_mun24$lista_nominal %>% mean()

library(ggspatial)
library(ggmap)

ggplot(mapa_mun24) +
  annotation_map_tile(type = "osm", zoom = 10) +   # OSM tiles, no key needed
  geom_sf(aes(fill = lista_nominal), alpha = 0.5, color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Lista Nominal por Sección") +
  labs(title = "Votos por Sección") +
  theme_void()


# Reproject to a metric CRS
df_metric <- st_transform(mapa_mun24, crs = 6372)   # MAGNA-SIRGAS / Mexico (if in Mexico)

# Calculate average area in m²
mean(st_area(df_metric))

# In km²
mean(st_area(df_metric)) / 1e6
min(st_area(df_metric)) / 1e6
max(st_area(df_metric)) / 1e6

# Tablita

mapa_mun24 %>% as_tibble() %>% select(name, dto_elect, seccion, prianrd24, jhh_24, lista_nominal, total_votos_24, p_mun24) %>% 
  filter(seccion!=0) %>% arrange(desc(lista_nominal)) %>%
  clipr::write_clip()

mapa_gob18 %>% as_tibble() %>% select(name, description, distrito_l, seccion, pan_plus_21, morena_21, lista_nominal, total_votos_21, p_gob21) %>% 
  filter(seccion!=0) %>% arrange(desc(lista_nominal)) %>%
  clipr::write_clip()








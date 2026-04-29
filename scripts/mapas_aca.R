## Mapa Acapulco
# DAOA
# 28/04/2026

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, sf, ggspatial, ggmap)
rm(list = ls())
dev.off()
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()

# Cargar Datos ------------------------------------------------------------

# Mapa de sección electoral
guerrero_seccion <- read_sf('input/acapulco/cartografia_guerrero/SECCION.shp')
# acapulco
aca_seccion <- guerrero_seccion %>% filter(municipio == 1)

# Datos: https://www.iepcgro.mx/principal/sitio/procesos_electorales

gro_municip_24 <- read_csv('input/acapulco/resultados_24/resultados_ayuntamiento24.csv') %>% clean_names()
gro_diploc_24 <- read_csv('input/acapulco/resultados_24/resultados_diploc24.csv') %>% clean_names()
gro_municip_24 %>% glimpse()
aca_mun_24 <- gro_municip_24 %>% filter(municipio == 'ACAPULCO DE JUAREZ')
aca_diploc_24 <- gro_diploc_24 %>% filter(municipio == 'ACAPULCO DE JUAREZ')

aca_mun_24 <- aca_mun_24 %>% select(nombre_estado, cabecera_distrital_local, nombre_mun = municipio, seccion:lista_nominal)
aca_diploc_24 <- aca_diploc_24 %>% select(nombre_estado, cabecera_distrital_local, nombre_mun = municipio, seccion:lista_nominal)

aca_mun_24

# Mapa Distritos -----------------------------------------------------------

aca_seccion %>% ggplot()+
  geom_sf(aes(fill = factor(distrito_f)))+
  labs(fill = 'Distrito Federal')+
  theme(legend.position = 'bottom')+
  guides(guide_legend(nrow = 1))

# Plots -------------------------------------------------------------------


# Resultados por Coalición ------------------------------------------------

aca_mun_24 %>% group_by(nombre_mun) %>% 
  summarise(across(pan:lista_nominal, ~ sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(cols = pan:num_votos_nulos, values_to = 'votos', names_to = 'partido') %>% 
  mutate(porcentaje = votos/total_votos*100) %>% arrange(desc(porcentaje)) %>% as.data.frame()

aca_mun_24 %>% group_by(nombre_mun) %>% 
  summarise(across(pan:lista_nominal, ~ sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(cols = pan:num_votos_nulos, values_to = 'votos', names_to = 'partido') %>% 
  mutate(porcentaje = votos/total_votos*100) %>% arrange(desc(porcentaje)) %>% 
  mutate(coalicion = case_when(partido %in% c('morena', 'pvem', 'pt', 'pt_pvem_morena','pvem_morena', 'pt_morena', 'pt_pvem') ~ 'Juntos Haremos Historia', 
                               partido %in% c('pan', 'pri', 'prd', 'pan_pri', 'pri_prd', 'pan_prd', 'pan_pri_prd') ~ 'PRIANRD', 
                               T ~ partido
                               )) %>% 
  group_by(coalicion) %>% 
  summarise(votos = sum(votos), porcentaje = sum(porcentaje), total_votos = max(total_votos), lista_nominal = max(lista_nominal)) %>% 
  mutate(coalicion = case_when(coalicion == 'fxmg' ~ 'Fuerza por México', 
                   coalicion == 'ma' ~ 'México Avanza', 
                   coalicion == 'mc' ~ 'Movimiento Ciudadano', 
                   coalicion == 'mlgro' ~ 'Movimiento Laborista', 
                   coalicion == 'pac' ~ 'Alianza Ciudadana', 
                   coalicion == 'pbg' ~ 'Partido Bienestar', 
                   coalicion == 'pes' ~ 'Partido Encuentro Social', 
                   coalicion == 'psg' ~ 'Partido Socialista', 
                   coalicion == 'regeneracion' ~ 'Regeneración', 
                   coalicion == 'num_votos_nulos' ~ 'Nulos', 
                   coalicion == 'num_votos_can_nreg' ~ 'No registrado', 
                   T ~ coalicion
                   )) %>% 
  filter(coalicion!='num_votos_validos') %>% 
  ggplot(aes(reorder(str_wrap(coalicion, 10), -porcentaje), porcentaje, fill = coalicion))+
  geom_col()+
  geom_label(aes(label = paste0(comma(votos), '\n', comma(porcentaje, accuracy = 0.1), '%')), size = 6, color = 'white')+
  labs(x = '', y = 'Porcentaje de Votos Recibidos', caption = 'Fuente: IEEG - Tabla de Resultados por Sección')+
  theme_minimal()+
  scale_fill_manual(values = c('Juntos Haremos Historia' = '#652E2D', 
                               'PRIANRD' = '#2F3C75', 
                               'Movimiento Ciudadano' = '#ED8638', 
                               'Fuerza por México' = '#DB6693',
                               'México Avanza' = '#535861',
                               'Movimiento Laborista' = '#8C3C39', 
                               'Alianza Ciudadana' = '#D4403C', 
                               'Partido Bienestar' = '#7E2B31', 
                               'Partido Encuentro Social' = '#6C3484', 
                               'Partido Socialista' = '#C43836', 
                               'Regeneración' = '#D2382F', 
                               'Nulos' = 'grey', 
                               'No registrado' = 'grey'))+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 10, color = 'black', face = 'bold'))


aca_mun_24 %>% group_by(seccion) %>% 
  summarise(across(pan:lista_nominal, ~ sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(cols = pan:num_votos_nulos, values_to = 'votos', names_to = 'partido') %>% 
  mutate(porcentaje = votos/total_votos*100) %>% arrange(desc(porcentaje)) %>% 
  mutate(coalicion = case_when(partido %in% c('morena', 'pvem', 'pt', 'pt_pvem_morena','pvem_morena', 'pt_morena', 'pt_pvem') ~ 'Juntos Haremos Historia', 
                               partido %in% c('pan', 'pri', 'prd', 'pan_pri', 'pri_prd', 'pan_prd', 'pan_pri_prd') ~ 'PRIANRD', 
                               T ~ partido
                               )) %>% 
  mutate(coalicion = case_when(coalicion == 'fxmg' ~ 'Fuerza por México', 
                   coalicion == 'ma' ~ 'México Avanza', 
                   coalicion == 'mc' ~ 'Movimiento Ciudadano', 
                   coalicion == 'mlgro' ~ 'Movimiento Laborista', 
                   coalicion == 'pac' ~ 'Alianza Ciudadana', 
                   coalicion == 'pbg' ~ 'Partido Bienestar', 
                   coalicion == 'pes' ~ 'Partido Encuentro Social', 
                   coalicion == 'psg' ~ 'Partido Socialista', 
                   coalicion == 'regeneracion' ~ 'Regeneración', 
                   coalicion == 'num_votos_nulos' ~ 'Nulos', 
                   coalicion == 'num_votos_can_nreg' ~ 'No registrado', 
                   T ~ coalicion
                   )) %>% 
  filter(coalicion!='num_votos_validos') %>% 
  group_by(seccion) %>% slice_max(order_by = votos, n = 1) %>% 
  ungroup() %>% count(coalicion) %>% 
  ggplot(aes(coalicion, n, fill = coalicion))+
  geom_col()+
  geom_label(aes(label =comma(n)), size = 10, color = 'white')+
  labs(x = '', y = 'Secciones Electorales', caption = 'Fuente: IEEG - Tabla de Resultados por Sección')+
  theme_minimal()+
  scale_fill_manual(values = c('Juntos Haremos Historia' = '#652E2D', 
                               'PRIANRD' = '#2F3C75', 
                               'Movimiento Ciudadano' = '#ED8638', 
                               'Fuerza por México' = '#DB6693',
                               'México Avanza' = '#535861',
                               'Movimiento Laborista' = '#8C3C39', 
                               'Alianza Ciudadana' = '#D4403C', 
                               'Partido Bienestar' = '#7E2B31', 
                               'Partido Encuentro Social' = '#6C3484', 
                               'Partido Socialista' = '#C43836', 
                               'Regeneración' = '#D2382F', 
                               'Nulos' = 'grey', 
                               'No registrado' = 'grey'))+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 10, color = 'black', face = 'bold'))

# Desglose de la Coalición ------------------------------------------------


aca_mun_24 %>% group_by(nombre_mun) %>% 
  summarise(across(pan:lista_nominal, ~ sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(cols = pan:num_votos_nulos, values_to = 'votos', names_to = 'partido') %>% 
  mutate(porcentaje = votos/total_votos*100) %>% arrange(desc(porcentaje)) %>% 
  mutate(coalicion = case_when(partido %in% c('morena', 'pvem', 'pt', 'pt_pvem_morena','pvem_morena', 'pt_morena', 'pt_pvem') ~ 'Juntos Haremos Historia', 
                               partido %in% c('pan', 'pri', 'prd', 'pan_pri', 'pri_prd', 'pan_prd', 'pan_pri_prd') ~ 'PRIANRD', 
                               T ~ partido
  )) %>% 
  filter(coalicion == 'Juntos Haremos Historia') %>% 
  mutate(partido = str_to_title(str_replace_all( partido,replacement = ' ', pattern = '_'))) %>%
  mutate(porcentaje = votos/sum(votos) * 100) %>% 
  ggplot(aes(reorder(partido, -porcentaje), porcentaje, fill = partido))+
  geom_col()+
  geom_label(aes(label =paste0(comma(votos) ,'\n', comma(porcentaje, accuracy = 0.1), '%')), size = 5, color = 'white') +
  labs(x = '', y = 'Secciones Electorales', caption = 'Fuente: IEEG - Tabla de Resultados por Sección')+
  theme_minimal()+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 10, color = 'black', face = 'bold'))+
  scale_fill_manual(values = c('Morena' = '#652E2D', 
                    'Pt' = '#D4403C',
                    'Pvem' = '#71B75B',
                    'PRIANRD' = '#2F3C75', 
                    'Pt Pvem Morena' = '#CA3F32',
                    'Pvem Morena' = '#A6C971',
                    'Pt Pvem' = '#B83933',
                    'Pt Morena' = '#A93A2F'
                    ))

# Mapas -------------------------------------------------------------------


# Mapa Dinámico -----------------------------------------------------------

aca_seccion <- aca_seccion %>% left_join(aca_mun_24)
  
aca_seccion

aca_seccion <- aca_seccion %>% st_make_valid()

st_crs(aca_seccion)
aca_seccion <- st_transform(aca_seccion, crs = 4326)
st_crs(aca_seccion)

library(leaflet)
library(htmltools)

aca_seccion <- aca_seccion %>% mutate(coalicion_morena = morena + pt + pvem + pt_pvem_morena + pt_pvem + pt_morena + pvem_morena, 
                       tot_jhh = coalicion_morena/total_votos*100)

labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  aca_seccion$seccion, aca_seccion$lista_nominal, aca_seccion$total_votos, aca_seccion$coalicion_morena , aca_seccion$tot_jhh
) %>% lapply(htmltools::HTML)


bins <- c(0, 10, 20,30, 40, 50,60, 70, 80,Inf)
pal <- colorBin("YlOrRd", domain = aca_seccion$tot_jhh, bins = bins)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = aca_seccion, 
              color = 'black', weight = 0.5) %>%
  addPolygons(data = aca_seccion,
              fillColor = ~pal(tot_jhh),
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

# Mapa con Toggle ---------------------------------------------------------

aca_mun_24 <- aca_mun_24 %>% mutate(coalicion_morena = morena + pt + pvem + pt_pvem_morena + pt_pvem + pt_morena + pvem_morena, 
                     tot_jhh = coalicion_morena/total_votos*100)

aca_dl_24 <- aca_diploc_24 %>% mutate(coalicion_morena_dl = morena + pt + pvem + pt_pvem_morena + pt_pvem + pt_morena + pvem_morena, 
                     tot_jhh_dl = coalicion_morena_dl/total_votos*100)

map_data <- left_join(aca_mun_24 %>% select(seccion, coalicion_morena, tot_jhh, total_votos_mun = total_votos, lista_nominal), 
                      aca_dl_24 %>% select(seccion, coalicion_morena_dl, tot_jhh_dl, total_votos_dl = total_votos)
)


aca_seccion <- aca_seccion %>% left_join(map_data)

aca_seccion <- aca_seccion %>% st_make_valid()

st_crs(aca_seccion)
aca_seccion <- st_transform(aca_seccion, crs = 4326)
st_crs(aca_seccion)

library(leaflet)
library(htmltools)


labels_mun <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  aca_seccion$seccion, aca_seccion$lista_nominal, aca_seccion$total_votos_mun, aca_seccion$coalicion_morena , aca_seccion$tot_jhh
) %>% lapply(htmltools::HTML)

labels_dl <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  aca_seccion$seccion, aca_seccion$lista_nominal, aca_seccion$total_votos_dl, aca_seccion$coalicion_morena_dl , aca_seccion$tot_jhh_dl
) %>% lapply(htmltools::HTML)


bins <- c(0, 10, 20,30, 40, 50,60, 70, 80,Inf)
pal_mun <- colorBin("YlOrRd", domain = aca_seccion$tot_jhh, bins = bins)
pal_dl <- colorBin("YlOrRd", domain = aca_seccion$tot_jhh_dl, bins = bins)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = aca_seccion,
              fillColor = ~pal_mun(tot_jhh),
              weight = 0.51,
              opacity = 1,
              color = "black",
              fillOpacity = 0.6, 
              group = "Elecciones Municipales 24",   # ← must match exactly
              highlightOptions = highlightOptions(
                weight = 1,
                color = "blue",
                fillOpacity = 1,
                bringToFront = TRUE), 
              label = labels_mun,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addPolygons(data = aca_seccion,
              fillColor = ~pal_dl(tot_jhh_dl),
              weight = 0.51,
              opacity = 1,
              color = "black",
              fillOpacity = 0.6, 
              group = "Elecciones DL 24",            # ← must match exactly
              highlightOptions = highlightOptions(
                weight = 1,
                color = "blue",
                fillOpacity = 1,
                bringToFront = TRUE), 
              label = labels_dl,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLayersControl(
    baseGroups = c("Elecciones Municipales 24", "Elecciones DL 24"),  # ← same strings
    options = layersControlOptions(collapsed = FALSE)
  )
  
# Mapa Estático -----------------------------------------------------------

aca_seccion <- aca_seccion %>% st_make_valid()
# Back OSM, zoom 14 to get SOMETHING, otherwise tiling sucks on the back, no API key needed!
ggplot(aca_seccion) +
  annotation_map_tile(type = "osm", zoom = 11) +   # OSM tiles, no key needed, me mamé, demasiado zoom
  geom_sf(aes(fill = tot_jhh), alpha = 0.5, color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Porcentaje de Votos por Juntos haremos historia") +
  labs(title = "Votos por Sección") +
  theme_void()

aca_seccion %>% ggplot()+
  geom_sf(aes(fill = tot_jhh))+
  scale_fill_gradient(
    low  = "yellow",
    high = "red"
  )+
  labs(fill = 'Porcentaje JHH')


# Secciones
ggplot(aca_seccion) +
  annotation_map_tile(type = "osm", zoom = 12) +   # OSM tiles, no key needed, me mamé, demasiado zoom con 14, 11 ok
  geom_sf(aes(fill = lista_nominal), alpha = 0.5, color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Lista Nominal") +
  theme_void()

# Distribucion ------------------------------------------------------------


quantile(aca_seccion$lista_nominal, probs = seq(0,1,0.01))

aca_seccion %>% as.data.frame() %>% 
  ggplot(aes(x = lista_nominal))+
  geom_density()+
  geom_vline(xintercept = 1281, color = 'green')+
  geom_vline(xintercept = 1819, color = 'blue')+
  geom_vline(xintercept = 3055, color = 'red')+
  theme_minimal()

aca_seccion %>% filter(lista_nominal>3000)

 mean(aca_seccion$lista_nominal, na.rm = T)



# Metric ------------------------------------------------------------------

 
# Reproject to a metric CRS
df_metric <- st_transform(aca_seccion, crs = 6372)   # MAGNA-SIRGAS / Mexico (if in Mexico)
 
# Calculate average area in m²
mean(st_area(df_metric))
 
# In km²
quantile(st_area(df_metric) / 1e6, probs = seq(0,1,0.01))
df_metric$area <- as.numeric(st_area(df_metric)/ 1e6)
df_metric %>% glimpse()
library(units)

min(df_metric$area)
max(df_metric$area)

ggplot(df_metric %>% as_tibble(), aes(x = area) )+
  geom_density()


# Tablita -----------------------------------------------------------------

aca_mun_24%>% mutate(coalicion_morena = morena + pt + pvem + pt_pvem_morena + pt_pvem + pt_morena + pvem_morena, 
                     tot_jhh = coalicion_morena/total_votos*100, participacion = total_votos/lista_nominal * 100) %>% 
  select(nombre_mun, seccion, pan:participacion) %>% arrange(desc(lista_nominal)) %>% clipr::write_clip()












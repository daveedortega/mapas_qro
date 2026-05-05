## Mapas Electorales Cuernavaca
# DAOA
# 04/05/2026

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, sf, leaflet, htmltools)
rm(list = ls())
dev.off()
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()

# Cargar Datos ------------------------------------------------------------

ayun_cuerna_24 <- read_csv('input/morelos/cuerna24_ayun.csv') %>% clean_names()

# Cargar Mapas ------------------------------------------------------------

# Revisión de Mapas
mg24 <- read_sf('~/Desktop/marco_geoestadistico/mg_2024_integrado/conjunto_de_datos/00mun.shp')

mg24 %>% filter(NOMGEO == 'Cuernavaca') %>% 
  mutate(NOMGEO = 'Cuernavaca INEGI') %>% 
  ggplot()+
  annotation_map_tile(type = "osm", zoom = 12) +   # OSM tiles, no key needed, me mamé, demasiado zoom con 14, 11 ok
  geom_sf(aes(fill = NOMGEO), alpha = 0.7)+
  geom_sf(data = morelos %>% filter(nombre == 'CUERNAVACA') %>% 
            mutate(municipio = 'Cuernavaca IEM'), aes(fill = municipio), alpha = 0.7)
  

# morelos <- read_sf('input/morelos/MUNICIPIO.shp')
morelos <- read_sf('input/morelos/SECCION.shp')

morelos %>% 
  ggplot()+
  geom_sf(aes(fill = factor(municipio)))

cuernavaca <- morelos %>% filter(municipio == 7)

# Sepa, lit de acuerdo a las autoridades

rm(mg24)

# Mapas -------------------------------------------------------------------

# Distritos

cuernavaca %>% ggplot()+
  geom_sf(aes(fill = factor(distrito_f)))+
  labs(fill = 'Distrito Federal')

cuernavaca %>% ggplot()+
  geom_sf(aes(fill = factor(distrito_l)))+
  labs(fill = 'Distrito Local')


# Analisis ----------------------------------------------------------------

ayun_cuerna_24 %>% summarise(sum(tot_votos), sum(l_nominal))

res_ayun_cuenra_24 <- ayun_cuerna_24 %>% mutate(
  prianrd = rsp+pan+pri+prd+
    c_pan_pri_prd_rsp+c_pan_pri_prd+c_pan_pri_rsp+c_pan_prd_rsp+c_pan_pri+c_pan_prd+c_pan_rsp+
    c_pri_prd_rsp+c_pri_prd+c_pri_rsp+
    c_prd_rsp, 
  shh = morena+na+pes+mas+
    c_morena_na_pes_mas+c_morena_na_pes+c_morena_na_mas+c_morena_pes_mas+
    c_morena_pes+c_morena_na+c_morena_mas+
    c_na_mas+c_na_pes+c_pes_mas,
  mc_plus = mc + progresa+c_mc_progresa
  ) %>% 
  group_by(seccion) %>% 
  summarise_at(.cols = c('prianrd', 'shh', 'mc_plus', 'pt', 'pvem',
                         'num_votos_nulos', 'no_registrados', 'tot_votos', 'l_nominal'), .funs = sum)

ayun_cuerna_24 %>% glimpse()

res_ayun_cuenra_24 %>% filter(seccion!='Totales') %>% 
  pivot_longer(cols = prianrd:no_registrados) %>% 
  group_by(partido = name) %>% 
  summarise(votos = sum(value), 
            l_nominal = sum(l_nominal), 
            tot_votos = sum(tot_votos)) %>% 
  mutate(porcentaje = votos/tot_votos * 100) %>% 
  mutate(partido = case_when(partido == 'shh' ~ 'Sigamos Haciendo Historia', 
                             partido == 'prianrd' ~ 'PAN+PRI+PRD+RSP', 
                             partido == 'pt' ~ 'PT', 
                             partido == 'pvem' ~ 'PVEM', 
                             partido == 'mc_plus' ~ 'MC + Progresa', 
                             partido == 'num_votos_nulos' ~ 'Nulos', 
                             partido == 'no_registrados' ~ 'No registrados', 
                             )) %>% 
  ggplot(aes(reorder(partido,-porcentaje), porcentaje, fill = partido))+
  geom_col()+
  geom_label(aes(label = paste0(comma(porcentaje, accuracy = 0.1), '%\n', comma(votos))), size = 6, color = 'white')+
  labs(x = '', y = '%')+
  theme_minimal()+
  scale_fill_manual(values = c('Sigamos Haciendo Historia' = '#652E2D', 
                               'MC + Progresa' = '#ED8638', 
                               'PT' = '#D93D37', 
                               'PVEM' = '#71B75B', 
                               'PAN+PRI+PRD+RSP' = '#2F3C75', 
                               'Nulos' = 'grey', 
                               'No registrados' = 'grey'))+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 10, color = 'black', face = 'bold'))



res_ayun_cuenra_24 %>% pivot_longer(cols = prianrd:no_registrados) %>% 
  filter(seccion!='Totales') %>% 
  group_by(seccion ,partido = name) %>% 
  summarise(votos = sum(value), 
            l_nominal = sum(l_nominal), 
            tot_votos = sum(tot_votos)) %>% 
  slice_max(votos, n = 1) %>% 
  mutate(partido = case_when(partido == 'shh' ~ 'Sigamos Haciendo Historia', 
                             partido == 'prianrd' ~ 'PAN+PRI+PRD+RSP', 
                             partido == 'pt' ~ 'PT', 
                             partido == 'pvem' ~ 'PVEM', 
                             partido == 'mc_plus' ~ 'MC + Progresa', 
                             partido == 'num_votos_nulos' ~ 'Nulos', 
                             partido == 'no_registrados' ~ 'No registrados', 
  )) %>% ungroup() %>% 
  count(partido) %>% 
  ggplot(aes(partido, n, fill = partido))+
  geom_col()+
  geom_label(aes(label = comma(n)), size = 8, color = 'white')+
  labs(x = '', y = 'Secciones')+
  theme_minimal()+
  scale_fill_manual(values = c('Sigamos Haciendo Historia' = '#652E2D', 
                               'MC + Progresa' = '#ED8638', 
                               'PT' = '#D93D37', 
                               'PVEM' = '#71B75B', 
                               'PAN+PRI+PRD+RSP' = '#2F3C75', 
                               'Nulos' = 'grey', 
                               'No registrados' = 'grey'))+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 10, color = 'black', face = 'bold'))


# Mapa Secciones Dinámico -------------------------------------------------


map_data <- res_ayun_cuenra_24 %>% pivot_longer(cols = prianrd:no_registrados) %>% 
  filter(seccion!='Totales') %>% 
  group_by(seccion ,partido = name) %>% 
  summarise(votos = sum(value), 
            l_nominal = sum(l_nominal), 
            tot_votos = sum(tot_votos)) %>% 
  filter(partido == 'shh') %>%
  mutate(p_morena = votos/tot_votos*100) %>% 
  mutate(seccion = as.numeric(seccion))

cuernavaca <- cuernavaca %>% left_join(map_data) %>% st_make_valid()


labels_mun <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  cuernavaca$seccion, cuernavaca$l_nominal, cuernavaca$tot_votos, cuernavaca$votos , cuernavaca$p_morena
) %>% lapply(htmltools::HTML)

bins <- c(0, 10, 20,30, 40, 50,60, 70, 80,Inf)
pal_mun <- colorBin("YlOrRd", domain = cuernavaca$p_morena, bins = bins)

cuernavaca <- st_transform(cuernavaca, crs = 4326)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = cuernavaca,
              fillColor = ~pal_mun(p_morena),
              weight = 0.51,
              opacity = 1,
              color = "black",
              fillOpacity = 0.6, 
              highlightOptions = highlightOptions(
                weight = 1,
                color = "blue",
                fillOpacity = 1,
                bringToFront = TRUE), 
              label = labels_mun,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) 


cuernavaca %>% ggplot()+
  geom_sf(aes(fill = p_morena))+
  scale_fill_gradient(low = "yellow", high = "red", name = "Porcentaje Morena")
  




















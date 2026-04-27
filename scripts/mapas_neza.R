## Mapas Neza
# DAOA
# 27/04/2026

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, sf)
rm(list = ls())
dev.off()
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()

# Cargar Datos ------------------------------------------------------------

# Mapas -------------------------------------------------------------------

secciones_edomex %>% as_tibble()%>% count(distrito_l)

secciones_edomex <- read_sf('~/Desktop/Mapas/secciones_edomex/SECCION.shp')

secciones_edomex %>% 
  ggplot()+
  geom_sf(aes(fill = factor(distrito_l)))+
  labs(fill = 'Distrito Local')+
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(ncol = 11))

secciones_edomex %>% 
  ggplot()+
  geom_sf(aes(fill = factor(distrito_f)))+
  labs(fill = 'Distrito Federal')+
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(ncol = 11))

# Municipio de Nezahualcoyotl: 60, integrado por DOS secciones electorales

secciones_neza <- secciones_edomex %>% filter(municipio == 60)

secciones_neza %>% 
  ggplot()+
  geom_sf(aes(fill = factor(distrito_l)))+
  labs(fill = 'Distrito Local')

secciones_neza %>% 
  ggplot()+
  geom_sf(aes(fill = factor(distrito_f)))+
  labs(fill = 'Distrito Federal')

# Datos electorales -------------------------------------------------------

resultados_ayuntamientos_24 <- read_xlsx('input/neza/Ayuntamientos 2024.xlsx', skip = 4) %>% clean_names()
resultados_dip_loc_24 <- read_csv('input/neza/diputaciones_locales_24.csv') %>% clean_names()

# Datos de Elecciones: https://www.ieem.org.mx/procesos-electorales/historico-de-procesos-electorales/elecciones-locales.html
# Tomar Tabla de Resultados, EXISTEN A NIVEL CASILLA, alv

resultados_ayuntamientos_24 %>% glimpse()
resultados_dip_loc_24 %>% glimpse()

resultados_dip_loc_24 %>% count(cabecera_distrital_local) %>% as.data.frame()
resultados_dip_loc_24 %>% filter(cabecera_distrital_local == 'CD. NEZAHUALCOYOTL') %>% 
  summarise(sum(lista_nominal), sum(total_votos))


# Para ayuntamientos sí está desglosado 681 secciones, una es 0
resultados_ayuntamientos_24 %>% filter(cabecera_distrital_local == 'CD. NEZAHUALCOYOTL') %>% 
  select(seccion, casillas, 
         pan:cand_ind9, total_votos, lista_nominal) %>% count(seccion)

res_neza_muni <- resultados_ayuntamientos_24 %>% filter(cabecera_distrital_local == 'CD. NEZAHUALCOYOTL') %>% 
  select(seccion, casillas, 
         pan:cand_ind9, num_votos_nulos, candidato_no_registrado = num_votos_can_nreg,total_votos, lista_nominal) 
# 680 secciones, correcto!

# Graph Totales -----------------------------------------------------------

res_neza_muni  %>% summarise(morena = sum(morena), 
                             mc = sum(mc), 
                             pt = sum(pt), 
                             pvem = sum(pvem), 
                             cc_pan_pri_prd_naem = sum(cc_pan_pri_prd_naem), 
                             num_votos_nulos = sum(num_votos_nulos), 
                             candidato_no_registrado = sum(candidato_no_registrado),
                            total = sum(total_votos))  %>% 
  pivot_longer(cols = morena:candidato_no_registrado, names_to = 'partido', values_to = 'votos') %>% 
  mutate(porcentaje = votos/total * 100) %>% 
  mutate(
    partido = fct_recode(partido,
                         "Movimiento Ciudadano" = "mc",
                         "Morena" = "morena",
                         "PT" = "pt",
                         "PRIANRD + NA" = "cc_pan_pri_prd_naem",
                         "Partido Verde"  = "pvem", 
                         "Nulos"  = "num_votos_nulos", 
                         "No registrado"  = "candidato_no_registrado", 
    )
  ) %>% 
  ggplot(aes(reorder(partido, -porcentaje), porcentaje))+
  geom_col(aes(fill = partido))+
  geom_label(aes(label = paste0(comma(votos), '\n', comma(porcentaje, accuracy = 0.1), '%')), size = 6)+
  labs(x = '', y = 'Porcentaje de Votos Recibidos', caption = 'Fuente: IEEM - Tabla de Resultados por Sección')+
  theme_minimal()+
  scale_fill_manual(values = c('Morena' = '#652E2D', 
                               'Movimiento Ciudadano' = '#ED8638', 
                               'PT' = '#D93D37', 
                               'Partido Verde' = '#71B75B', 
                               'PRIANRD + NA' = '#2F3C75', 
                               'Nulos' = 'grey', 
                               'No registrado' = 'grey'))+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 10, color = 'black', face = 'bold'))

# Gráfica secciones Ganadas -----------------------------------------------

res_neza_muni %>% filter(seccion!=0) %>% 
  group_by(seccion)  %>% summarise(morena = sum(morena), 
                             mc = sum(mc), 
                             pt = sum(pt), 
                             pvem = sum(pvem), 
                             cc_pan_pri_prd_naem = sum(cc_pan_pri_prd_naem), 
                             num_votos_nulos = sum(num_votos_nulos), 
                             candidato_no_registrado = sum(candidato_no_registrado),
                             total = sum(total_votos)) %>% 
  pivot_longer(morena:candidato_no_registrado) %>% 
  group_by(seccion) %>% 
  slice_max(order_by = value, n = 1) %>% 
  ungroup() %>% 
  count(name) %>% 
  mutate(
    name = fct_recode(name,
                         "Movimiento Ciudadano" = "mc",
                         "Morena" = "morena",
                         "PT" = "pt",
                         "PRIANRD + NA" = "cc_pan_pri_prd_naem",
                         "Partido Verde"  = "pvem", 
                         "Nulos"  = "num_votos_nulos", 
                         "No registrado"  = "candidato_no_registrado", 
    )
  ) %>% 
  ggplot(aes(reorder(name, -n), n, fill = name) ) +
  geom_col()+
  geom_label(aes(label = n), color = 'white', size = 14)+
  labs(x = '', y = 'Secciones Electorales', caption = 'Fuente: IEEM - Tabla de Resultados por Sección')+
  theme_minimal()+
  scale_fill_manual(values = c('Morena' = '#652E2D', 
                               'Movimiento Ciudadano' = '#ED8638', 
                               'PT' = '#D93D37', 
                               'Partido Verde' = '#71B75B', 
                               'PRIANRD + NA' = '#2F3C75', 
                               'Nulos' = 'grey', 
                               'No registrado' = 'grey'))+
  theme(text = element_text(family = 'Montserrat'), 
        legend.position = 'none', 
        axis.text.x = element_text(size = 10, color = 'black', face = 'bold'))


# Mapa Electoral 2024 -----------------------------------------------------

candidat_ganadoras_neza <- res_neza_muni %>% filter(seccion!=0) %>% 
  group_by(seccion)  %>% summarise(morena = sum(morena), 
                                   mc = sum(mc), 
                                   pt = sum(pt), 
                                   pvem = sum(pvem), 
                                   cc_pan_pri_prd_naem = sum(cc_pan_pri_prd_naem), 
                                   num_votos_nulos = sum(num_votos_nulos), 
                                   candidato_no_registrado = sum(candidato_no_registrado),
                                   total = sum(total_votos)) %>% 
  pivot_longer(morena:candidato_no_registrado) %>% 
  group_by(seccion) %>% 
  slice_max(order_by = value, n = 1) %>% select(seccion, candidatura_ganadora = name)

resultados_morena_neza <- res_neza_muni %>% filter(seccion!=0) %>% 
  group_by(seccion)  %>% summarise(morena = sum(morena), 
                                   mc = sum(mc), 
                                   pt = sum(pt), 
                                   pvem = sum(pvem), 
                                   cc_pan_pri_prd_naem = sum(cc_pan_pri_prd_naem), 
                                   num_votos_nulos = sum(num_votos_nulos), 
                                   candidato_no_registrado = sum(candidato_no_registrado),
                                   total = sum(total_votos), 
                                   lista_nominal = sum(lista_nominal)) %>% 
  pivot_longer(morena:candidato_no_registrado) %>% 
  mutate(ln_morena = value/lista_nominal * 100, 
         tot_morena = value/total * 100) %>% 
  filter(name == 'morena') %>% 
  select(-name) %>% rename(votos_morena = value)

# Mapa

mapa_neza <- secciones_neza %>% 
  left_join(resultados_morena_neza) %>% 
  left_join(candidat_ganadoras_neza)

mapa_neza <- mapa_neza %>% st_make_valid()

st_crs(mapa_neza)
mapa_neza <- st_transform(mapa_neza, crs = 4326)
st_crs(mapa_neza)

library(leaflet)
library(htmltools)

labels <- sprintf(
  "<strong>%s</strong><br/> Votos: %g <br/>Porcentaje: %g %%",
  mapa_neza$seccion, mapa_neza$votos_morena, mapa_neza$tot_morena
) %>% lapply(htmltools::HTML)


bins <- c(0, 10, 30, 40,45 , 55,60,Inf)
pal <- colorBin("YlOrRd", domain = mapa_neza$tot_morena, bins = bins)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_neza, 
              color = 'black', weight = 0.5) %>%
  addPolygons(data = mapa_neza,
              fillColor = ~pal(tot_morena),
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






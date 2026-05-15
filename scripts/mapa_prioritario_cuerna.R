## Mapa Prioritario Cuerna
# DAOA
# 14/05/2026

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, sf)
rm(list = ls())
dev.off()
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()

# Cargar Datos ------------------------------------------------------------

mapa_cuerna <- read_rds('../preplike_shiny/data/mapa_mun24_cuerna.rds')

# Complementar Mapa -------------------------------------------------------

# Top lugares para campaña territorial. 
# Pocas secciones?
# 40 secciones
# Top por lista nominal


mapa_cuerna$lista_nominal


mayor_ln <- mapa_cuerna %>% slice_max(order_by = lista_nominal, n = 40, with_ties = F) 
mayor_morena <- mapa_cuerna %>% slice_max(order_by = tot_morena, n = 40, with_ties = F)
mayor_prianrd <- mapa_cuerna %>%
  mutate(tot_ops = prianrd/total_votos * 100) %>%
  slice_max(order_by = tot_ops, n = 40, with_ties = F)


mapa_cuerna <- mapa_cuerna %>% mutate(top_ln = ifelse(seccion %in% mayor_ln$seccion, 'Top 40 Lista Nominal', NA))
mapa_cuerna <- mapa_cuerna %>% mutate(top_morena= ifelse(seccion %in% mayor_morena$seccion, 'Top 40 Secciones por Morena', NA))
mapa_cuerna <- mapa_cuerna %>% mutate(top_ops = ifelse(seccion %in% mayor_prianrd$seccion, 'Top 40 Secciones con Voto Opositor', NA))

# new priorities
mapa_cuerna <- mapa_cuerna %>% mutate(priority = paste(top_ln, top_morena, top_ops)) %>% 
  mutate(priority = str_squish(str_remove_all(priority,'NA')) )

mapa_cuerna <- mapa_cuerna %>% mutate(priority = ifelse(priority == '', NA, priority))

mapa_cuerna %>% 
  ggplot(aes(x = lista_nominal))+
  stat_ecdf(geom = "step") 


mapa_cuerna <- mapa_cuerna %>% mutate(participacion = total_votos/lista_nominal * 100)
mapa_cuerna %>% ggplot(aes(lista_nominal, tot_morena, color = candidatura_ganadora))+
  geom_point()


# Pallettes
color_map <- c(
  "Top 40 Lista Nominal" = "green4",
  "Top 40 Secciones por Morena" = "salmon",
  "Top 40 Secciones con Voto Opositor" = "blue",
  "Top 40 Lista Nominal Top 40 Secciones por Morena" = "firebrick",
  "Top 40 Lista Nominal Top 40 Secciones con Voto Opositor" = "lightblue"
)

pal_binary <- colorFactor(
  palette  = unname(color_map),
  levels   = names(color_map),
  domain   = mapa_cuerna$priority,
  na.color = "#cccccc"
)

labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Participación: %g %% <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  mapa_cuerna$seccion, mapa_cuerna$lista_nominal, mapa_cuerna$participacion, mapa_cuerna$morena, mapa_cuerna$tot_morena
) %>% lapply(htmltools::HTML)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_cuerna, 
              weight = 0.51, opacity = 1, color = "black", fillOpacity = 0.6,
              fillColor = ~pal_binary(priority),
              highlightOptions = highlightOptions(color = 'blue',
                                                  fillColor = 'blue'), 
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(position = "bottomright",
            pal      = pal_binary,
            values   = mapa_cuerna$priority,
            title    = "Tipo de Sección",
            opacity  = 1)

# Mayor impacto de pauta
# Donde mejor le vaya al pan?
# 











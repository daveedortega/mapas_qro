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

# Select N

n_select <- 40

mayor_ln <- mapa_cuerna %>% slice_max(order_by = lista_nominal, n = n_select, with_ties = F) 
mayor_morena <- mapa_cuerna %>% slice_max(order_by = tot_morena, n = n_select, with_ties = F)
mayor_prianrd <- mapa_cuerna %>%
  mutate(tot_ops = prianrd/total_votos * 100) %>%
  slice_max(order_by = tot_ops, n = n_select, with_ties = F)


mapa_cuerna <- mapa_cuerna %>% mutate(top_ln = ifelse(seccion %in% mayor_ln$seccion, paste0('Top ', n_select, ' Secciones por Lista Nominal'), NA))
mapa_cuerna <- mapa_cuerna %>% mutate(top_morena= ifelse(seccion %in% mayor_morena$seccion, paste0('Top ', n_select,' Secciones por Morena'), NA))
mapa_cuerna <- mapa_cuerna %>% mutate(top_ops = ifelse(seccion %in% mayor_prianrd$seccion, paste0('Top ', n_select, ' Secciones con Voto Opositor'), NA))

mapa_cuerna <- mapa_cuerna %>% mutate(priority = paste(top_ln, top_morena, top_ops))

mapa_cuerna <- mapa_cuerna %>% mutate(priority = str_squish(str_remove_all(priority, 'NA')))

priorities <- unique(mapa_cuerna$priority)

# Dynamically assign colors based on which categories are present
assign_color <- function(p) {
  ln  <- grepl("Lista Nominal", p)
  mor <- grepl("Morena",        p)
  ops <- grepl("Opositor",      p)
  
  dplyr::case_when(
    ln  & mor & ops ~ "purple",      # in all three
    ln  & mor       ~ "orange",      # lista nominal + morena
    ln  & ops       ~ "blue",   # lista nominal + opositor
    mor & ops       ~ "salmon",      # morena + opositor
    ln              ~ "green4",   # only lista nominal
    mor             ~ "firebrick",   # only morena
    ops             ~ "steelblue",      # only opositor
    TRUE            ~ "#cccccc"      # not in any top (empty string)
  )
}

color_map <- setNames(
  sapply(priorities, assign_color),
  priorities
)

pal_binary <- colorFactor(
  palette  = unname(color_map),
  levels   = names(color_map),
  domain   = mapa_cuerna$priority,
  na.color = "#cccccc"
)

mapa_cuerna <- mapa_cuerna %>% mutate(participacion = total_votos/lista_nominal * 100)

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

mapa_cuerna %>% count(priority)
mapa_cuerna %>% st_drop_geometry() %>% clipr::write_clip()
# Mayor impacto de pauta
# Donde mejor le vaya al pan?
# 


# Copy participacion y cosa morena ----------------------------------------



mapa_cuerna <- mapa_cuerna %>% mutate(tot_ops = prianrd/total_votos*100)%>% mutate(propensity_ops = participacion * (tot_ops/100)) 

ci <- classInt::classIntervals(var = mapa_cuerna$propensity_ops, n = 5, style = "jenks")

breaks <- ci$brks
pal <- colorBin(
  palette  = c("#C6DBF0", "#9ECAE1", "#6AAED6", "#2171B5", "#08306B"),
  domain   = mapa_cuerna$propensity_ops,
  bins     = breaks,
  na.color = "#cccccc"
)


labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Participación: %g %% <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  mapa_cuerna$seccion, mapa_cuerna$lista_nominal, mapa_cuerna$participacion, mapa_cuerna$morena, mapa_cuerna$tot_morena
) %>% lapply(htmltools::HTML)



ci <- classInt::classIntervals(var = mapa_cuerna$participacion, n = 5, style = "jenks")

breaks_par <- ci$brks
pal_par <- colorBin(
  palette = c("#FFFF99", "#93C94B", "#4CA840", "#1F7A2E", "#0A4A1A"),
  domain   = mapa_cuerna$participacion,
  bins     = breaks_par,
  na.color = "#cccccc"
)


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_cuerna, color = 'black', weight = 0.5) %>%
  
  # --- Layer 1 ---
  addPolygons(
    data        = mapa_cuerna,
    fillColor   = ~pal(propensity_ops),
    group       = 'Propensión a Votar por la Oposición',
    weight = 0.51, opacity = 1, color = "black", fillOpacity = 0.6,
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px", direction = "auto"),
    highlightOptions = highlightOptions(weight = 1, color = "blue",
                                        fillOpacity = 1, bringToFront = TRUE)
  ) %>%
  addLegend(
    layerId  = "legend_resultados",          # <-- named ID
    position = "bottomright",
    pal      = pal,
    values   = breaks,
    title    = "Propensión a Votar por la Oposición",
    opacity  = 0.85, 
    group = 'Propensión a Votar por la Oposición'
  ) %>% # --- Layer 2 ---
  addPolygons(
    data        = mapa_cuerna,
    fillColor   = ~pal_par(participacion),
    group       = 'Participación',
    weight = 0.51, opacity = 1, color = "black", fillOpacity = 0.6,
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px", direction = "auto"),
    highlightOptions = highlightOptions(weight = 1, color = "blue",
                                        fillOpacity = 1, bringToFront = TRUE)
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = pal_par,
    values   = breaks_par,
    title    = "Participación",
    opacity  = 0.85,
    group = 'Participación' ) %>%
  addLayersControl(
    overlayGroups = c("Propensión a Votar por la Oposición", "Participación"),
    options    = layersControlOptions(collapsed = FALSE)
  )


# Plotcitos ---------------------------------------------------------------
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

mapa_cuerna %>% ggplot(aes(participacion, tot_morena)) +
  geom_hline(yintercept = 40)+
  geom_point(aes(color = candidatura_ganadora))+
  theme_minimal()+
  labs(x = 'Porcentaje de Voto por Morena', y = 'Porcentaje de Participación')


# Revised priorities ------------------------------------------------------

final_priority <- mapa_cuerna %>% filter(!is.na(priority)) %>% slice_max(order_by = lista_nominal, n=40)

mapa_cuerna <- mapa_cuerna %>% 
  filter(!seccion %in% final_priority$seccion) %>% 
  mutate(priority = NA) %>% 
  rbind(final_priority)

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












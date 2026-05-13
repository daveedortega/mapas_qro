## Revisión Muestra Neza
# 07/05/2026

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, writexl, sf, leaflet, htmltools)
rm(list = ls())

# Cargar Datos ------------------------------------------------------------

muestra_neza <- read_xlsx('input/neza/muestra_sist_estrat_munip_neza_80_val.xlsx')
mapa_neza <- readRDS('../preplike_shiny/data/mapa_mun24_neza.rds')

# Analisis ----------------------------------------------------------------

mapa_neza <- mapa_neza %>% mutate(seccion_polled = ifelse(seccion %in% muestra_neza$SECCION, 1, 0))


mapa_neza %>% ggplot()+
  geom_sf(aes(fill = factor(seccion_polled)))


# Crear Mapa Dinámico -----------------------------------------------------

ci     <- classInt::classIntervals(var = mapa_neza$tot_morena, n = 5, style = "jenks")
breaks <- ci$brks
pal <- colorBin(
  palette  = c("#FFFF00", "#FFC300", "#FF8C00", "#E84A0C", "#C0392B"),
  domain   = mapa_neza$tot_morena,
  bins     = breaks,
  na.color = "#cccccc"
)

pal_binary <- colorFactor(
  palette  = c("1" = "firebrick", "0" = "#457B9D"),
  domain   = mapa_neza$seccion_polled,
  na.color = "#cccccc"
)

labels <- labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  mapa_neza$seccion, mapa_neza$lista_nominal, mapa_neza$total_votos, mapa_neza$morena, mapa_neza$tot_morena
) %>% lapply(htmltools::HTML)


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_neza, color = "black", weight = 0.5, fill = FALSE) |>
  addPolygons(data = mapa_neza, 
    fillColor        = ~pal(tot_morena),
    weight           = 0.5,
    color            = "black",
    opacity          = 1,
    fillOpacity      = 0.6,
    group = 'resultados', 
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    highlightOptions = highlightOptions(
      weight      = 1.5,
      color       = "blue",
      fillOpacity = 1,
      bringToFront = TRUE
    )
  ) %>% 
  addPolygons(data = mapa_neza, 
              fillColor        = ~pal_binary(seccion_polled),
              group = 'prioritaria', 
              weight           = 0.5,
              color            = "black",
              opacity          = 1,
              fillOpacity      = 0.6,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"), 
              highlightOptions = highlightOptions(
                weight      = 1.5,
                color       = "blue",
                fillOpacity = 1,
                bringToFront = TRUE
              )
  ) %>% 
  addLayersControl(
    baseGroups = c("resultados", "prioritaria"),  # radio buttons, only one visible at a time
    options = layersControlOptions(collapsed = FALSE)
  )

# División por Importancia ------------------------------------------------

# Potencial de votos? 

mapa_neza %>% glimpse()
  
mapa_neza <- mapa_neza %>% mutate(participacion = total_votos/lista_nominal*100) 

lm(tot_morena~participacion,data= mapa_neza)

mapa_neza %>% glimpse()

mapa_neza %>% as_tibble() %>% 
  ggplot(aes(tot_morena, participacion))+
  geom_point(aes( color = candidatura_ganadora, size = lista_nominal))+
  geom_smooth(se = F)+
  theme_minimal()+
  # scale_color_manual(values = c('morena' = 'firebrick', 'cc_pan_pri_prd_naem' = 'blue4'))+
  labs(x = 'Porcentaje de Votos capturados por Morena', y = 'Participación Electoral')



mapa_neza <- mapa_neza %>% mutate(tot_ops = 100-tot_morena)

quantile(mapa_neza$tot_ops, probs = seq(0,1,0.001))

mapa_neza %>% ggplot(aes(tot_morena, participacion))+
  geom_point(aes(color = candidatura_ganadora))+
  geom_smooth(se = F, color = 'black')+
  theme_minimal()+
  # scale_color_manual(values = c('morena' = 'firebrick', 'cc_pan_pri_prd_naem' = 'blue4'))+
  labs(x = 'Porcentaje de Votos Por Morena', y = 'Participación Electoral')

# Propensity to Morena ----------------------------------------------------

mapa_neza %>% mutate(proprnsity_morena = participacion * (tot_morena/100)) %>% 
  select(proprnsity_morena) %>% as_tibble() %>% 
  ggplot(aes(x = proprnsity_morena))+
  geom_density()


mapa_neza <- mapa_neza %>% mutate(propensity_ops = participacion * (tot_ops/100)) 

ci <- classInt::classIntervals(var = mapa_neza$propensity_ops, n = 5, style = "jenks")

breaks <- ci$brks
pal <- colorBin(
  palette  = c("#C6DBF0", "#9ECAE1", "#6AAED6", "#2171B5", "#08306B"),
  domain   = mapa_neza$propensity_ops,
  bins     = breaks,
  na.color = "#cccccc"
)



labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Participación: %g %% <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  mapa_neza$seccion, mapa_neza$lista_nominal, mapa_neza$participacion, mapa_neza$morena, mapa_neza$tot_morena
) %>% lapply(htmltools::HTML)



pal_binary <- colorFactor(
  palette  = c("1" = "firebrick", "0" = "#457B9D"),
  domain   = mapa_neza$seccion_polled,
  na.color = "#cccccc"
)

# Feo
lm(seccion_polled~propensity_ops+cand, data = mapa_neza %>% mutate(cand = ifelse(candidatura_ganadora == 'morena', 0, 1)))
# 
# leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(data = mapa_neza, color = 'black', weight = 0.5) %>%
#   
#   # --- Layer 1 ---
#   addPolygons(
#     data        = mapa_neza,
#     fillColor   = ~pal(propensity_ops),
#     group       = 'resultados',
#     weight = 0.51, opacity = 1, color = "black", fillOpacity = 0.6,
#     label = labels,
#     labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
#                                 textsize = "15px", direction = "auto"),
#     highlightOptions = highlightOptions(weight = 1, color = "blue",
#                                         fillOpacity = 1, bringToFront = TRUE)
#   ) %>%
#   addLegend(
#     layerId  = "legend_resultados",          # <-- named ID
#     position = "bottomright",
#     pal      = pal,
#     values   = breaks,
#     title    = "Propensión a Votar por la Oposición",
#     opacity  = 0.85
#   ) %>%
#   
#   # --- Layer 2 ---
#   addPolygons(
#     data      = mapa_neza,
#     fillColor = ~pal_binary(seccion_polled),
#     group     = 'prioritaria',
#     weight = 0.5, opacity = 1, color = "black", fillOpacity = 0.6,
#     label = labels,
#     labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
#                                 textsize = "15px", direction = "auto"),
#     highlightOptions = highlightOptions(weight = 1.5, color = "blue",
#                                         fillOpacity = 1, bringToFront = TRUE)
#   ) %>%
#   addLegend(
#     layerId  = "legend_prioritaria",         # <-- named ID
#     position = "bottomright",
#     pal      = pal_binary,
#     values   = mapa_neza$seccion_polled,
#     title    = "Sección Prioritaria",
#     opacity  = 0.85
#   ) 


mapa_neza %>% mutate(prioritaria = ifelse(propensity_ops>=40, 1, 0)) %>% 
  group_by(prioritaria) %>% summarise(mean(lista_nominal))

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_neza, color = 'black', weight = 0.5) %>%
  
  # --- Layer 1 ---
  addPolygons(
    data        = mapa_neza,
    fillColor   = ~pal(propensity_ops),
    group       = 'resultados',
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
    opacity  = 0.85
  ) 
  

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_neza, color = 'black', weight = 0.5) %>%
  
  # --- Layer 1 ---
  addPolygons(
    data        = mapa_neza,
    fillColor   = ~pal(propensity_ops),
    group       = 'resultados',
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
    opacity  = 0.85
  ) 
  

# Pintamos Participación --------------------------------------------------


mapa_neza <- mapa_neza %>% mutate(propensity_ops = participacion * (tot_ops/100)) 

ci <- classInt::classIntervals(var = mapa_neza$propensity_ops, n = 5, style = "jenks")

breaks <- ci$brks
pal <- colorBin(
  palette  = c("#C6DBF0", "#9ECAE1", "#6AAED6", "#2171B5", "#08306B"),
  domain   = mapa_neza$propensity_ops,
  bins     = breaks,
  na.color = "#cccccc"
)



labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Participación: %g %% <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  mapa_neza$seccion, mapa_neza$lista_nominal, mapa_neza$participacion, mapa_neza$morena, mapa_neza$tot_morena
) %>% lapply(htmltools::HTML)


ci <- classInt::classIntervals(var = mapa_neza$participacion, n = 5, style = "jenks")

breaks_par <- ci$brks
pal_par <- colorBin(
  palette = c("#FFFF99", "#93C94B", "#4CA840", "#1F7A2E", "#0A4A1A"),
  domain   = mapa_neza$participacion,
  bins     = breaks_par,
  na.color = "#cccccc"
)


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_neza, color = 'black', weight = 0.5) %>%
  
  # --- Layer 1 ---
  addPolygons(
    data        = mapa_neza,
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
    data        = mapa_neza,
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

# Selección del top 20 por grupo


quantile(mapa_neza$participacion, probs = seq(0,1,0.01))
# 61.08313 to less


quantile(mapa_neza$propensity_ops, probs = seq(0,1,0.01))
# 33.61439 upwards

mapa_neza <- mapa_neza %>% mutate(prioritario_part = ifelse(participacion<=61, 'Baja Participación', NA), 
                     prioritario_ops = ifelse(propensity_ops>=33.6, 'Top 20% Oposición', NA), 
                     ) %>% 
  mutate(priority = paste(prioritario_part, prioritario_ops, sep = '-')) %>% 
  mutate(priority = ifelse(is.na(prioritario_part) & is.na(prioritario_ops), NA, priority)) %>% 
  # filter(!is.na(priority)) %>% 
  mutate(priority = str_remove(priority, '-NA'), 
         priority = str_remove(priority, 'NA-'), 
         ) 

mapa_neza %>% count(priority)

pal_binary <- colorFactor(
  palette  = c("Baja Participación" = "green4", 
               'Baja Participación-Top 20% Oposición' = 'firebrick',
               "Top 20% Oposición" = "#457B9D"
               ),
  domain   = mapa_neza$priority,
  na.color = "#cccccc"
)


labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Participación: %g %% <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  mapa_neza$seccion, mapa_neza$lista_nominal, mapa_neza$participacion, mapa_neza$morena, mapa_neza$tot_morena
) %>% lapply(htmltools::HTML)




leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_neza, color = 'black', weight = 0.5) %>%
  
  # --- Layer 1 ---
  addPolygons(
    data        = mapa_neza,
    fillColor   = ~pal_binary(priority),
    group       = 'Tipo de Prioridad',
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
    pal      = pal_binary,
    values   = mapa_neza$priority,
    title    = "Propensión a Votar por la Oposición",
    opacity  = 0.85, 
    group = 'Propensión a Votar por la Oposición'
  )

# Desglose menos secciones ------------------------------------------------


toptoptop <- mapa_neza %>% filter(!is.na(priority)) %>% group_by(priority) %>% slice_max(order_by = lista_nominal, n = 80)

mapa_neza %>% filter(!seccion %in% toptoptop$seccion)

secciones_superpro <- mapa_neza %>% mutate(priority = NA) %>% filter(!seccion %in% toptoptop$seccion) %>% 
  rbind(toptoptop)



pal_binary <- colorFactor(
  palette  = c("Baja Participación" = "green4", 
               'Baja Participación-Top 20% Oposición' = 'firebrick',
               "Top 20% Oposición" = "#457B9D"
  ),
  domain   = secciones_superpro$priority,
  na.color = "#cccccc"
)


labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Participación: %g %% <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  secciones_superpro$seccion, secciones_superpro$lista_nominal, secciones_superpro$participacion, secciones_superpro$morena, secciones_superpro$tot_morena
) %>% lapply(htmltools::HTML)



secciones_superpro %>% count(priority)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = secciones_superpro, color = 'black', weight = 0.5) %>%
  
  # --- Layer 1 ---
  addPolygons(
    data        = secciones_superpro,
    fillColor   = ~pal_binary(priority),
    group       = 'Tipo de Prioridad',
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
    pal      = pal_binary,
    values   = mapa_neza$priority,
    title    = "Propensión a Votar por la Oposición",
    opacity  = 0.85, 
    group = 'Propensión a Votar por la Oposición'
  )

# Desglose menos secciones ------------------------------------------------

#160 cap

muestra_neza

toptoptop <- mapa_neza %>% filter(!is.na(priority), !seccion %in% muestra_neza$SECCION) %>% 
  slice_max(order_by = lista_nominal, n = 80) %>% mutate(priority = 'Sección Recomendada')
secciones_muestra <- mapa_neza %>% filter(seccion %in% muestra_neza$SECCION)

mapa_neza %>% filter(!seccion %in% toptoptop$seccion)

secciones_superpro <- mapa_neza %>% mutate(priority = NA) %>% 
  filter(!seccion %in% toptoptop$seccion) %>% 
  filter(!seccion %in% secciones_muestra$seccion) %>% 
  rbind(toptoptop) %>% 
  rbind(secciones_muestra %>% mutate(priority = 'Sección de Muestreo'))

secciones_superpro %>% count(priority)

pal_binary <- colorFactor(
  palette  = c("Sección Recomendada" = "green4", 
               "Sección de Muestreo" = "lightpink"
  ),
  domain   = secciones_superpro$priority,
  na.color = "#cccccc"
)


labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Participación: %g %% <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  secciones_superpro$seccion, secciones_superpro$lista_nominal, secciones_superpro$participacion, secciones_superpro$morena, secciones_superpro$tot_morena
) %>% lapply(htmltools::HTML)



secciones_superpro %>% count(priority)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = secciones_superpro, color = 'black', weight = 0.5) %>%
  
  # --- Layer 1 ---
  addPolygons(
    data        = secciones_superpro,
    fillColor   = ~pal_binary(priority),
    group       = 'Tipo de Prioridad',
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
    pal      = pal_binary,
    values   = secciones_superpro$priority,
    title    = "Propensión a Votar por la Oposición",
    opacity  = 0.85, 
    group = 'Propensión a Votar por la Oposición'
  )





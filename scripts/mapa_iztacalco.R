## Mapa Iztacalco
# DAOA
# 2025/05/17

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, sf, leaflet)
rm(list = ls())
dev.off()
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()

# Cargar Datos ------------------------------------------------------------

mapa_iztacalco <- read_sf('input/cdmx/secciones_iztacalco/mapa_secciones_iztacalco.shp')
colonias_cdmx <- read_sf('~/Desktop/Mapas/COLONIAS_IECM/mgpc_2019.shp')

colonias_iztacalco <- colonias_cdmx %>% filter(NOMDT == 'IZTACALCO') %>% 
  st_make_valid() %>% st_transform(st_crs(mapa_iztacalco))

mapa_iztacalco <- mapa_iztacalco %>% mutate(seccion = as.numeric(str_squish(str_remove(Name, 'SECCIÓN'))))
mapa_iztacalco <- mapa_iztacalco %>% select(seccion) %>% mutate(municipio = 'IZTACALCO')

# Resultados electorales

res_cdmx <- read_xlsx('input/cdmx/bd2024alccas.xlsx', skip = 6) %>% clean_names()


res_cdmx %>% glimpse()

res_cdmx <- res_cdmx %>% group_by(municipio = demarcacion_territorial, seccion = seccion_electoral) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))

res_cdmx <- res_cdmx %>% mutate(tot_morena = pvem_pt_morena/votos_totales * 100, 
                    prianrd = pan_pri_prd + pan_pri+ pan_prd+ pri_prd + pan +pri + prd, 
                    tot_ops = prianrd/votos_totales * 100, 
                    participacion = votos_totales/lista_nominal * 100)

res_cdmx <- res_cdmx %>% rename(total_votos = votos_totales)

# Pegar resultados --------------------------------------------------------

mapa_iztacalco <- mapa_iztacalco %>% left_join(res_cdmx %>% ungroup() %>% select(-municipio))

# Centroides para Google Maps -----------------------------------------------

centroids <- mapa_iztacalco |>
  st_transform(4326) |>
  st_centroid() |>
  st_coordinates() |>
  as_tibble() |>
  rename(cent_lng = X, cent_lat = Y)

mapa_iztacalco <- mapa_iztacalco |>
  bind_cols(centroids) |>
  mutate(gmap_url = paste0("https://www.google.com/maps?q=", cent_lat, ",", cent_lng))

# Crear Mapa Dinámico -----------------------------------------------------

ci     <- classInt::classIntervals(var = mapa_iztacalco$tot_morena, n = 5, style = "jenks")
breaks <- ci$brks
pal <- colorBin(
  palette  = c("#FFFF00", "#FFC300", "#FF8C00", "#E84A0C", "#C0392B"),
  domain   = mapa_iztacalco$tot_morena,
  bins     = breaks,
  na.color = "#cccccc"
)


labels <- sprintf(
  "Sección <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  mapa_iztacalco$seccion, mapa_iztacalco$lista_nominal, mapa_iztacalco$total_votos, mapa_iztacalco$pvem_pt_morena, mapa_iztacalco$tot_morena
) %>% lapply(htmltools::HTML)

popups <- sprintf(
  '<div style="font-family: sans-serif; font-size: 13px; line-height: 1.8; min-width: 180px;">
     <strong style="font-size: 14px;">Sección %s</strong><br/>
     Lista Nominal: %g<br/>
     Votos Emitidos: %g<br/>
     Votos Morena: %g<br/>
     Porcentaje de Morena: <strong>%g%%</strong><br/>
     <hr style="margin: 6px 0;"/>
     <a href="%s" target="_blank" style="color: #E84A0C; text-decoration: none;">
       &#x1F4CD; Ver en Google Maps
     </a>
   </div>',
  mapa_iztacalco$seccion, mapa_iztacalco$lista_nominal, mapa_iztacalco$total_votos,
  mapa_iztacalco$pvem_pt_morena, mapa_iztacalco$tot_morena, mapa_iztacalco$gmap_url
) %>% lapply(htmltools::HTML)


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_iztacalco, color = "black", weight = 0.5, fill = FALSE) |>
  addPolygons(data = mapa_iztacalco, 
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
              ),
              popup = popups
  ) %>% 
  addLegend(position = "bottomright",
            pal      = pal,
            values   = breaks,
            title    = "Tipo de Sección",
            opacity  = 1)

# Agregamos Totales por colonia -------------------------------------------

mapa_iztacalco <- mapa_iztacalco %>% st_join(colonias_iztacalco %>% select(NOMDT, NOMUT)) 


resumen_colonias <- mapa_iztacalco %>% st_drop_geometry() %>% 
  select(NOMDT, NOMUT, pan:lista_nominal) %>% 
  group_by(NOMDT, NOMUT) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  mutate(tot_morena = pvem_pt_morena/total_votos * 100,
         prianrd = pan_pri_prd + pan_pri+ pan_prd+ pri_prd + pan +pri + prd, 
         tot_ops = prianrd/total_votos * 100, 
         participacion = total_votos/lista_nominal * 100)

resumen_colonias <- colonias_iztacalco %>% left_join(resumen_colonias)


centroides_colonias <- resumen_colonias |>
  st_transform(4326) |>
  st_centroid() |>
  st_coordinates() |>
  as_tibble() |>
  rename(cent_lng = X, cent_lat = Y)

resumen_colonias <- resumen_colonias |>
  bind_cols(centroides_colonias) |>
  mutate(gmap_url = paste0("https://www.google.com/maps?q=", cent_lat, ",", cent_lng))


ci_col     <- classInt::classIntervals(var = resumen_colonias$tot_morena, n = 5, style = "jenks")
breaks_col <- ci_col$brks
pal_col <- colorBin(
  palette  = c("#FFFF00", "#FFC300", "#FF8C00", "#E84A0C", "#C0392B"),
  domain   = resumen_colonias$tot_morena,
  bins     = breaks_col,
  na.color = "#cccccc"
)

label_col <- sprintf(
  "Colonia: <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  resumen_colonias$NOMUT, resumen_colonias$lista_nominal, resumen_colonias$total_votos, resumen_colonias$pvem_pt_morena, resumen_colonias$tot_morena
) %>% lapply(htmltools::HTML)

popups_colonias <- sprintf(
  '<div style="font-family: sans-serif; font-size: 13px; line-height: 1.8; min-width: 180px;">
     <strong style="font-size: 14px;">Sección %s</strong><br/>
     Lista Nominal: %g<br/>
     Votos Emitidos: %g<br/>
     Votos Morena: %g<br/>
     Porcentaje de Morena: <strong>%g%%</strong><br/>
     <hr style="margin: 6px 0;"/>
     <a href="%s" target="_blank" style="color: #E84A0C; text-decoration: none;">
       &#x1F4CD; Ver en Google Maps
     </a>
   </div>',
  resumen_colonias$NOMUT, resumen_colonias$lista_nominal, resumen_colonias$total_votos,
  resumen_colonias$pvem_pt_morena, resumen_colonias$tot_morena, resumen_colonias$gmap_url
) %>% lapply(htmltools::HTML)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = mapa_iztacalco,
              fillColor        = ~pal(tot_morena),
              weight           = 0.5,
              color            = "black",
              opacity          = 1,
              fillOpacity      = 0.6,
              group = 'secciones',
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
              ),
              popup = popups
  ) %>%
  addLegend(position = "bottomright",
            pal      = pal,
            values   = breaks,
            title    = "Porcentaje de Voto por Morena",
            opacity  = 1,
            group    = 'secciones',
            className = "legend-secciones info legend") %>%
  addPolygons(data = resumen_colonias, 
              fillColor        = ~pal_col(tot_morena),
              weight           = 0.5,
              color            = "black",
              opacity          = 1,
              fillOpacity      = 1,
              group = 'colonias', 
              label = label_col,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight      = 1.5,
                color       = "blue",
                fillOpacity = 1,
                bringToFront = TRUE
              ),
              popup = popups_colonias) %>% 
  addLegend(position = "bottomright",
            pal      = pal_col,
            values   = breaks_col,
            title    = "Porcentaje de Voto por Morena",
            opacity  = 1, 
            group    = 'colonias') %>%
  addLayersControl(overlayGroups = c('colonias', 'secciones'), 
                   options = layersControlOptions(collapsed = FALSE))


# Subselección de Colonias ------------------------------------------------

top_colonias <- resumen_colonias %>%
  mutate(proporcion_total_colonia = lista_nominal/sum(lista_nominal) * 100) %>% 
  slice_max(order_by = lista_nominal, n = 5, with_ties = F)

top_secciones <- mapa_iztacalco %>% filter(NOMUT %in% top_colonias$NOMUT) %>% group_by(NOMUT) %>% 
  slice_max(n = 5, order_by = lista_nominal, with_ties = F)

# Colonia
ci_ln_col <- classInt::classIntervals(var = top_colonias$lista_nominal, n = 5, style = "jenks")

breaks_ln_col <- ci_ln_col$brks

pal_ln_col <- colorBin(
  palette  = c("#C6DBF0", "#9ECAE1", "#6AAED6", "#2171B5", "#08306B"),
  domain   = top_colonias$lista_nominal,
  bins     = breaks_ln_col,
  na.color = "#cccccc"
)

label_col_ln <- sprintf(
  "Colonia: <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  top_colonias$NOMUT, top_colonias$lista_nominal, top_colonias$total_votos, top_colonias$pvem_pt_morena, top_colonias$tot_morena
) %>% lapply(htmltools::HTML)

popups_colonias_ln <- sprintf(
  '<div style="font-family: sans-serif; font-size: 13px; line-height: 1.8; min-width: 180px;">
     <strong style="font-size: 14px;">Sección %s</strong><br/>
     Lista Nominal: %g<br/>
     Votos Emitidos: %g<br/>
     Votos Morena: %g<br/>
     Porcentaje de Morena: <strong>%g%%</strong><br/>
     <hr style="margin: 6px 0;"/>
     <a href="%s" target="_blank" style="color: #E84A0C; text-decoration: none;">
       &#x1F4CD; Ver en Google Maps
     </a>
   </div>',
  top_colonias$NOMUT, top_colonias$lista_nominal, top_colonias$total_votos,
  top_colonias$pvem_pt_morena, top_colonias$tot_morena, top_colonias$gmap_url
) %>% lapply(htmltools::HTML)

# Secciones
ci_ln_sec <- classInt::classIntervals(var = top_secciones$lista_nominal, n = 5, style = "jenks")

breaks_ln_sec <- ci_ln_sec$brks

pal_ln_sec <- colorBin(
  palette  = c("#C6DBF0", "#9ECAE1", "#6AAED6", "#2171B5", "#08306B"),
  domain   = ci_ln_sec$lista_nominal,
  bins     = breaks_ln_sec,
  na.color = "#cccccc"
)

label_sec_ln <- sprintf(
  "Sección: <strong>%s</strong><br/> Lista Nominal: %g<br/> Votos Emitidos: %g <br/> Votos Morena: %g<br/>Porcentaje de Morena: %g %%",
  top_secciones$seccion, top_secciones$lista_nominal, top_secciones$total_votos, top_secciones$pvem_pt_morena, top_secciones$tot_morena
) %>% lapply(htmltools::HTML)

popups_ln_sec <- sprintf(
  '<div style="font-family: sans-serif; font-size: 13px; line-height: 1.8; min-width: 180px;">
     <strong style="font-size: 14px;">Sección %s</strong><br/>
     Lista Nominal: %g<br/>
     Votos Emitidos: %g<br/>
     Votos Morena: %g<br/>
     Porcentaje de Morena: <strong>%g%%</strong><br/>
     <hr style="margin: 6px 0;"/>
     <a href="%s" target="_blank" style="color: #E84A0C; text-decoration: none;">
       &#x1F4CD; Ver en Google Maps
     </a>
   </div>',
  top_secciones$seccion, top_secciones$lista_nominal, top_secciones$total_votos,
  top_secciones$pvem_pt_morena, top_secciones$tot_morena, top_secciones$gmap_url
) %>% lapply(htmltools::HTML)

# Final -------------------------------------------------------------------


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = top_colonias, 
              fillColor        = ~pal_ln_col(lista_nominal),
              weight           = 0.5,
              color            = "black",
              opacity          = 1,
              fillOpacity      = 1,
              group = 'colonias importantes', 
              label = label_col_ln,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight      = 1.5,
                color       = "blue",
                fillOpacity = 1,
                bringToFront = TRUE
              ), 
              popup = popups_colonias_ln) %>% 
  addPolygons(data = top_secciones, 
              fillColor        = ~pal_ln_sec(lista_nominal),
              weight           = 0.5,
              color            = "black",
              opacity          = 1,
              fillOpacity      = 1,
              group = 'secciones importantes', 
              label = label_sec_ln,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight      = 1.5,
                color       = "blue",
                fillOpacity = 1,
                bringToFront = TRUE
              ), 
              popup = popups_ln_sec) %>% 
  addPolygons(data = mapa_iztacalco,
              fillColor        = ~pal(tot_morena),
              weight           = 0.5,
              color            = "black",
              opacity          = 1,
              fillOpacity      = 0.6,
              group = 'secciones',
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
              ),
              popup = popups
  ) %>%
  addPolygons(data = resumen_colonias,
              fillColor        = ~pal_col(tot_morena),
              weight           = 0.5,
              color            = "black",
              opacity          = 1,
              fillOpacity      = 1,
              group = 'colonias',
              label = label_col,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(
                weight      = 1.5,
                color       = "blue",
                fillOpacity = 1,
                bringToFront = TRUE
              ),
              popup = popups_colonias) %>%
  addLegend(position = "bottomright",
            pal      = pal,
            values   = breaks,
            title    = "Porcentaje de Voto por Morena Secciones",
            opacity  = 1,
            group    = 'secciones',
            className = "legend-secciones info legend") %>%
  addLegend(position = "bottomright",
            pal      = pal_col,
            values   = breaks_col,
            title    = "Porcentaje de Voto por Morena Colonias",
            opacity  = 1,
            group    = 'colonias') %>%
  addLegend(position = "bottomright",
            pal      = pal_ln_col,
            values   = breaks_ln_col,
            title    = "Lista Nominal Colonias",
            opacity  = 1,
            group    = 'colonias importantes') %>%
  addLegend(position = "bottomright",
            pal      = pal_ln_sec,
            values   = breaks_ln_sec,
            title    = "Lista Nominal Secciones",
            opacity  = 1,
            group    = 'secciones importantes') %>%
  addLayersControl(overlayGroups = c('colonias', 'secciones', 'colonias importantes', 'secciones importantes'), 
                   options = layersControlOptions(collapsed = FALSE))









  
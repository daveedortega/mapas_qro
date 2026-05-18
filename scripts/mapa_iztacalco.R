## Mapa Iztacalco
# DAOA
# 2025/05/17

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor, readxl, sf)
rm(list = ls())
dev.off()
sysfonts::font_add_google('Montserrat')
showtext::showtext_auto()

# Cargar Datos ------------------------------------------------------------

mapa_iztacalco <- read_sf('input/cdmx/secciones_iztacalco/mapa_secciones_iztacalco.shp')

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
              )
  ) %>% 
  addLegend(position = "bottomright",
            pal      = pal,
            values   = breaks,
            title    = "Tipo de Sección",
            opacity  = 1)



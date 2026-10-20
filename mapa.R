# ---- 1. Pacotes (precisam ser carregados toda vez que abrir o RStudio) ----
library(tibble)
library(leaflet)
library(htmlwidgets)

# ---- 2. Pontos de coleta: UMA LINHA POR COLETA ----
# Valores de exemplo: troque pelas suas coordenadas reais
pontos <- tribble(
  ~id,   ~lat,     ~lon,     ~especie, ~data,        ~obs,
  "P01", -30.069756, -51.141647, "",  "20/02/2026", "no concreto do chao do estacionamento da fito",
  "P02", -30.069522, -51.141634, "",   "20/02/2026", "",
  "P03", -30.069683, -51.141571, "",  "20/02/2026", "ao lado de um poste amarelo, a entrada contem três folhas de gramineas",
  "P04", -30.069985, -51.141378, "",  "20/02/2026", "sob areia no estacionamento da fito",
  "P05", -30.069937, -51.141509, "",  "20/02/2026", "entre lajotas no estacionamento da fito",
  "P06", -30.069754, -51.141388, "",  "20/02/2026", "entre lajotas, na calçada do lado da rua do DHS/DEPLAV",
  "P07", -30.069885, -51.141728, "",  "20/02/2026", "",
  "P08", -30.069848, -51.141709, "",  "20/02/2026", "",
  "P09", -30.069885, -51.141901, "",  "20/02/2026", "",
  "P10", -30.069905, -51.141232, "",  "20/02/2026", "",
  "P11", -30.070023, -51.140789, "",  "20/02/2026", "",
  "P12", -30.070614, -51.140226, "",  "07/05/2026", "",
  "P13", -30.070311, -51.140474, "",  "07/05/2026", "",
  "P14", -30.070777, -51.139732, "",  "07/05/2026", "",
  "P14", -30.070699, -51.140531, "",  "07/05/2026", ""
)

# ---- 3. Mapa ----
mapa <- leaflet(pontos) |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") |>
  addTiles(group = "Ruas") |>
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 7, color = "white", weight = 2,
    fillColor = "red", fillOpacity = 0.9,
    label = ~id,
    popup = ~paste0("<b>", id, "</b><br>Especie: ", especie,
                    "<br>Data: ", data, "<br>", obs)
  ) |>
  fitBounds(min(pontos$lon), min(pontos$lat),
            max(pontos$lon), max(pontos$lat),
            options = list(maxZoom = 18)) |>
  addLayersControl(baseGroups = c("Satélite", "Ruas")) |>
  addScaleBar()

mapa
saveWidget(mapa, "mapa_coletas.html", selfcontained = TRUE)
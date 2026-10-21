# ---- 1. Pacotes (precisam ser carregados toda vez que abrir o RStudio) ----
library(tibble)
library(leaflet)
library(htmlwidgets)
library(base64enc)

# ---- 2. Pontos de coleta: UMA LINHA POR COLETA ----
# cor: nome em inglês ("red", "orange", "blue"...) ou código hexadecimal ("#B57EDC")
pontos <- tribble(
  ~id,   ~lat,       ~lon,       ~especie, ~data,        ~cor,      ~foto,     ~obs,
  #"P01", -30.069756, -51.141647, "",       "20/02/2026", "red",     "",        "no concreto do chao do estacionamento da fito",
  "P01", -30.069522, -51.141634, "",       "20/02/2026", "red",     "P01.jpg", "entre lajotas no estacionamento da fito",
  "P02", -30.069683, -51.141571, "",       "20/02/2026", "red",     "P02.jpg", "ao lado de um poste amarelo, a entrada contem três folhas de gramineas",
  "P03", -30.069985, -51.141378, "",       "20/02/2026", "red",     "P03.jpg", "sob areia no estacionamento da fito",
  "P04", -30.069937, -51.141509, "",       "20/02/2026", "red",     "P04.jpg", "entre lajotas no estacionamento da fito",
  "P05", -30.069754, -51.141388, "",       "20/02/2026", "red",     "P05.jpg", "entre lajotas, na calçada do lado da rua do DHS/DEPLAV",
  "P06", -30.069885, -51.141728, "",       "20/02/2026", "red",     "P06.jpg", "",
  "P07", -30.069848, -51.141709, "",       "20/02/2026", "red",     "P07.jpg", "",
  "P08", -30.069885, -51.141901, "",       "20/02/2026", "red",     "P08.jpg", "",
  "P09", -30.069905, -51.141232, "",       "20/02/2026", "red",     "P09.jpg", "",
  "P10", -30.069521, -51.141354, "",       "20/02/2026", "red",     "P10.jpg", "",
  "P11", -30.070023, -51.140789, "",       "20/02/2026", "red",     "P11.jpg", "",
  "P12", -30.070614, -51.140226, "",       "07/05/2026", "#B57EDC", "P12.jpg", "",
  "P13", -30.070311, -51.140474, "",       "07/05/2026", "#B57EDC", "P13.jpg", "",
  "P14", -30.070777, -51.139732, "",       "07/05/2026", "#B57EDC", "P14.jpg", "",
  "P15", -30.070699, -51.140531, "",       "07/05/2026", "#B57EDC", "P15.jpg", ""
)

# ---- 3. Foto no popup ----
monta_foto <- function(arquivo) {
  caminho <- file.path("fotos", arquivo)
  if (is.na(arquivo) || arquivo == "") return("")
  if (!file.exists(caminho)) {
    message("Foto não encontrada: ", caminho)
    return("")
  }
  tipo <- if (grepl("\\.png$", arquivo, ignore.case = TRUE)) "image/png" else "image/jpeg"
  uri  <- dataURI(file = caminho, mime = tipo)
  paste0("<br><img src='", uri, "' width='250'>")
}
pontos$foto_html <- vapply(pontos$foto, monta_foto, character(1))

# ---- 3b. Confere as cores (cor vazia ou inválida vira cinza) ----
confere_cor <- function(x) {
  if (is.na(x) || x == "") return("#808080")
  tryCatch(
    rgb(t(col2rgb(x)), maxColorValue = 255),
    error = function(e) {
      message("Cor inválida: '", x, "' (o ponto ficou cinza)")
      "#808080"
    }
  )
}
pontos$cor <- vapply(pontos$cor, confere_cor, character(1), USE.NAMES = FALSE)

# Legenda: uma entrada para cada combinação de data e cor
legenda <- unique(pontos[, c("data", "cor")])

# ---- 4. Mapa ----
mapa <- leaflet(pontos) |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") |>
  addTiles(group = "Ruas") |>
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 7, color = "white", weight = 2,
    fillColor = ~cor, fillOpacity = 0.9,
    label = ~id,
    popup = ~paste0("<b>", id, "</b><br>Especie: ", especie,
                    "<br>Data: ", data, "<br>", obs, foto_html),
    popupOptions = popupOptions(maxWidth = 300)
  ) |>
  addLegend(
    position = "bottomright",
    colors = legenda$cor,
    labels = legenda$data,
    title = "Data da coleta",
    opacity = 1
  ) |>
  fitBounds(min(pontos$lon), min(pontos$lat),
            max(pontos$lon), max(pontos$lat),
            options = list(maxZoom = 18)) |>
  addLayersControl(baseGroups = c("Satélite", "Ruas")) |>
  addScaleBar()

mapa
saveWidget(mapa, "mapa_coletas.html", selfcontained = TRUE)

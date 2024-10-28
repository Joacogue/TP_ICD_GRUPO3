tarifa_usd <- read_csv("tarifa-media-diaria-en-usd-por-region-y-categoria.csv", locale = locale(decimal_mark = ","))
tarifa_usd$indice_tiempo <- as.Date(tarifa_usd$indice_tiempo, format = "%Y-%m-%d")
tarifa_usd <- tarifa_usd %>%
  mutate(tarifa_media_diaria_en_usd = as.numeric(gsub(",", ".", tarifa_media_diaria_en_usd)))
tarifa_usd <- tarifa_usd %>%
  mutate(trimestre = paste0(year(indice_tiempo), "-Q", quarter(indice_tiempo)))
tarifa_usd <- tarifa_usd %>%
  group_by(trimestre, region_de_destino, categoria_del_hotel) %>%
  summarize(
    media = mean(tarifa_media_diaria_en_usd, na.rm = TRUE),
    mediana = median(tarifa_media_diaria_en_usd, na.rm = TRUE),
    dispersion = sd(tarifa_media_diaria_en_usd, na.rm = TRUE)
  )




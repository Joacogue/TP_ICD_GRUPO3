tasas <- read_csv("tasas-de-ocupacion-plazas-por-region-y-categoria.csv", locale = locale(decimal_mark = ","))
tasas$indice_tiempo <- as.Date(tasas$indice_tiempo, format = "%Y-%m-%d")
tasas <- tasas %>%
  mutate(tasa_de_ocupacion_plazas = as.numeric(gsub(",", ".", tasa_de_ocupacion_plazas)))
tasas <- tasas %>%
  mutate(trimestre = paste0(year(indice_tiempo), "-Q", quarter(indice_tiempo)))
tasas <- tasas %>%
  group_by(trimestre, region_de_destino, categoria_del_hotel) %>%
  summarize(
    media = mean(tasa_de_ocupacion_plazas, na.rm = TRUE),
    mediana = median(tasa_de_ocupacion_plazas, na.rm = TRUE),
    dispersion = sd(tasa_de_ocupacion_plazas, na.rm = TRUE)
  )



viajeros_hosp <- read_csv("viajeros_por_localidad_segun_residencia.csv", locale = locale(decimal_mark = ","))
viajeros_hosp$indice_tiempo <- as.Date(viajeros_hosp$indice_tiempo, format = "%Y-%m-%d")

viajeros_hosp <- viajeros_hosp %>%
  mutate(viajeros = as.numeric(gsub(",", ".", viajeros)))
viajeros_hosp <- viajeros_hosp %>%
  mutate(trimestre = paste0(year(indice_tiempo), "-Q", quarter(indice_tiempo)))
viajeros_hosp <- viajeros_hosp %>%
  group_by(trimestre, localidad, origen_viajeros) %>%
  summarize(
    media = mean(viajeros, na.rm = TRUE),
    mediana = median(viajeros, na.rm = TRUE),
    dispersion = sd(viajeros, na.rm = TRUE)
  )


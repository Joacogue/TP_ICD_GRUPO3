cambio_usd<- read_csv("tipos-de-cambio-historicos.csv", locale = locale(decimal_mark = ","))
cambio_usd$indice_tiempo <- as.Date(cambio_usd$indice_tiempo, format = "%Y-%m-%d")

cambio_usd <- cambio_usd %>%
  mutate(dolar_estadounidense = as.numeric(gsub(",", ".", dolar_estadounidense)))

cambio_usd <- cambio_usd %>%
  select(dolar_estadounidense, indice_tiempo) %>%
  filter(year(indice_tiempo) >= 2021 & year(indice_tiempo) <= 2024) %>%
  mutate(trimestre = paste0(year(indice_tiempo), "-Q", quarter(indice_tiempo)))

cambio_usd <- cambio_usd %>%
  group_by(trimestre) %>%
  summarize(
    media = mean(dolar_estadounidense, na.rm = TRUE),
    mediana = median(dolar_estadounidense, na.rm = TRUE),
    dispersion = sd(dolar_estadounidense, na.rm = TRUE)
  )






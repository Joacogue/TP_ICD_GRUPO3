hab_pesos <- read_csv("ingresos-por-habitacion-disponible-por-region-y-categoria.csv", locale = locale(decimal_mark = ","))
hab_usd$indice_tiempo <- as.Date(tp_icd_hotel$indice_tiempo, format = "%Y-%m-%d")
hab_usd <- hab_usd %>%
  mutate(ingresos_por_habitacion_disponible_en_usd = as.numeric(gsub(",", ".", ingresos_por_habitacion_disponible_en_usd)))
hab_usd_trim <- hab_usd %>%
  mutate(trimestre = paste0(year(indice_tiempo), "-Q", quarter(indice_tiempo)))
valores_2 <- hab_usd_trim %>%
  group_by(region_de_destino, trimestre, categoria_del_hotel) %>%
  summarize(
    media = mean(ingresos_por_habitacion_disponible_en_usd, na.rm = TRUE),
    mediana = median(ingresos_por_habitacion_disponible_en_usd, na.rm = TRUE),
    dispersion = sd(ingresos_por_habitacion_disponible_en_usd, na.rm = TRUE)
  )
view(hab_pesos)

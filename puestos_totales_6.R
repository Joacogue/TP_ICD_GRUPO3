puestos_tot <- read_csv("puestos-de-trabajo-totales-por-region-y-categoria.csv", locale = locale(decimal_mark = ","))
puestos_tot$indice_tiempo <- as.Date(puestos_tot$indice_tiempo, format = "%Y-%m-%d")
puestos_tot <- puestos_tot %>%
  mutate(puestos_de_trabajo = as.numeric(gsub(",", ".", puestos_de_trabajo)))
puestos_tot <- puestos_tot %>%
  mutate(trimestre = paste0(year(indice_tiempo), "-Q", quarter(indice_tiempo)))
puestos_tot <- puestos_tot %>%
  group_by(trimestre, region_de_destino, categoria_del_hotel) %>%
  summarize(
    media = mean(puestos_de_trabajo, na.rm = TRUE),
    mediana = median(puestos_de_trabajo, na.rm = TRUE),
    dispersion = sd(puestos_de_trabajo, na.rm = TRUE)
  )

puestos <- read_csv("puestos-de-trabajo-equivalentes-totales-por-region-y-categoria.csv", locale = locale(decimal_mark = ","))
puestos$indice_tiempo <- as.Date(puestos$indice_tiempo, format = "%Y-%m-%d")
puestos <- puestos %>%
  mutate(pernoctes = as.numeric(gsub(",", ".", pernoctes)))
pernoc_num <- pernoc_num %>%
  mutate(trimestre = paste0(year(indice_tiempo), "-Q", quarter(indice_tiempo)))
pernoc_num <- pernoc_num %>%
  group_by(trimestre, localidad, origen_pernoctes) %>%
  summarize(
    media = mean(pernoctes, na.rm = TRUE),
    mediana = median(pernoctes, na.rm = TRUE),
    dispersion = sd(pernoctes, na.rm = TRUE)
  )

view(puestos)

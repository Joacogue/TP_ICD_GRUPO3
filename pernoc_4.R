pernoc <- read_csv("pernoctes_por_localidad_segun_residencia.csv", locale = locale(decimal_mark = ","))
pernoc$indice_tiempo <- as.Date(pernoc$indice_tiempo, format = "%Y-%m-%d")
pernoc_num <- pernoc %>%
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



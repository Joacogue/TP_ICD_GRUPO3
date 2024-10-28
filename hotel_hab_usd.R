library(tidyverse)
library(dplyr)
library(lubridate)
tp_icd_hotel <- read_csv("estadia-media-residentes-y-no-residentes-por-destino.csv", locale = locale(decimal_mark = ","))
tp_icd_hotel$indice_tiempo <- as.Date(tp_icd_hotel$indice_tiempo, format = "%Y-%m-%d")
trimetres <- tp_icd_hotel %>%
  mutate(trimestre = paste0(year(indice_tiempo), "-Q", quarter(indice_tiempo)))
valores <- trimetres %>%
  group_by(region_de_destino, trimestre, origen_viajeros) %>%
  summarize(
    media = mean(estadia_media_en_noches, na.rm = TRUE),
    mediana = median(estadia_media_en_noches, na.rm = TRUE),
    dispersion = sd(estadia_media_en_noches, na.rm = TRUE)
  )

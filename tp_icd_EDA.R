library(tidyverse)
library(modelr)
tp_adelanto_usd <- read_csv("data_tp_icd.csv", locale = locale(decimal_mark = "."))

# Región de destino elegida en base a región de origen

ggplot(tp_adelanto_usd %>% filter(tipo_visitante=='Excursionista'), aes(x = region_origen, fill = region_destino)) +
  geom_bar(position = "stack") +
  labs(title = "Región de destino elegida",
       subtitle = "en base a Región de origen del excursionista",
       x = "Región de Origen",
       y = "Cantidad de Turistas",
       fill = "Región de Destino") +
  coord_flip()


# Se puede ver que en todos los casos el mayor porcentaje de los viajes son dentro
# de la misma región

# Discriminando por tipo de viajero

ggplot(tp_adelanto_usd %>% filter(tipo_visitante=='Turista'), aes(x = region_origen, fill = region_destino)) +
  geom_bar(position = "stack") +
  labs(title = "Región de destino elegida",
       subtitle = "en base a Región de origen del turista",
       x = "Región de Origen",
       y = "Cantidad de Turistas",
       fill = "Región de Destino") +
  coord_flip()

ggplot(tp_adelanto_usd %>% filter(tipo_visitante=='Excursionista'), aes(x = region_origen, fill = region_destino)) +
  geom_bar(position = "stack") +
  labs(title = "Región de destino elegida",
       subtitle = "en base a Región de origen del excursionista",
       x = "Región de Origen",
       y = "Cantidad de Turistas",
       fill = "Región de Destino") +
  coord_flip()

#Discriminando en turistas que la region de origen no sea la misma que destino

ggplot(tp_adelanto_usd %>% filter(tipo_visitante=='Turista', region_origen!=region_destino), aes(x = region_origen, fill = region_destino)) +
  geom_bar(position = "stack") +
  labs(title = "Región de destino elegida",
       subtitle = "en base a Región de origen del turista",
       x = "Región de Origen",
       y = "Cantidad de Turistas",
       fill = "Región de Destino") +
  coord_flip()

# Destinos más elegidos por trimestre
turistas_por_trimestre <- tp_adelanto_usd %>%
  filter(tipo_visitante=='Excursionista') %>% 
  group_by(trimestre, region_destino) %>%
  summarise(cantidad_turistas = sum(cantidad_integrantes, na.rm = TRUE))

ggplot(turistas_por_trimestre, aes(x = trimestre, y = cantidad_turistas, fill = region_destino)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cantidad_turistas), 
            position = position_stack(vjust = 0.5),
            color = "white") +  
  labs(title = "Cantidad de Excursionistas",
       subtitle="por Trimestre y Región de Destino",
       x = "Trimestre",
       y = "Cantidad de Turistas",
       fill = "Región de Destino") +
  theme_minimal()

# Distrución de gasto per cápita USD por región de destino

q1 <- quantile(tp_adelanto_usd$gasto_pc_usd, 0.25)
q3 <- quantile(tp_adelanto_usd$gasto_pc_usd, 0.75)
iqr <- q3 - q1

limite_inferior <- 0
limite_superior <- q3 + 2 * iqr

ggplot(tp_adelanto_usd %>% filter(gasto_pc >= q[1] & gasto_pc <= q[2]), 
       aes(x = region_destino, y = gasto_pc_usd)) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  labs(title = "Distribución del Gasto Per Cápita por Región de Destino",
       x = "Región de Destino",
       y = "Gasto Per Cápita") +
  coord_cartesian(ylim = c(limite_inferior, limite_superior)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_discrete(labels = c("Provincia de Buenos Aires - Partidos del GBA" = "Buenos Aires - GBA",
                              "Provincia de Buenos Aires - Resto" = "Buenos Aires - Resto"))


#top 10 destinos
top_10_destinos <- tp_adelanto_usd %>%
  filter(tipo_visitante=='Turista') %>% 
  group_by(region_destino, provincia_destino, localidad_destino) %>%
  summarise(cantidad_turistas = sum(cantidad_integrantes, na.rm = TRUE)) %>%
  arrange(region_destino, desc(cantidad_turistas)) %>%
  group_by(region_destino) %>%
  slice_head(n = 10) %>%
  ungroup()

print(top_10_destinos)

top_provincias <- tp_adelanto_usd %>%
  group_by(region_destino, provincia_destino) %>%
  summarise(total_turistas = sum(cantidad_integrantes)) %>%
  top_n(10, total_turistas) %>%
  arrange(region_destino, desc(total_turistas))

top_localidades <- tp_adelanto_usd %>%
  group_by(region_destino, localidad_destino) %>%
  summarise(total_turistas = sum(cantidad_integrantes)) %>%
  top_n(10, total_turistas) %>%
  arrange(region_destino, desc(total_turistas))

#Evolucion de la proporcion de turistas

library(forcats)

destino_por_anio <- tp_adelanto_usd %>%
  group_by(anio, trimestre, region_destino) %>%
  summarise(total_visitantes = sum(cantidad_integrantes), .groups = 'drop') %>%
  group_by(anio, trimestre) %>%
  mutate(porcentaje = round((total_visitantes / sum(total_visitantes)) * 100)) %>%
  ungroup() %>%
  mutate(anio_trimestre = paste0(anio, "-Q", trimestre))

destino_por_anio <- destino_por_anio %>%
  group_by(anio_trimestre) %>%
  mutate(
    porcentaje = round((total_visitantes / sum(total_visitantes)) * 100, 1)
  ) %>%
  mutate(
    ajuste = 100 - sum(porcentaje),
    porcentaje = if_else(row_number() == 1, porcentaje + ajuste, porcentaje) 
  )

destino_por_anio <- destino_por_anio %>%
  mutate(region_destino = fct_reorder(region_destino, porcentaje, .fun = mean, .desc = TRUE))


ggplot(destino_por_anio, aes(x = anio_trimestre, y = porcentaje, group = region_destino)) +
  geom_line(aes(color = region_destino), linewidth = 1) +
  geom_point(
    aes(fill = region_destino), 
    size = 2, 
    pch = 21, 
    color = "white", 
    stroke = 1
  ) +
  labs(title = "Evolución de la proporción de visitantes a cada región",
       subtitle = "Proporción trimestral de visitantes por región de destino sobre total de visitantes",
       x = "Año - Trimestre",
       y = "Porcentaje",
       color = "Región de destino", 
       fill = "Región de destino") +
  scale_y_continuous(breaks = seq(0, 100, by = 10),  # Ajusta el rango y el intervalo de etiquetas en el eje y
                     labels = function(x) paste0(x, "%")) +  # Añadir el símbolo de porcentaje
  theme_minimal() +
  theme(plot.title = element_text(),
        axis.text.y = element_text(), 
        axis.ticks.y = element_line(),
        axis.text.x = element_text(angle = 45, hjust = 1))


#Destinos elegidos por trimestre clasificado por tipo de visitante (ver con zoom)
turistas_por_trimestre <- tp_adelanto_usd %>%
  group_by(tipo_visitante, trimestre, region_destino) %>%
  summarise(cantidad_turistas = sum(cantidad_integrantes, na.rm = TRUE),
            .groups = 'keep')

ggplot(turistas_por_trimestre, aes(x = cantidad_turistas, y = region_destino, 
                                   fill = factor(trimestre))) +
  geom_bar(stat = "identity", position = position_dodge(0.9),width = 1) +
  geom_text(aes(label = cantidad_turistas), 
            position = position_dodge(0.9),
            hjust = 1,
            color = "black",
            size = 3) +  
  labs(title = "Cantidad de visitantes a cada region
       por trimestre y region de destino",
       subtitle = "Evaluado por tipo de visitante",
       fill = "Trimestre",
       x = "Cantidad de Turistas",
       y = "") +
  facet_wrap(~tipo_visitante)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())


ggplot(tp_adelanto_usd%>% filter(!alojamiento == 0 & !alojamiento== "Ns./ Nr."),
       aes(x= region_destino, fill= alojamiento))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()

ggplot(tp_adelanto_usd%>% filter(!transporte== "Ns./ Nr."),
       aes(x= region_destino, fill= transporte))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)


ggplot(tp_adelanto_usd%>% filter(!spa_termas== "Ns./ Nr."),
       aes(x= region_destino, fill= spa_termas))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

ggplot(tp_adelanto_usd%>% filter(!visito_playa_mar_rio== "Ns./ Nr."),
       aes(x= region_destino, fill= visito_playa_mar_rio))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

ggplot(tp_adelanto_usd%>% filter(!deporte_nieve== "Ns./ Nr."),
       aes(x= region_destino, fill= deporte_nieve))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

ggplot(tp_adelanto_usd%>% filter(!deporte_aventura== "Ns./ Nr."),
       aes(x= region_destino, fill= deporte_aventura))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

ggplot(tp_adelanto_usd%>% filter(!teatro_cine_concierto== "Ns./ Nr."),
       aes(x= region_destino, fill= teatro_cine_concierto))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

ggplot(tp_adelanto_usd%>% filter(!museos_monumentos_zoo== "Ns./ Nr."),
       aes(x= region_destino, fill= museos_monumentos_zoo))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

ggplot(tp_adelanto_usd%>% filter(!parques_reservas== "Ns./ Nr."),
       aes(x= region_destino, fill= parques_reservas))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

ggplot(tp_adelanto_usd%>% filter(!casinos_bingos== "Ns./ Nr."),
       aes(x= region_destino, fill= casinos_bingos))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

ggplot(tp_adelanto_usd%>% filter(!salida_nocturna== "Ns./ Nr."),
       aes(x= region_destino, fill= salida_nocturna))+
  geom_bar(position = "fill")+
  theme_minimal()+
  coord_flip()+
  theme_minimal()+
  facet_grid(~tipo_visitante)

glimpse(destino_por_anio)

destino_por_anio %>%
  group_by(anio_trimestre) %>%
  summarise(suma_porcentajes = sum(porcentaje))

library(dplyr)
library(waffle)
library(RColorBrewer)

# Filtrar el dataset destino_por_anio
df <- destino_por_anio

colores_set3 <- brewer.pal(n = length(unique(data_seleccionada$region_destino)), name = "Set3")

# Definir manualmente el año y trimestre
anio_seleccionado <- 2021  # Cambia este valor según el año que desees
trimestre_seleccionado <- 1  # Cambia este valor entre 1, 2, 3 o 4 según el trimestre

# Filtrar los datos del trimestre y año seleccionados
data_seleccionada <- df %>%
  filter(anio == anio_seleccionado & trimestre == trimestre_seleccionado)

# Asegurar que los porcentajes sumen 100 (en caso de redondeo)
data_seleccionada <- data_seleccionada %>%
  mutate(porcentaje_ajustado = round(porcentaje / sum(porcentaje) * 100))

# Corregir el último porcentaje para que la suma sea exactamente 100
# Sumar todos los porcentajes ajustados y restar ese valor de 100
total_ajustado <- sum(data_seleccionada$porcentaje_ajustado)
diferencia <- 100 - total_ajustado

# Ajustar el último valor (siempre al final de la lista)
data_seleccionada$porcentaje_ajustado[nrow(data_seleccionada)] <- data_seleccionada$porcentaje_ajustado[nrow(data_seleccionada)] + diferencia

data_seleccionada$region_destino <- factor(data_seleccionada$region_destino, 
                                           levels = unique(data_seleccionada$region_destino))

data_seleccionada <- data_seleccionada %>%
  arrange(desc(porcentaje_ajustado))

# Graficar el waffle con los porcentajes ajustados
waffle::waffle(
  c(setNames(data_seleccionada$porcentaje_ajustado, data_seleccionada$region_destino)),
  rows = 10,
  colors = colores_set3,
  title = paste("Proporción de Visitantes por Región - Trimestre", trimestre_seleccionado, "Año", anio_seleccionado),
  xlab = "1 cuadrado = 1% de visitantes"
)

ggplot(tp_adelanto_usd, aes(x = tipo_visitante, y = gasto_pc_usd)) +
  geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black", alpha = 0.5) + 
  labs(title = "Distribución del Gasto Per Cápita por Quintil de Ingreso Familiar",
       x = "Quintil de Ingreso Per Cápita Familiar",
       y = "Gasto Per Cápita en USD") +
  theme_minimal()

ggplot(tp_adelanto_usd, aes(x = gasto_pc_usd)) +
  geom_density(fill = "coral", alpha = 0.6) +
  labs(title = "Distribución del Gasto Per Cápita",
       x = "Gasto Per Cápita [USD]",
       y = "Densidad") +
  theme_minimal()


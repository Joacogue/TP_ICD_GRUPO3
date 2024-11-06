library(tidyverse)
library(modelr)
tp_icd <- read_csv("data_tp_icd.csv", locale = locale(decimal_mark = "."))

tp_icd <- tp_icd%>% 
  mutate(anio_trimestre = paste0(anio,"-", trimestre))%>%
  mutate(duracion_viaje = as.numeric(duracion_viaje))%>%
  mutate(anio_trimestre = as.factor(anio_trimestre))



base <- ggplot(tp_icd, aes(x= duracion_viaje, y=gasto_pc_usd))+
  geom_point(aes(color= factor(cantidad_integrantes)))+
  labs(title = "Gasto percapita del visitante",
       subtitle = "En funcion de la duracion del viaje",
       x = "Duracion del viaje [DIAS]",
       y = "Gasto[USD]")+
  scale_x_continuous(limits = c(0,100))

base

mod <- lm(data=tp_icd, formula = gasto_pc_usd ~ duracion_viaje)
#gasto_pc_usd = 60.6202 + 13.0537 * duracion_viaje
summary(mod)

#grafico de residuos
plot(mod)

#prediccion
grid <- data_grid(tp_icd,duracion_viaje) %>% add_predictions(mod)
base + geom_line(data=grid, aes(x=duracion_viaje, y=pred, group = 1), linewidth=1, color='red')


mod1 <- lm(data=tp_icd, formula = gasto_pc_usd ~ duracion_viaje + region_destino)
##gasto_pc_usd = 50.7784 + 12.6526 * duracion_viaje <- centro
#gasto_pc_usd = 50.7784 + 71.9394 + 12.6526 * duracion_viaje <- ciudad de bs.as
#gasto_pc_usd = 50.7784 + 5.0320 + 12.6526 * duracion_viaje <- cuyo 
#gasto_pc_usd = 50.7784 -8.7190 + 12.6526 * duracion_viaje <- litoral
summary(mod1)

grid <- data_grid(tp_icd,duracion_viaje, region_destino) %>% add_predictions(mod1)
base + geom_line(data=grid, aes(x=duracion_viaje, y=pred, group = 1), linewidth=1, color='red')

plot(mod1)
anova(mod, mod1) 

mod2 <- lm(data=tp_icd, formula = gasto_pc_usd ~ duracion_viaje + cantidad_integrantes)
##gasto_pc_usd = 121.79 + 12.7841 * duracion_viaje - 266706 * cantidad_integrantes
summary(mod2)

plot(mod2)

grid <- data_grid(tp_icd, duracion_viaje, cantidad_integrantes) %>% 
  add_predictions(mod2)
base + geom_line(data=grid, aes(x=duracion_viaje, y=pred, group = cantidad_integrantes), linewidth=1, color='red')

anova(mod1, mod2)

mod3 <- lm(data=tp_icd, formula = gasto_pc_usd ~ duracion_viaje+
           + cantidad_integrantes + region_destino)
##gasto_pc_usd = 121.79 + 12.7841 * duracion_viaje - 26.6706 * cantidad_integrantes
summary(mod3)

plot(mod3)

grid <- data_grid(tp_icd, duracion_viaje, cantidad_integrantes, region_destino) %>% 
  add_predictions(mod3)
base + geom_line(data=grid, aes(x=duracion_viaje, y=pred, group = cantidad_integrantes), color='red')
anova(mod2,mod3)

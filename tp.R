library(tidyverse)
tp_icd <- read_csv("evyth_microdatos.csv", locale = locale(decimal_mark = ","))
tp_diccionario <- read_csv("evyth_diccionario_registro.csv")
tp <- tp_icd%>%
  filter(anio>=2021)%>%
  relocate(p004, .after= miembro)%>%
  relocate(gasto_pc, .after = anio)%>%
  filter(p004==1)

tp_filtrado <- tp %>%
  select(-pondera, -px06_agrup, -px07_agrup, -px08_agrup, -pxb16_1_1, -pxb16_1_2,
         -pxb16_1_3, -pxb16_1_4, -pxb16_1_5, -pxb16_1_6, -pxb16_1_7, -pxb16_1_9, 
         -pxb16_2, -p004, -p005, -p006, -p007, -cond_act, -p013, -j_sexo, -j_edad,
         -j_nivel_ed, -nivel_ed)


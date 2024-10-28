library(tidyverse)
tp_icd <- read_csv('C:\\Users\\usuario\\Downloads\\evyth_microdatos.csv', locale = locale(decimal_mark = ","))
tp <- tp_icd%>%
  filter(anio>=2021)%>%
  relocate(p004, .after= miembro)%>%
  relocate(gasto_pc, .after = anio)%>%
  filter(p004==1)

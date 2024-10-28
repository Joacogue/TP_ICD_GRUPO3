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
         -j_nivel_ed, -nivel_ed, -px14, -px17_1, -p006_agrup)

tp_con_variables <- tp_filtrado %>%
  rename(
    cantidad_integrantes = px06,
    duracion_viaje = 	
      px07,
    alojamiento = 	
      px08,
    transporte = 	
      px09,
    motivo = 	
      px10_1,
    paquete_turistico = 
    px11,
    planificacion =
    px13,
    calif_transporte =
      px18_1,
    calif_alojamiento = 
      px18_2,
    calif_gastonomia = 
    px18_3, 
    calif_info_turistica =
    px18_4,
    calif_higiene = 
      px18_5,
    calif_seguridad =
    px18_6,
    calif_viaje =
      px18_7,
    spa_termas =
    px17_2_2,
    visito_playa_mar_rio =
    px17_2_3,
    deporte_nieve =
      px17_2_4,
    deporte_aventura =
      px17_2_5,
    caza_pesca =
    px17_2_6,
    espect_deportivo =
      px17_2_7,
    espect_religioso =
      px17_2_8,
    teatro_cine_concierto =
    px17_2_9,
    museos_monumentos_zoo =
      px17_2_10,
    parques_reservas =
      px17_2_11,
    casinos_bingos =
      px17_2_12,
    salida_nocturna =
      px17_2_13,
    espacios_rurales = 
      px17_2_1,
    paquete_por_internet =
      px15_1,
    alojamiento_por_internet =
    px15_2,
    transporte_por_internet =
      px15_3,
    excursiones_entradas_por_internet =
      px15_4,
    transporte_en_paquete =
      px12_1,
    alojamiento_en_paquete =
      px12_2,
    comidas_en_paquete =
      px12_3,
    todas_las_comidas_en_paquete =
      px12_4,
    traslados_en_paquete =
      px12_5,
    excursiones_en_paquete =
     px12_6,
    seguro_en_paquete =
      px12_7,
    otros_servicios_en_paquete =
     px12_8,
    quintil_ingreso_per_capita_familiar =
      quintil_pcf_visitante,
    miembros_del_hogar =
      p002
    )

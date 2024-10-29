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
    calif_gastronomia = 
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

tp_con_valores <- tp_con_variables %>%
  mutate(
    # Region Origen
    region_origen = case_when(
      region_origen == 1 ~ "Ciudad de Buenos Aires",
      region_origen == 2 ~ "Provincia de Buenos Aires - Partidos del GBA",
      region_origen == 3 ~ "Provincia de Buenos Aires - Resto",
      region_origen == 4 ~ "Centro",
      region_origen == 5 ~ "Litoral",
      region_origen == 6 ~ "Norte",
      region_origen == 7 ~ "Cuyo",
      region_origen == 8 ~ "Patagonia",
      TRUE ~ as.character(region_origen)
    ),
    # Aglomerado Origen
    aglomerado_origen = case_when(
      aglomerado_origen == 2 ~ "Gran La Plata",
      aglomerado_origen == 3 ~ "Bahía Blanca - Cerri",
      aglomerado_origen == 4 ~ "Gran Rosario",
      aglomerado_origen == 5 ~ "Gran Santa Fe",
      aglomerado_origen == 6 ~ "Gran Parana",
      aglomerado_origen == 7 ~ "Posadas",
      aglomerado_origen == 8 ~ "Gran Resistencia",
      aglomerado_origen == 9 ~ "Comodoro Rivadavia - Rada Tilly",
      aglomerado_origen == 10 ~ "Gran Mendoza",
      aglomerado_origen == 12 ~ "Corrientes",
      aglomerado_origen == 13 ~ "Gran Córdoba",
      aglomerado_origen == 14 ~ "Concordia",
      aglomerado_origen == 15 ~ "Formosa",
      aglomerado_origen == 17 ~ "Neuquén - Plottier",
      aglomerado_origen == 18 ~ "Santiago del Estero - La Banda",
      aglomerado_origen == 19 ~ "Jujuy - Palpalá",
      aglomerado_origen == 20 ~ "Río Gallegos",
      aglomerado_origen == 22 ~ "Gran Catamarca",
      aglomerado_origen == 23 ~ "Salta",
      aglomerado_origen == 25 ~ "La Rioja",
      aglomerado_origen == 26 ~ "San Luis - El Chorrillo",
      aglomerado_origen == 27 ~ "Gran San Juan",
      aglomerado_origen == 29 ~ "Gran Tucumán - TafiViejo",
      aglomerado_origen == 30 ~ "Santa Rosa - Toay",
      aglomerado_origen == 31 ~ "Ushuaia - Rio Grande",
      aglomerado_origen == 32 ~ "Ciudad de Buenos Aires",
      aglomerado_origen == 33 ~ "Partidos del GBA",
      aglomerado_origen == 34 ~ "Mar del Plata - Batán",
      aglomerado_origen == 36 ~ "Río Cuarto",
      aglomerado_origen == 38 ~ "San Nicolás - Villa Constitución",
      aglomerado_origen == 91 ~ "Rawson - Trelew",
      aglomerado_origen == 91 ~ "Viedma - Carmen de Patagones",
      TRUE ~ as.character(provincia_destino)
    ),
    # Region destino
    region_destino = case_when(
      region_destino == 1 ~ "Ciudad de Buenos Aires",
      region_destino == 2 ~ "Provincia de Buenos Aires - Partidos del GBA",
      region_destino == 3 ~ "Provincia de Buenos Aires - Resto",
      region_destino == 4 ~ "Centro",
      region_destino == 5 ~ "Litoral",
      region_destino == 6 ~ "Norte",
      region_destino == 7 ~ "Cuyo",
      region_destino == 8 ~ "Patagonia",
      TRUE ~ as.character(region_destino)
    ),
    # Provincia de destino
    provincia_destino = case_when(
      provincia_destino == 2 ~ "CABA",
      provincia_destino == 4 ~ "Partidos del GBA (Pcia. Bs. As.)",
      provincia_destino == 6 ~ "Buenos Aires (Resto)",
      provincia_destino == 10 ~ "Catamarca",
      provincia_destino == 14 ~ "Córdoba",
      provincia_destino == 18 ~ "Corrientes",
      provincia_destino == 22 ~ "Chaco",
      provincia_destino == 26 ~ "Chubut",
      provincia_destino == 30 ~ "Entre Ríos",
      provincia_destino == 34 ~ "Formosa",
      provincia_destino == 38 ~ "Jujuy",
      provincia_destino == 42 ~ "La Pampa",
      provincia_destino == 46 ~ "La Rioja",
      provincia_destino == 50 ~ "Mendoza",
      provincia_destino == 54 ~ "Misiones",
      provincia_destino == 58 ~ "Neuquén",
      provincia_destino == 62 ~ "Río Negro",
      provincia_destino == 66 ~ "Salta",
      provincia_destino == 70 ~ "San Juan",
      provincia_destino == 74 ~ "San Luis",
      provincia_destino == 78 ~ "Santa Cruz",
      provincia_destino == 82 ~ "Santa Fe",
      provincia_destino == 86 ~ "Santiago del Estero",
      provincia_destino == 90 ~ "Tucumán",
      provincia_destino == 94 ~ "Tierra del Fuego",
      TRUE ~ as.character(provincia_destino)
    ),
    # Tipo visitante
    tipo_visitante = case_when(
      tipo_visitante == 1 ~ "Turista",
      tipo_visitante == 2 ~ "Excursionista",
      TRUE ~ as.character(tipo_visitante)
    ),
    # Multidestino
    multidestino = case_when(
      multidestino == 1 ~ "Una etapa",
      multidestino == 2 ~ "Dos etapas o mas",
      TRUE ~ as.character(multidestino)
    ),
    # Multidestino
    multidestino = case_when(
      multidestino == 1 ~ "Una etapa",
      multidestino == 2 ~ "Dos etapas o mas",
      TRUE ~ as.character(multidestino)
    ),
    # Multidestino
    alojamiento = case_when(
      alojamiento == 1 ~ "Segunda vivienda del hogar (propia)",
      alojamiento == 2 ~ "Segunda vivienda del hogar (alquilada)",
      alojamiento == 3 ~ "Vivienda de familiares o amigos que  residen en ella",
      alojamiento == 4 ~ "Vivienda de familiares o amigos que no residen en ella",
      alojamiento == 5 ~ "Vivienda alquilada por temporada",
      alojamiento == 6 ~ "Camping (carpa, casa rodante)",
      alojamiento == 7 ~ "Hotel u hostería hasta 3 estrellas (o categoria desconocida)",
      alojamiento == 8 ~ "Hotel 4 y 5 estrellas, apart hotel o similares",
      alojamiento == 9 ~ "Estancia o casa rural",
      alojamiento == 10 ~ "Otro (tiempo compartido, lugares públicos, etc.)",
      alojamiento == 99 ~ "Ns./ Nr.",
      TRUE ~ as.character(alojamiento)
    ),
    # Transporte
    transporte = case_when(
      transporte == 1 ~ "Automóvil o similar (propio)",
      transporte == 2 ~ "Omnibus",
      transporte == 3 ~ "Tren",
      transporte == 4 ~ "Avión",
      transporte == 5 ~ "Embarcación",
      transporte == 6 ~ "Taxi o remís",
      transporte == 8 ~ "Otro (auto alquilado, motocicleta, bicicleta, a pie, a dedo, etc.)",
      transporte == 99 ~ "Ns./ Nr.",
      TRUE ~ as.character(transporte)
    ),
    # Motivo de viaje
    motivo = case_when(
      motivo == 1 ~ "Esparcimiento, ocio, recreacion",
      motivo == 2 ~ "Visitas a familiares o amigos",
      motivo == 3 ~ "Trabajo, negocios, motivos profesionales",
      motivo == 4 ~ "Estudios y formación",
      motivo == 5 ~ "Razones de salud",
      motivo == 6 ~ "Motivos religiosos (peregrinaciones, etc.)",
      motivo == 7 ~ "Compras",
      motivo == 8 ~ "Otros",
      motivo == 99 ~ "Ns./ Nr.",
      TRUE ~ as.character(motivo)
    ),
    # Contrataron paquete turistico?
    paquete_turistico = case_when(
      paquete_turistico == 1 ~ "Si",
      paquete_turistico == 2 ~ "No",
      paquete_turistico == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(paquete_turistico)
    ),
    # Transporte en paquete
    transporte_en_paquete = case_when(
      transporte_en_paquete == 1 ~ "Si",
      transporte_en_paquete == 2 ~ "No",
      transporte_en_paquete == 9 ~ "Ns./ Nr.",
      alojamiento_en_paquete == 10 ~ "No utilizaron paquete turístico",
      TRUE ~ as.character(transporte_en_paquete)
    ),
    # Alojamiento
    alojamiento_en_paquete = case_when(
      alojamiento_en_paquete == 1 ~ "Si",
      alojamiento_en_paquete == 2 ~ "No",
      alojamiento_en_paquete == 9 ~ "Ns./ Nr.",
      alojamiento_en_paquete == 10 ~ "No utilizaron paquete turístico",
      TRUE ~ as.character(alojamiento_en_paquete)
    ),
    # Comidas
    comidas_en_paquete = case_when(
      comidas_en_paquete == 1 ~ "Si",
      comidas_en_paquete == 2 ~ "No",
      comidas_en_paquete == 9 ~ "Ns./ Nr.",
      comidas_en_paquete == 10 ~ "No utilizaron paquete turístico",
      TRUE ~ as.character(comidas_en_paquete)
    ),
    # Todas las comidas
    todas_las_comidas_en_paquete = case_when(
      todas_las_comidas_en_paquete == 1 ~ "Si",
      todas_las_comidas_en_paquete == 2 ~ "No",
      todas_las_comidas_en_paquete == 9 ~ "Ns./ Nr.",
      todas_las_comidas_en_paquete == 10 ~ "No utilizaron paquete turístico",
      TRUE ~ as.character(todas_las_comidas_en_paquete)
    ),
    # Traslado
    traslados_en_paquete = case_when(
      traslados_en_paquete == 1 ~ "Si",
      traslados_en_paquete == 2 ~ "No",
      traslados_en_paquete == 9 ~ "Ns./ Nr.",
      traslados_en_paquete == 10 ~ "No utilizaron paquete turístico",
      TRUE ~ as.character(traslados_en_paquete)
    ),
    # Excursiones
    excursiones_en_paquete = case_when(
      excursiones_en_paquete == 1 ~ "Si",
      excursiones_en_paquete == 2 ~ "No",
      excursiones_en_paquete == 9 ~ "Ns./ Nr.",
      excursiones_en_paquete == 10 ~ "No utilizaron paquete turístico",
      TRUE ~ as.character(excursiones_en_paquete)
    ),
    # Seguro
    seguro_en_paquete = case_when(
      seguro_en_paquete == 1 ~ "Si",
      seguro_en_paquete == 2 ~ "No",
      seguro_en_paquete == 9 ~ "Ns./ Nr.",
      seguro_en_paquete == 10 ~ "No utilizaron paquete turístico",
      TRUE ~ as.character(seguro_en_paquete)
    ),
    # Otros servicios
    otros_servicios_en_paquete = case_when(
      otros_servicios_en_paquete == 1 ~ "Si",
      otros_servicios_en_paquete == 2 ~ "No",
      otros_servicios_en_paquete == 9 ~ "Ns./ Nr.",
      otros_servicios_en_paquete == 10 ~ "No utilizaron paquete turístico",
      TRUE ~ as.character(otros_servicios_en_paquete)
    ),
    # Planificacion
    planificacion = case_when(
      planificacion == 1 ~ "El mismo día",
      planificacion == 2 ~ "Entre un día y menos de una semana",
      planificacion == 3 ~ "Entre una semana y un mes",
      planificacion == 4 ~ "Más de un mes y hasta seis meses",
      planificacion == 5 ~ "Más de seis meses",
      planificacion == 99 ~ "Ns./ Nr.",
      TRUE ~ as.character(planificacion)
    ),
    # Paquete por internet
    paquete_por_internet = case_when(
      paquete_por_internet == 1 ~ "Si",
      paquete_por_internet == 2 ~ "No",
      paquete_por_internet == 9 ~ "Ns./ Nr.",
      paquete_por_internet == 10 ~ "No utilizaron internet",
      TRUE ~ as.character(paquete_por_internet)
    ),
    # Alojamiento por internet
    alojamiento_por_internet = case_when(
      alojamiento_por_internet == 1 ~ "Si",
      alojamiento_por_internet == 2 ~ "No",
      alojamiento_por_internet == 9 ~ "Ns./ Nr.",
      alojamiento_por_internet == 10 ~ "No utilizaron internet",
      TRUE ~ as.character(alojamiento_por_internet)
    ),
    # Transporte por internet
    transporte_por_internet = case_when(
      transporte_por_internet == 1 ~ "Si",
      transporte_por_internet == 2 ~ "No",
      transporte_por_internet == 9 ~ "Ns./ Nr.",
      transporte_por_internet == 10 ~ "No utilizaron internet",
      TRUE ~ as.character(transporte_por_internet)
    ),
    # Excursiones por internet
    excursiones_entradas_por_internet = case_when(
      excursiones_entradas_por_internet == 1 ~ "Si",
      excursiones_entradas_por_internet == 2 ~ "No",
      excursiones_entradas_por_internet == 9 ~ "Ns./ Nr.",
      excursiones_entradas_por_internet == 10 ~ "No utilizaron internet",
      TRUE ~ as.character(excursiones_entradas_por_internet)
    ),
    # Espacios rurales
    espacios_rurales = case_when(
      espacios_rurales == 1 ~ "Si",
      espacios_rurales == 2 ~ "No",
      espacios_rurales == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(espacios_rurales)
    ),
    # Spa  Termas
    spa_termas = case_when(
      spa_termas == 1 ~ "Si",
      spa_termas == 2 ~ "No",
      spa_termas == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(spa_termas)
    ),
    # Playa  mar  rio
    visito_playa_mar_rio = case_when(
      visito_playa_mar_rio == 1 ~ "Si",
      visito_playa_mar_rio == 2 ~ "No",
      visito_playa_mar_rio == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(visito_playa_mar_rio)
    ),
    # Nieve
    deporte_nieve = case_when(
      deporte_nieve == 1 ~ "Si",
      deporte_nieve == 2 ~ "No",
      deporte_nieve == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(deporte_nieve)
    ),
    # Aventura
    deporte_aventura = case_when(
      deporte_aventura == 1 ~ "Si",
      deporte_aventura == 2 ~ "No",
      deporte_aventura == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(deporte_aventura)
    ),
    # Caza Pesca
    caza_pesca = case_when(
      caza_pesca == 1 ~ "Si",
      caza_pesca == 2 ~ "No",
      caza_pesca == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(caza_pesca)
    ),
    # Caza Pesca
    caza_pesca = case_when(
      caza_pesca == 1 ~ "Si",
      caza_pesca == 2 ~ "No",
      caza_pesca == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(caza_pesca)
    ),
    # espect deportivo
    espect_deportivo = case_when(
      espect_deportivo == 1 ~ "Si",
      espect_deportivo == 2 ~ "No",
      espect_deportivo == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(espect_deportivo)
    ),
    # espect religioso
    espect_religioso = case_when(
      espect_religioso == 1 ~ "Si",
      espect_religioso == 2 ~ "No",
      espect_religioso == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(espect_religioso)
    ),
    # teatro cine concierto
    teatro_cine_concierto = case_when(
      teatro_cine_concierto == 1 ~ "Si",
      teatro_cine_concierto == 2 ~ "No",
      teatro_cine_concierto == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(teatro_cine_concierto)
    ),
    # museos monumentos zoo
    museos_monumentos_zoo = case_when(
      museos_monumentos_zoo == 1 ~ "Si",
      museos_monumentos_zoo == 2 ~ "No",
      museos_monumentos_zoo == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(museos_monumentos_zoo)
    ),
    # Parques reservas
    parques_reservas = case_when(
      parques_reservas == 1 ~ "Si",
      parques_reservas == 2 ~ "No",
      parques_reservas == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(parques_reservas)
    ),
    # Casinos bingos
    casinos_bingos = case_when(
      casinos_bingos == 1 ~ "Si",
      casinos_bingos == 2 ~ "No",
      casinos_bingos == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(casinos_bingos)
    ),
    # Salida nocturna
    salida_nocturna = case_when(
      salida_nocturna == 1 ~ "Si",
      salida_nocturna == 2 ~ "No",
      salida_nocturna == 9 ~ "Ns./ Nr.",
      TRUE ~ as.character(salida_nocturna)
    ),
    # Calif de transporte
    calif_transporte = case_when(
      calif_transporte == 99 ~ "Ns./ Nr.",
      calif_transporte == 100 ~ "No usó",
      TRUE ~ as.character(calif_transporte)
    ),
    # Calif de alojamiento
    calif_alojamiento = case_when(
      calif_alojamiento == 99 ~ "Ns./ Nr.",
      calif_alojamiento == 100 ~ "No usó",
      TRUE ~ as.character(calif_alojamiento)
    ),
    # Calif de gastronomia
    calif_gastronomia = case_when(
      calif_gastronomia == 99 ~ "Ns./ Nr.",
      calif_gastronomia == 100 ~ "No usó",
      TRUE ~ as.character(calif_gastronomia)
    ),
    # Calif de info turistica
    calif_info_turistica = case_when(
      calif_info_turistica == 99 ~ "Ns./ Nr.",
      calif_info_turistica == 100 ~ "No usó",
      TRUE ~ as.character(calif_info_turistica)
    ),
    # Calif de higiene
    calif_higiene = case_when(
      calif_higiene == 99 ~ "Ns./ Nr.",
      calif_higiene == 100 ~ "No usó",
      TRUE ~ as.character(calif_higiene)
    ),
    # Calif de seguridad
    calif_seguridad = case_when(
      calif_seguridad == 99 ~ "Ns./ Nr.",
      calif_seguridad == 100 ~ "No usó",
      TRUE ~ as.character(calif_seguridad)
    ),
    # Calif de viaje
    calif_viaje = case_when(
      calif_viaje == 99 ~ "Ns./ Nr.",
      calif_viaje == 100 ~ "No usó",
      TRUE ~ as.character(calif_viaje)
    ),
    # Condicion actividad jefe de hogar
    j_cond_act = case_when(
      j_cond_act == 1 ~ "Ocupado",
      j_cond_act == 2 ~ "Desocupado",
      j_cond_act == 3 ~ "Inactivo",
      j_cond_act == 9 ~ "Ns./ Nr",
      TRUE ~ as.character(j_cond_act)
    )
  )


tp_con_valores %>%
  summarise(distintos=n_distinct(tp_con_valores), total=n())

#833 observaciones repetidas, eliminamos?
tp_sin_repetidos <- unique(tp_con_valores)  


tp_sin_NA <- tp_sin_repetidos %>%
  mutate(paquete_turistico
         = replace(paquete_turistico,
                   is.na(paquete_turistico) ,"Ns./ Nr."),
         
         transporte_en_paquete
         = replace(transporte_en_paquete,
                   is.na(transporte_en_paquete) ,"Ns./ Nr."),
         
         alojamiento_en_paquete
         = replace(alojamiento_en_paquete,
                   is.na(alojamiento_en_paquete) ,"Ns./ Nr."),
         
         comidas_en_paquete
         = replace(comidas_en_paquete,
                   is.na(comidas_en_paquete) ,"Ns./ Nr."),
         
         todas_las_comidas_en_paquete
         = replace(todas_las_comidas_en_paquete,
                   is.na(todas_las_comidas_en_paquete) ,"Ns./ Nr."),
         
         traslados_en_paquete
         = replace(traslados_en_paquete,
                   is.na(traslados_en_paquete) ,"Ns./ Nr."),
         
         excursiones_en_paquete
         = replace(excursiones_en_paquete,
                   is.na(excursiones_en_paquete) ,"Ns./ Nr."),
         
         seguro_en_paquete
         = replace(seguro_en_paquete,
                   is.na(seguro_en_paquete) ,"Ns./ Nr."),
         
         otros_servicios_en_paquete
         = replace(otros_servicios_en_paquete,
                   is.na(otros_servicios_en_paquete) ,"Ns./ Nr."),
         
         planificacion
         = replace(planificacion,
                   is.na(planificacion) ,"Ns./ Nr."),
         
         paquete_por_internet
         = replace(paquete_por_internet,
                   is.na(paquete_por_internet) ,"Ns./ Nr."),
         
         alojamiento_por_internet
         = replace(alojamiento_por_internet,
                   is.na(alojamiento_por_internet) ,"Ns./ Nr."),
         
         transporte_por_internet
         = replace(transporte_por_internet,
                   is.na(transporte_por_internet) ,"Ns./ Nr."),
         
         excursiones_entradas_por_internet
         = replace(excursiones_entradas_por_internet,
                   is.na(excursiones_entradas_por_internet) ,"Ns./ Nr."),
         
         espacios_rurales
         = replace(espacios_rurales,
                   is.na(espacios_rurales) ,"Ns./ Nr."),
         
         spa_termas
         = replace(spa_termas,
                   is.na(spa_termas) ,"Ns./ Nr."),
         
         visito_playa_mar_rio
         = replace(visito_playa_mar_rio,
                   is.na(visito_playa_mar_rio) ,"Ns./ Nr."),
         
         deporte_nieve
         = replace(deporte_nieve,
                   is.na(deporte_nieve) ,"Ns./ Nr."),
         
         deporte_aventura
         = replace(deporte_aventura,
                   is.na(deporte_aventura) ,"Ns./ Nr."),
         
         caza_pesca
         = replace(caza_pesca,
                   is.na(caza_pesca) ,"Ns./ Nr."),
         
         espect_deportivo
         = replace(espect_deportivo,
                   is.na(espect_deportivo) ,"Ns./ Nr."),
         
         espect_religioso
         = replace(espect_religioso,
                   is.na(espect_religioso) ,"Ns./ Nr."),
         
         teatro_cine_concierto
         = replace(teatro_cine_concierto,
                   is.na(teatro_cine_concierto) ,"Ns./ Nr."),
         
         museos_monumentos_zoo
         = replace(museos_monumentos_zoo,
                   is.na(museos_monumentos_zoo) ,"Ns./ Nr."),
         
         parques_reservas
         = replace(parques_reservas,
                   is.na(parques_reservas) ,"Ns./ Nr."),
         
         casinos_bingos
         = replace(casinos_bingos,
                   is.na(casinos_bingos) ,"Ns./ Nr."),
         
         salida_nocturna
         = replace(salida_nocturna,
                   is.na(salida_nocturna) ,"Ns./ Nr."),
         
         calif_transporte
         = replace(calif_transporte,
                   is.na(calif_transporte) ,"Ns./ Nr."),
         
         calif_alojamiento
         = replace(calif_alojamiento,
                   is.na(calif_alojamiento) ,"Ns./ Nr."),
         
         calif_gastronomia
         = replace(calif_gastronomia,
                   is.na(calif_gastronomia) ,"Ns./ Nr."),
         
         calif_info_turistica
         = replace(calif_info_turistica,
                   is.na(calif_info_turistica) ,"Ns./ Nr."),
         
         calif_higiene
         = replace(calif_higiene,
                   is.na(calif_higiene) ,"Ns./ Nr."),
         
         calif_seguridad
         = replace(calif_seguridad,
                   is.na(calif_seguridad) ,"Ns./ Nr."),
         
         calif_viaje
         = replace(calif_viaje,
                   is.na(calif_viaje) ,"Ns./ Nr."))

#quedan 7971 NA que corresponden a codigos faltantes, 
#si no vamos a usar esas variables habria que elinarlas para que quede limpio

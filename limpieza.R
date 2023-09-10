# Convierto primero a factor y luego a string todas las opciones, para tener directamente las etiquetas. 
etra <- etra %>% 
  mutate_all(as_factor) |> 
  mutate_if(is.factor, as.character) |> 
  mutate_all(~na_if(., "<NA>"))


# Centros y filtro de los sin fecha
etra <- etra %>%
  mutate (fecha_encuesta = str_replace_all(date_created, "\\s.*", "")) %>% 
  mutate (fecha_lugar = case_when(fecha_encuesta=="05/04/2023" ~ "01. 04-05-23 - CAI San Juan Bautista (El Talar - visita 1)",
                                  fecha_encuesta=="05/12/2023" ~ "02. 12-05-23 -  Casa de los Jóvenes (Olivos)",
                                  fecha_encuesta=="05/15/2023" ~ "03. 15-05-23 - Casa del Joven La Cueva (San Fernando)",
                                  fecha_encuesta=="05/31/2023" ~ "04. 31-05-23 - CEC Casita de los Sueños (Victoria)",
                                  fecha_encuesta=="06/01/2023" ~ "05. 01-06-23 - Casabierta a la Vida (Benavidez)",
                                  fecha_encuesta=="06/08/2023" ~ "06. 08-06-23 - Ceferino Namuncurá (Virreyes)",
                                  fecha_encuesta=="06/16/2023" ~ "07. 16-06-23 - CAFF Santa Clotilde (Las Tunas)",
                                  fecha_encuesta=="06/22/2023" ~ "08. 22-06-23 - Proyecto Horizonte (La Cava)",
                                  fecha_encuesta=="06/28/2023" ~ "09. 28-06-23 - CAI San Juan Bautista (El Talar - visita 2)",
                                  #fecha_encuesta=="06/29/2023" ~ "10. 29-06-23",
                                  fecha_encuesta=="07/05/2023" ~ "11. 05-07-23 -  EES N° 8 (visita 1)",
                                  fecha_encuesta=="07/14/2023" ~ "12. 14-07-23 -  EES N° 8 (visita 2)",
                                  TRUE ~ "Sin datos")) %>%
  mutate (orden=as.numeric(str_sub(fecha_lugar,1,2))) %>% 
  filter (fecha_lugar!="Sin datos") |> 
  mutate (zona = case_when(str_detect(fecha_lugar,"11.|12.") ~ "Oeste",
                                      TRUE ~ "Norte"))
  
  

#Filtro las edades fuera del objetivo de investigación ---- 
etra <- etra |> 
  filter (Edad != "tengo otra edad")

#Grupos de Edad ---- 
etra <- etra |> 
  mutate (grupos_edad=case_when(Edad==13 | Edad==14 ~ "13-14 años",
                                Edad==15 | Edad==16 ~ "15-16 años",
                                Edad==17 | Edad==18 ~ "17-18 años"))

#Género ---- 
etra <- etra |> 
  mutate (Genero_recat = ifelse (Genero %in% c ("Varón","Mujer"),Genero,"Otros + S/D"))

# Familia migrante ----
etra <- etra %>% 
    mutate (Familia_Migrante_Recat= case_when (Familia_Migrante == "No, ninguno" ~ "Familia nativa",
                                               Familia_Migrante == "Prefiero no contestar" ~ "Prefiero no contestar",
                                               TRUE ~ "Familia migrante"))
# Trabajo (pasado) REVISAR ----
etra <- etra %>% 
  mutate (Trabajo_Previo_otro = case_when (str_detect(Trabajo_Previo_otro,"cuid|Cuid|niñera") ~ "Tareas de Cuidado",
                                   #str_detect(q0022_other,"Ayudante de")  ~ "Ayudante en oficios",
                                   str_detect(Trabajo_Previo_otro,"Ayudante de")  ~ "Changas eventuales",
                                   #str_detect(q0022_other, "Chapa|pintura|uñas|pestañas|manicuria|peinados|masajes|pelo|programacion|Heladero") ~ "Oficio propio",
                                   str_detect(Trabajo_Previo_otro, "Chapa|pintura|uñas|pestañas|manicuria|peinados|masajes|pelo|programacion|Heladero") ~ "Changas eventuales",
                                   str_detect(Trabajo_Previo_otro, "con mi papa") ~ "Acompañaba a un familiar a su trabajo",
                                   str_detect(Trabajo_Previo_otro, "En el mercado de becar") ~ "Trabajaba como empleado/a",
                                   str_detect(Trabajo_Previo_otro, "fabrica") ~ "Trabajaba como empleado/a",
                                   TRUE ~ Trabajo_Previo_otro)) %>% 
  mutate (Trabajo_Previo_recat = ifelse (Trabajo_Previo=="Otro(s):",Trabajo_Previo_otro,Trabajo_Previo)) %>% 
  mutate (Trabajo_Previo_recat = case_when (str_detect (Trabajo_Previo,"Trabajaba como empleado/a") ~ "Empleado/a para un patrón",
                                   Trabajo_Previo_recat %in% c ("Cuidado de hermanos/as",
                                                       "Cuidaba a una persona enferma") ~ "Tareas de Cuidado",
                                   TRUE ~ Trabajo_Previo_recat)) 

# Sensaciones Policía ----
etra <- etra %>% 
  mutate (Sensacion_Policia_otro_recat= case_when (str_detect(Sensacion_Policia_otro,"me da seguridad pero llega muy tarde") ~ "Me da seguridad pero no me gusta que esté en mi barrio",
                                                   str_detect (Sensacion_Policia_otro,"meda lo mismo|siento que no cambia nada|Me da lo mismo|Ninguno|Me da igual|me da igual|Ninguna,no sirven|No le doy importancia|No me da nada|No me importa|no hace nada|me da igual") ~ "Indiferencia",
                                                   Sensacion_Policia_otro!="" ~ "Otra(s):")) |> 
  mutate (Sensacion_Policia_recat = ifelse (Sensacion_Policia=="Otra(s):",Sensacion_Policia_otro_recat,Sensacion_Policia)) |> 
  mutate (Sensacion_Policia_recat= ifelse (Sensacion_Policia_recat=="Prefiero no contestar" | Sensacion_Policia_recat=="Sin datos", NA,Sensacion_Policia_recat)) 

# Agentes de discriminación ---- 
etra <- etra |> 
  mutate (q0045_12_recat = case_when (str_detect (q0045_other,"Novia|cole|compañeros") ~ "Compañeros/as de colegio o amigos/as",
                                str_detect (q0045_other,"Familia|familia|familiares|Hermanas|Familiares") ~ "Familiares",
                                TRUE ~ q0045_0012))
# Referente ante problemas ----           
etra <- etra |> 
  mutate (q0043_otro_recat = case_when (str_detect (q0043_other,"prefiero no contarlo|suento|espejo|SOLO|No hablo con nadie|solo|gata|lloro") ~ "Prefiero no recurrir a nadie",
                                      str_detect (q0043_other,"Novia|novia|novio|Novio|<3") ~ "Pareja",
                                      str_detect (q0043_other,"familia|mamá|Hermane") ~ "Familia",
                                      str_detect (q0043_other,"Murga") ~ "Familia",
                                      TRUE ~ q0043_0015))


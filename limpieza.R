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
                                  #fecha_encuesta=="07/30/2023" ~ "13. 30-07-23",
                                  TRUE ~ "Sin datos")) %>%
  mutate (orden=as.numeric(str_sub(fecha_lugar,1,2))) 
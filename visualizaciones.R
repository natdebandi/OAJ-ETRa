# Carga ---- 
source ("./ETRa_carga.R")

# Funciones ---- 
source ("./funciones.R")

# Limpieza --- 
source ("./limpieza.R")

# Tablas ---- 
# Tabla 1. Cantidad de encuestas por fecha y lugar de realización ----
etra %>% 
  group_by(orden,fecha_lugar) %>% 
    summarise(cantidad=n()) %>% 
  ungroup() |> 
  mutate (porcentaje=round(cantidad/sum(cantidad)*100,2)) %>% 
  arrange (orden) %>% 
  adorn_totals (where ="row") %>% 
  gt() %>% 
  gtsave(data=.,
         filename="t1.docx",
         path="./tbl/")

# Gráfico 1. Grupos de Edad según Género ----
etra %>%
  cruce_bivariado_con_total(.data = . ,
                            .cruce_x = Genero_recat,
                            .cruce_y = grupos_edad) %>% 
  mutate (Genero_recat=factor(Genero_recat,
                                  levels=c("Total",
                                           "Otros + S/D",
                                           "Mujer",
                                           "Varón"))) |> 
  mutate (grupos_edad = factor (grupos_edad, 
                                levels = c ("17-18 años",
                                            "15-16 años",
                                            "13-14 años"))) |> 
  mutate (etiquetas=generar_etiquetas(.x1 = porcentaje,
                                      .x2 = cantidad,
                                      .tipo = "porcentajes_frecuencias")) |> 
  grafico_apiladas_tres_variables(.x = Genero_recat,
                                  .y = porcentaje,
                                  .fill=grupos_edad,
                                  .etiquetas = etiquetas
                                  )+
  coord_flip()+
  theme(legend.position = "bottom")
  

# Gráfico 2. Género según grupos de Edad  ----
g1 <- etra %>%
  cruce_bivariado_con_total(.data=.,
                            .cruce_x=grupos_edad,
                            .cruce_y=Genero_recat) %>% 
# Le doy a las variables el orden que yo quiero 
  mutate (Genero_recat=factor(Genero_recat,
                              levels=c("Otros + S/D",
                                       "Mujer",
                                       "Varón"))) |>
  mutate (grupos_edad=factor(grupos_edad,
                              levels=c("Total",
                                       "17-18 años",
                                       "15-16 años",
                                       "13-14 años"))) |>
  mutate (etiquetas=generar_etiquetas(.x1 = porcentaje,
                                      .x2 = cantidad,
                                      .tipo = "porcentajes_frecuencias")) |> 
  grafico_apiladas_tres_variables(.x = grupos_edad,
                                  .y = porcentaje,
                                  .fill=Genero_recat,
                                  .etiquetas = etiquetas,
                                  .ordenX = grupos_edad
  )+
  coord_flip()+
  theme(legend.position = "bottom")+
  scale_fill_jama()+
  labs(title = "Gráfico 1. Cantidad de adolescentes según Grupos de Edad y Género. AMBA. 2023",
       caption = "Fuente: Elaboración propia")+
  theme(plot.title.position = "plot") 
  
  

  ggsave (plot=g1,
          dpi=320,
          device="tiff",
          filename="./graphs/g1.tiff")
  
  
  
# Gráfico 3. Motivos por los que va a la escuela ----
g2 <- etra %>% 
  select (starts_with("q0019")) %>% 
  dist_frecuencias_respuesta_multiple() %>% 
  filter (n >1) %>% 
  mutate (valor = case_when(str_detect (valor,"Ser profesional") ~ "Ser profesional",
                            str_detect (valor,"Ser valorado socialmente") ~ "Ser valorado socialmente",
                            str_detect (valor,"Ganar más dinero") ~ "Mejorar situación económica",
                            str_detect (valor,"No sirve para nada") ~ "No sirve para nada",
                            str_detect (valor,"Otra(s)") ~ "Otras",
                            TRUE ~ valor)) %>% 
  mutate (etiquetas = generar_etiquetas(.x1 = porcentaje,
                                        .x2 = n,
                                        .tipo ="porcentajes_frecuencias")) %>% 
  grafico_barras_dos_variables_etiqueta(.x = valor,
                               .y = porcentaje,
                               .ordenX = porcentaje,
                               .etiquetas=etiquetas)+
  geom_bar_text(contrast="FALSE", position = "stack", reflow = TRUE)+
  coord_flip()+
  scale_fill_jama()+
    labs(title = "Gráfico 2. Motivos para asistir a la escuela. AMBA. 2023",
         caption = "Fuente: Elaboración propia")
  
  
  ggsave (plot=g2,
          dpi=320,
          device="tiff",
          filename="./graphs/g2.tiff")+
    theme(plot.title.position = "plot")
    

# Gráfico 4. Tipo de beca  
etra %>% 
  select (starts_with("q0015")) %>% 
  dist_frecuencias_respuesta_multiple()   
  # Por ahora hasta acá. Desarrollar 

# Gráfico 5. Tipo de trabajo actual 
g3 <-  etra %>% 
  dist_frecuencias(Trabajo_Previo_recat) %>% 
  mutate (etiquetas = generar_etiquetas(.x1= valid_percent,
                                        .x2= n,
                                        .tipo ="porcentajes_frecuencias")) %>% 
  filter (Trabajo_Previo_recat!="Sin datos") %>% 
  grafico_barras_dos_variables_etiqueta (.x=Trabajo_Previo_recat,
                               .y=valid_percent,
                               .ordenX = valid_percent,
                               .etiquetas = etiquetas)+
   geom_bar_text(contrast="FALSE", position = "stack", reflow = TRUE)+
   coord_flip()+
   scale_fill_jama()+
   labs(title = "Gráfico 3. Tipo de trabajo realizado (presente y/o previo). AMBA. 2023",
        caption = "Fuente: Elaboración propia")+
  theme(plot.title.position = "plot")
  
 
 
 ggsave (plot=g3,
         dpi=320,
         device="tiff",
         filename="./graphs/g3.tiff")

 # Gráfico 6. Trabajo y remuneración ----
etra %>% 
 select (q0020,q0024) %>% 
   filter(q0020!="No") %>% 
    dist_frecuencias(q0024)
# Por ahora hasta acá. A desarrollar. 
 
# Gráfico 7. Género de los trabajos de cuidado y domésticos
 etra %>% 
   filter (Trabajo_Previo_recat %in% c("Tareas de cuidado",
                                "Trabajo doméstico")) %>% 
   
   select (Genero_recat) %>% 
   tabyl(.,Genero_recat) %>% 
   gt()
 #Por ahora hasta acá. A desarrollar
   
# Gráfico 8. Continuidad laboral   
 etra %>% 
   select (q0025) %>% 
   tabyl(.,q0025) %>% 
   gt()
 #Por ahora hasta acá. A desarrollar
 
 # Gráfico 9. Afectación del trabajo a los estudios  
 etra %>% 
   select (starts_with("q0028")) %>% 
   dist_frecuencias_respuesta_multiple() %>% 
   gt()
 # Por ahora hasta acá. Desarrollar 

 # Gráfico 10. Sensación que produce ver pasar a la policía. 
 g4 <- etra %>% 
   dist_frecuencias(Sensacion_Policia_recat) |> 
   mutate (etiquetas = generar_etiquetas(.x1= valid_percent,
                                         .x2= n,
                                         .tipo ="porcentajes_frecuencias")) %>% 
   filter (Sensacion_Policia_recat!="Sin datos") %>% 
   grafico_barras_dos_variables_etiqueta (.x=Sensacion_Policia_recat,
                                          .y=valid_percent,
                                          .ordenX = valid_percent,
                                          .etiquetas = etiquetas)+
   geom_bar_text(contrast="FALSE", position = "stack", reflow = TRUE)+
   coord_flip() +
   scale_fill_jama()+
   labs(title = "Gráfico 4.Sensación que produce ver pasar a la policía en el propio barrio. AMBA. 2023",
        caption = "Fuente: Elaboración propia")+
   theme(plot.title.position = "plot") 
   
 
 
 ggsave (plot=g4,
         dpi=320,
         device="tiff",
         filename="./graphs/g4.tiff")

 # Gráfico 11 Sensación que produce ver pasar a la policía por zona ----
 etra %>%   
   cruce_bivariado (.x = Genero,
                    .y=Sensacion_Policia_recat) |> 
   arrange (-cantidad) |> 
   gt()
   mutate (etiquetas = generar_etiquetas(.x1= valid_percent,
                                         .x2= n,
                                         .tipo ="porcentajes_frecuencias")) %>% 
   filter (Sensacion_Policia_recat!="Sin datos") %>% 
   grafico_barras_tres_variables (.x=Sensacion_Policia_recat,
                                          .y=valid_percent,
                                          .ordenX = valid_percent,
                                          .etiquetas = etiquetas,
                                          .fill = zona)+
   geom_bar_text(contrast="FALSE", position = "stack", reflow = TRUE)+
   coord_flip()

  # Gráfico 12. Fue parado por la policía ----
etra %>%   
  dist_frecuencias(q0032)

# Gráfico 13.  Frecuencia de detención  por la policía ----
etra %>%   
  dist_frecuencias(q0032)

# Gráfico 14.  Abusos sufridos en la detención  por la policía ----   
etra %>%   
     select (starts_with("q0036")) |> 
     dist_frecuencias_respuesta_multiple() |> 
     gt()
# Gráfico 15.  Traslados a espacios de detención por la policía ----   
   etra %>%   
     dist_frecuencias(q0037)

# Gráfico 16. Agentes de discriminación ----   
   g5 <- etra %>%   
     select (starts_with("q0045")) |> 
     select (-q0045_0012,-q0045_other,
            -q0045_0010,-q0045_0011 ) |> 
    dist_frecuencias_respuesta_multiple() |> 
    filter (valor != "Por nadie, no me sentí discriminado" &
            valor != "Prefiero no contestar") |> 
     mutate (etiquetas = generar_etiquetas(.x1= porcentaje,
                                           .x2= n,
                                           .tipo ="porcentajes_frecuencias")) %>%
     grafico_barras_dos_variables_etiqueta (.x=valor,
                                            .y=porcentaje,
                                            .ordenX = porcentaje,
                                            .etiquetas = etiquetas)+
     geom_bar_text(contrast="FALSE", position = "stack", reflow = TRUE)+
     coord_flip()+
     scale_fill_jama()+
     labs(title = "Gráfico 5. Principales responsables de discriminación a adolescentes. AMBA. 2023",
          caption = "Fuente: Elaboración propia")+
     theme(plot.title.position = "plot")
   
   ggsave (plot=g5,
           dpi=320,
           device="tiff",
           filename="./graphs/g5.tiff")
  
#Gráfico 17. Motivos discriminación---- 
   etra |> 
     select (starts_with("q0044")) |> 
     select (-q0044_0015,-q0044_other) |> 
     dist_frecuencias_respuesta_multiple() |> 
     gt()
# Gráfico 18. Referentes ante problemas
   etra |> 
     select (starts_with("q0043")) |> 
     select (-q0043_0015,-q0043_other) |> 
     dist_frecuencias_respuesta_multiple() |> 
     gt()
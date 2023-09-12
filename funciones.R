# Función "únicos" ---- 
unicos <- function(.palabras) {
  .palabras <- stringr::str_sort(unique(.palabras))
  return(.palabras)
}	


#Función spanner género ---- 
spanner_genero <- function (tabla) 
{
  tabla <- tabla %>% 
    tab_spanner(label = "Femenino", columns = matches("Femenino")) %>%
    tab_spanner(label = "Masculino", columns = matches("Masculino")) %>% 
    tab_spanner(label = "Otrxs", columns = matches("Otrxs")) 
  return (tabla)
}

#Función dist-frecuencias ----
dist_frecuencias <- function (.data, .x)
{
  cuadro <- .data %>% 
    tabyl(var1 = {{.x}}, sort=TRUE) %>% 
    adorn_pct_formatting(digits = 1) %>% 
    arrange (desc(n)) %>% 
    mutate ({{.x}} := ifelse (is.na({{.x}}),"Sin datos",{{.x}})) %>% 
    bind_rows(slice(., which({{.x}} != "Sin datos")), slice(., which({{.x}} == "Sin datos"))) %>%
    tail(nrow(.)/2)
  
  cuadro$percent <- as.numeric(sub("%", "", cuadro$percent))
 
    if ("valid_percent" %in% colnames(cuadro)) {
    cuadro$valid_percent <- as.numeric(sub("%", "", cuadro$valid_percent))
    } 
  
  return (cuadro)
  
}


#Función cuadro bivariado ----
cuadro_bivariado <- function (.data, .x,.y)
{
cuadro <- .data %>% 
  tabyl(var1 = {{.y}},var2={{.x}},sort=TRUE,
        show_missing_levels = F) %>% 
  adorn_totals(where = "col") %>% 
  adorn_totals (where ="row") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  #adorn_ns(position = "front") %>% 
  arrange(desc(Total)) 

b <- cuadro[1] == "Sin datos"
cuadro <- rbind(cuadro[!b,], cuadro[b,]) 
b <- cuadro[1] == "Total"
cuadro <- rbind(cuadro[!b,], cuadro[b,])

return (cuadro)

}

#Función generar gt dist frecuencias ---- 
generar_gt_dist_frecuencias <- function (.cuadro, .titulo="",.subtitulo="",.fuente="",.x,.titulo_x="",.validos=FALSE) 
{
    if(.validos==TRUE) { 
      .cuadro <- .cuadro %>% 
      gt () %>% 
      cols_label({{.x}} := .titulo_x,n = "F", percent="%", valid_percent = "% sobre válidos") }
    else { 
    .cuadro <- .cuadro %>% 
    gt() %>% 
    cols_label({{.x}} := .titulo_x,n = "F", percent="%") } 
  .cuadro <- .cuadro %>% 
  tab_header(
    title = md(.titulo),
    subtitle = md(.subtitulo)) %>% 
    tab_source_note(source_note = .fuente) %>% 
    cols_label({{.x}} := .titulo_x)
    return (.cuadro)
}

generar_gt_respuesta_multiple <- function (.cuadro, .titulo="",.subtitulo="",.fuente="",.x,.titulo_x="",.validos=FALSE) 
{
  if(.validos==TRUE) { 
    .cuadro <- .cuadro %>% 
      gt () %>% 
      cols_label({{.x}} := .titulo_x,n = "F", percent="%", valid_percent = "% sobre válidos") }
  else { 
    .cuadro <- .cuadro %>% 
      gt() %>% 
      cols_label({{.x}} := .titulo_x,n = "F", percent="%") } 
  .cuadro <- .cuadro %>% 
    tab_header(
      title = md(.titulo),
      subtitle = md(.subtitulo)) %>% 
    tab_source_note(source_note = .fuente) %>% 
    cols_label({{.x}} := .titulo_x)
  return (.cuadro)
}




#Función generar GT cuadro bivariado ---- 
generar_gt_cuadro_bivariado <- function (.cuadro, .titulo="",.subtitulo="",.fuente="",.x,
                                         .titulo_x={{.x}}) {
{{.cuadro}} %>%   
  gt() %>%
  tab_header(
    title = md(.titulo),
    subtitle = md(.subtitulo)) %>% 
  tab_source_note(source_note = .fuente) %>% 
  cols_label({{.x}} := .titulo_x)
  }


#Funcuón fytporcentaje ---- 
fyporcentaje <- function (.tabla)
{
col_names = colnames(.tabla)
new_col_names = gsub(pattern = ".*cantidad.*",replacement = "F",col_names)
new_col_names = gsub(pattern = ".*porcentaje*",replacement = "%",new_col_names)
list_for_dynamic_cols_label = structure(as.list(new_col_names), names=col_names)
return (list_for_dynamic_cols_label)

}

#Función generar cuadro respuesta multiple ---- 
generar_cuadro_respuesta_multiple <- function(.data)
{
cuadro <- {{.data}} %>% 
    pivot_longer(everything(), names_to="nombre", values_to="valor") %>%
    group_by(nombre) %>%
    summarize(respuestas = sum(valor),
              total_casos = n(),
              total_sobre_casos = round(100 * respuestas / total_casos,1)) %>% 
    ungroup() %>% 
    mutate (total_respuestas = sum(respuestas)) %>% 
    mutate (total_sobre_respuestas = round(100 * respuestas / total_respuestas,1)) %>%  
    arrange(-respuestas)   
return (cuadro)
}

#Función generar cuadro respuesta múltiple bivariado ----
generar_cuadro_respuesta_multiple_bivariado <- function(.data,.cruce)
{
result <- {{.data}} %>%
  pivot_longer (-{{.cruce}}, names_to ="nombre", values_to = "valor")

result2 <- result %>%
  group_by({{.cruce}},nombre) %>% 
  summarise(respuestas = sum(valor),
            total_respuestas =n(),
            total_casos=nrow({{.data}}),
            porcentaje_casos=round(respuestas/total_casos * 100,1),
            porcentaje_respuestas = round (respuestas / total_respuestas * 100,1)) 

result3 <- result2 %>%
  select (-total_casos,-total_respuestas) %>% 
  pivot_wider(names_from = {{.cruce}}, values_from = 
                c("respuestas","porcentaje_casos","porcentaje_respuestas")) %>%  
  mutate (total_casos = nrow ({{.data}})) %>% 
  adorn_totals(.,where="row")
}



generar_cuadro_respuesta_multiple_bivariado2 <- function(.data,.cruce)
{
  
  .cruce <- enquo(.cruce)
  nombre <- quo_name(.cruce)
  
  glue_valor<- paste0("{",nombre,"}_{.value}")
  
  result <- .data %>%
    pivot_longer (-!!.cruce, names_to ="nombre", values_to = "valor")
  
  result2 <- result %>%
    group_by(!!.cruce,nombre) %>% 
    summarise(a.respuestas = sum(valor),
              totalrespuestas =n(),
              totalcasos=nrow({{.data}}),
              c.porcentajecasos=round(a.respuestas/totalcasos * 100,2),
              b.porcentajerespuestas = round (a.respuestas / totalrespuestas * 100,2)) 
  
  result3 <- result2 %>%
    arrange(!!.cruce) %>% 
    select (-totalcasos,-totalrespuestas) %>% 
    pivot_wider(names_from = !!.cruce, names_glue=glue_valor,values_from = 
                  c("a.respuestas","c.porcentajecasos","b.porcentajerespuestas")) %>%  
    mutate (total_casos = nrow ({{.data}})) %>% 
    adorn_totals(.,where="row") 
result3 <- result3[,sort(colnames(result3))]
col_to_move <- "nombre"
result3 <- cbind(result3[col_to_move], result3[setdiff(colnames(result3), col_to_move)])


}

 mover_al_final <- function (.data,.variable,.valor)
 {
.data %>% 
 bind_rows(slice(., which({{.variable}} != .valor)), slice(., which({{.variable}} == .valor))) %>%
 tail(nrow(.)/2)
 }
#Funcion gráfico barra dos variables ----
grafico_barras_dos_variables <- function (.data,.x,.y,.ordenX={{.x}},.etiquetas="") 
{
g <- {{.data}} %>% 
  ggplot(aes(x=reorder({{.x}},{{.ordenX}}),y={{.y}}))+
  geom_bar(position="dodge",stat="identity",fill="#01a2d9")+
  labs(x="",y="", fill = "") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )+
   geom_text(aes(x={{.x}},label={{.etiquetas}}),
              position = position_dodge(width=1),vjust=-0.5,size=3)+
  scale_x_discrete(labels = label_wrap(10)) 
  return (g)
}



generar_etiquetas <- function (.tipo,.x1=NULL,.x2=NULL) 
{
  etiquetas <- ""
if (!is.null(.x2) & !is.null(.x1))
    {
      if (.tipo == "frecuencias_porcentajes") {
        etiquetas <- paste0 (.x1,"(",.x2,"%)")
      }
      else if (.tipo == "porcentajes_frecuencias") {
        etiquetas <- paste0 (.x1,"%(",.x2,")")
      }
    }
  else if (!is.null(.x1))
  {
    if (.tipo == "porcentajes")
    {
      etiquetas <- paste0 (.x1,"%")
    }
    else if (.tipo == "frecuencias")
    {
      etiquetas <- .x1
    }
    else {
      print ("Tipo incorrecto")
      etiquetas <- ""
    }
  }
  else {
    print ("Error en los parámetros")
    etiquetas <- ""
  }
return (etiquetas)
}

grafico_apiladas_tres_variables <- function (.data,.x,.y,.fill,.ordenX=-{{.y}},
                                             .ordenfill={{.fill}},.etiquetas="")
{
  require ("ggthemes") 
{{.data}} %>%
    ggplot (aes(x=reorder({{.x}},{{.ordenX}}),y={{.y}},fill=reorder({{.fill}},{{.ordenfill}}),
                label = {{.etiquetas}}))+
    geom_bar(position="stack",stat="identity")+
    labs(x="",y="", fill = "") +
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    ) +
    ggfittext::geom_bar_text(position="stack",place = "center")+
    scale_fill_economist()
}

grafico_barras_tres_variables <- function (.data,.x,.y,.fill,.etiquetas="",
                                           .ordenX=-{{.y}},.ordenfill={{.fill}},
                                           .tipoEtiqueta="top") {
  require ("ggthemes") 
  {{.data}} %>% 
    ggplot(aes(x=reorder({{.x}},{{.ordenX}}),y={{.y}},fill=reorder({{.fill}},{{.ordenfill}})))+
    geom_bar(position="dodge",stat="identity")+
    labs(x="",y="", fill = "") +
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    ) +
    geom_text(aes(label={{.etiquetas}}),position = position_dodge(width=0.9),
              vjust= case_when
              (.tipoEtiqueta=="top" ~ -0.5,
               .tipoEtiqueta== "inside_top" ~ 1.5),size=3)+
    scale_fill_economist()
}

cruce_bivariado <- function(.data,.x,.y) {
  cuadro <- {{.data}} %>% 
  filter (!is.na({{.x}})) %>% 
  filter (!is.na({{.y}})) %>% 
  group_by({{.x}},{{.y}}) %>% 
  summarise(cantidad=n()) %>% 
  mutate (porcentaje=round(cantidad/sum(cantidad)*100,2)) 
  return (cuadro)
}
summaryDF <- function(indf) {
  require(splitstackshape)
  temp <- data.table(summary(indf))[, c("V2", "N"), with = FALSE]
  dcast.data.table(cSplit(temp, "N", ":")[!is.na(N_1)],
                   N_1 ~ V2, value.var = "N_2")
}

cruce_bivariado_con_total <- function (.data,.cruce_x,.cruce_y) 
{
  result <-  {{.data}} %>%   
    group_by({{.cruce_x}}, {{.cruce_y}}) %>%
    summarise(cantidad = n()) %>%
    mutate (porcentaje = round(cantidad / sum (cantidad) * 100,2)) |> 
    bind_rows(
      {{.data}} %>%
        group_by({{.cruce_y}}) %>%
        summarise(cantidad = n()) %>%
        mutate (porcentaje = round(cantidad / sum (cantidad) * 100,2)) |> 
        mutate({{.cruce_x}} := "Total"))
  return (result)
}

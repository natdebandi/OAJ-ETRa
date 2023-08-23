# Carga ---- 
source ("./ETRa_carga.R")

# Funciones ---- 
source ("./funciones.R")

# Limpieza --- 
source ("./limpieza.R")

# Tablas ---- 
# Tabla 1. Cantidad de encuestas por fecha y lugar de realización
etra %>% 
  group_by(orden,fecha_lugar) %>% 
    summarise(cantidad=n()) %>% 
  ungroup() |> 
  mutate (porcentaje=round(cantidad/sum(cantidad)*100,2)) %>% 
  arrange (orden) %>% 
  adorn_totals (where ="row") %>% 
  gt()

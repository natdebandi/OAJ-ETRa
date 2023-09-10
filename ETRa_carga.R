# Librerías ---- 
library(ggplot2)
library(tidyverse)
library(dplyr)
library (lubridate)
library (gt)
library (janitor)
library (haven)
library (scales)
library (stringr)
library (ggfittext)
# Cargar datos ------------------------------------------------------------
rm(list = ls())
#mydir<-"G:/My Drive/2. Data-work/R-workspace/PISAC"
#setwd(mydir)
mydir<-getwd()

# Uso la base de SPSS porque la de CSV tiene una manera RARÍSIMA de agrupar las variables ----
#etra<-read.csv("data/etra.csv", header = TRUE, sep = ",", quote = "\"", encoding ="UTF-8")

etra<- haven::read_spss("./data/etra.sav")

# Cambios de nombre 
etra <- etra |> 
  rename(Edad = q0002) |> 
  rename (Genero = q0003) %>% 
  rename (Familia_Migrante = q0005) %>% 
  rename (Trabajo_Previo = q0022) %>%  
  rename (Trabajo_Previo_otro = q0022_other) |> 
  rename (Sensacion_Policia=q0039) |> 
  rename (Sensacion_Policia_otro=q0039_other)

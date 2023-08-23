# Librerías ---- 
library(ggplot2)
library(tidyverse)
library(dplyr)
library (lubridate)
library (gt)
library (janitor)


# Cargar datos ------------------------------------------------------------
rm(list = ls())
#mydir<-"G:/My Drive/2. Data-work/R-workspace/PISAC"
#setwd(mydir)
mydir<-getwd()
etra<-read.csv("data/etra.csv", header = TRUE, sep = ",", quote = "\"", encoding ="UTF-8")



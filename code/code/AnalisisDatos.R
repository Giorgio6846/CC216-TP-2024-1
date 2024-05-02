#Instalacion de librerias
install.packages("dplyr")
install.packages("ggplot2")

#Uso de librerias
library(dplyr)
library(ggplot2)

#Configuracion de Working Directory
setwd('../../data')
getwd()

#Lectura de archivo
originalFile <- read.csv('hotel_bookings_modified.csv', header = TRUE, stringsAsFactors = FALSE)
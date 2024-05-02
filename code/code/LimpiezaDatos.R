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
originalFile <- read.csv('hotel_bookings_original.csv', header = TRUE, stringsAsFactors = FALSE)

#Limpieza de archivos
 
View(originalFile)
summary(originalFile)

  #Ver el tipo de dato por cada columna
  str(originalFile)

  #Conversion de columna de integer to bool
    
    #Columna: is_canceled
    #Al no encontrar ningun dato faltante en la columna is_canceled se puede continuar con el cambio  
    originalFile$is_canceled <- as.logical(originalFile$is_canceled)
  
    #Columna: is_repeated_guest
    #Al no encontrar ningun dato faltante en la columna is_repeated_guest se puede continuar con el cambio
    originalFile$is_repeated_guest <- as.logical(originalFile$is_repeated_guest)
    
    #Despues de la conversion a logical
    str(originalFile)
    
  #Conversion de columna de character to date
    
    #Columna: reservation_status_date
    #Al no encontrar ningun dato faltante en la columna reservation_status_date se puede continuar con el cambio  
    originalFile$reservation_status_date <- as.Date(originalFile$reservation_status_date, "%Y-%m-%d")
  
    #Despues de la conversion a logical
    str(originalFile)
    
  #Conversion de NA
    
    #Obtener la cantidad de NA en cada columna asi como informacion del dataframe antes del cambio de datos
    summary(originalFile)
    colSums(is.na(originalFile))
  
    #Se ha encontrado que en la columna children tiene 4 NA por ello se ha considerado omitirlos al no ser una gran cantidad
    originalFile <- na.omit(originalFile)
    
    #Obtener la cantidad de NA despues de haber omitido las celdas
    summary(originalFile)
    colSums(is.na(originalFile))
  
  #Se han eliminado los 4 NA babies

#Debido a que se va a usar recurrente esta funcion se va a crear una para visualizar las columnas
visCols <- function(colVis) {
  ggplot(data = originalFile, mapping = aes(x = colVis)) + 
    geom_boxplot()
}

visCountCols <- function(colVis) {
  ggplot(data = originalFile, mapping = aes(x = colVis), stat = "identity") +
    geom_bar()
}
    
  #Visualizacion de datos valores atipicos
    
    summary(originalFile)
    #Se asume que hay valores atipicos en adr, adults, lead_time, children, babies, days_in_waiting_list
    
    #Columna adr
        
      #Visualizacion mediante ggplot
      visCols(originalFile$adr)
      
      #Se ha encontrado que la columna adr hay valores atipicos mayores a 4000
      #Conteo de valores mayores de 4000
      sum(originalFile$adr > 4000, rm.NA=TRUE)
      
      #Se han encontrado dos valores valores atipicos, al no ser una gran cantidad en la cual afecte el analisis se ha decidido eliminarlos
      
      #Transformacion a NA
      originalFile <- originalFile %>%
        mutate(adr = ifelse(adr < 0 | adr > 4000, NA, adr))
      colSums(is.na(originalFile))
      
      originalFile <- na.omit(originalFile)
      
      #Visualizacion mediante ggplot
      visCols(originalFile$adr)
      
    #Columna adults
      
      #Visualizacion mediante ggplot
      visCountCols(originalFile$adults)

      #Se han encontrado que la columna adults hay valores atipicos mayores a 10
      sum(originalFile$adults >= 10, rm.NA=TRUE)

      #Se han encontrado catorce valores valores atipicos, al no ser una gran cantidad en la cual afecte el analisis se ha decidido eliminarlos
      originalFile <- originalFile %>%
        mutate(adults = ifelse(adults >= 10, NA, adults))
      colSums(is.na(originalFile))
      
      originalFile <- na.omit(originalFile)
      
      #Visualizacion mediante ggplot
      visCountCols(originalFile$adults)
      
    #Columna leadtime
      
      #Visualizacion mediante ggplot
      visCols(originalFile$lead_time)
      
      #Se han encontrado que la columna lead_time hay valores atipicos mayores a 650
      #Conteo de valores mayores o iguales de 650
      sum(originalFile$lead_time >= 650, rm.NA=TRUE)
      
      #Se han encontrado tres valores valores atipicos, al no ser una gran cantidad en la cual afecte el analisis se ha decidido eliminarlos
      originalFile <- originalFile %>%
        mutate(lead_time = ifelse(lead_time >= 650, NA, lead_time))
      colSums(is.na(originalFile))
      
      originalFile <- na.omit(originalFile)
      
      #Visualizacion mediante ggplot
      visCols(originalFile$lead_time)
      
    #Columna children
      
      #Visualizacion mediante ggplot
      visCountCols(originalFile$children)
      
      #Se han encontrado que la columna adults hay valores atipicos mayores a 5
      #Conteo de valores mayores o iguales de 5
      sum(originalFile$children >= 5, rm.NA=TRUE)
      
      #Se han encontrado dos valores valores atipicos, al no ser una gran cantidad en la cual afecte el analisis se ha decidido eliminarlos
      originalFile <- originalFile %>%
        mutate(children = ifelse(children >= 5, NA, children))
      colSums(is.na(originalFile))
      
      originalFile <- na.omit(originalFile)
      
      #Visualizacion mediante ggplot
      visCountCols(originalFile$children)

    #Columna babies
      
      #Visualizacion mediante ggplot
      visCountCols(originalFile$babies)
      
      #Se han encontrado que la columna adults hay valores atipicos mayores a 5
      #Conteo de valores mayores o iguales de 5
      sum(originalFile$babies >= 5, rm.NA=TRUE)
      
      #Se han encontrado dos valores valores atipicos, al no ser una gran cantidad en la cual afecte el analisis se ha decidido eliminarlos
      originalFile <- originalFile %>%
        mutate(babies = ifelse(babies >= 5, NA, babies))
      colSums(is.na(originalFile))
      
      originalFile <- na.omit(originalFile)
      
      #Visualizacion mediante ggplot
      visCountCols(originalFile$babies)
      
    #Columna days_in_waiting_list
      
      #Visualizacion mediante ggplot
      visCols(originalFile$days_in_waiting_list)
      
      #Se han encontrado que la columna lead_time hay valores atipicos mayores a 300
      #Conteo de valores mayores o iguales de 300
      sum(originalFile$days_in_waiting_list >= 300, rm.NA=TRUE)
      
      #Se han encontrado 76 valores valores atipicos, al no ser una gran cantidad en la cual afecte el analisis se ha decidido eliminarlos
      originalFile <- originalFile %>%
        mutate(days_in_waiting_list = ifelse(days_in_waiting_list >= 300, NA, days_in_waiting_list))
      colSums(is.na(originalFile))
      
      originalFile <- na.omit(originalFile)
      
      #Visualizacion mediante ggplot
      visCols(originalFile$days_in_waiting_list)

#Guardado de archivos
write.csv(originalFile, 'hotel_bookings_modified.csv')
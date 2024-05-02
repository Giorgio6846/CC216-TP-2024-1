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

  #Obtener la cantidad de NA en cada columna
  colSums(is.na(originalFile))
 
  #Conversion de columna de integer to bool
    
    #Columna: is_canceled
    #Al no encontrar ningun dato faltante en la columna is_canceled se puede continuar con el cambio  
    originalFile$is_canceled <- as.logical(originalFile$is_canceled)
  
    #Columna: is_repeated_guest
    #Al no encontrar ningun dato faltante en la columna is_repeated_guest se puede continuar con el cambio
    originalFile$is_repeated_guest <- as.logical(originalFile$is_repeated_guest)
    
    #Despues de la conversion a logical
    str(originalFile)
    
  #Reconocimiento de datos de caracteres a lista para deteccion de anomalias

    list = as.list(df)
    print(list)
    
  
#Guardado de archivos
write.csv(originalFile, 'hotel_bookings_modified.csv')

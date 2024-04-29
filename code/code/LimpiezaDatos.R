#Configuracion de Working Directory
setwd('../../data')
getwd()

#Lectura de archivo
originalFile <- read.csv('hotel_bookings_original.csv', header = TRUE, stringsAsFactors = FALSE)
modifiedFile <- originalFile
#Limpieza de archivos
 
View(originalFile)
summary(originalFile)

#Guardado de archivos
write.csv(modifiedFile, 'hotel_bookings_modified.csv')

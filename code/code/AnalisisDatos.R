#Instalacion de librerias
install.packages("dplyr")
install.packages("ggplot2")

#Uso de librerias
library(dplyr)
library(ggplot2)
library(scales) # for percent formatting


#Configuracion de Working Directory
setwd('../../data')
getwd()

#Lectura de archivo
analyzedData <- read.csv('hotel_bookings_modified.csv', header = TRUE, stringsAsFactors = FALSE)

#Conversion de columna de character to date

#Columna: reservation_status_date
#Al no encontrar ningun dato faltante en la columna reservation_status_date se puede continuar con el cambio  
analyzedData$reservation_status_date <- as.Date(analyzedData$reservation_status_date, "%Y-%m-%d")


#Informacion de datos
head(analyzedData)
summary(analyzedData)
str(analyzedData)

#Pregunta 1 ¿Cuántas reservas se realizan por tipo de hotel? o ¿Qué tipo de hotel prefiere la gente?

analyzedData %>%
  count(hotel)

ggplot(data = analyzedData, mapping = aes(x = hotel), stat = "identity") +
  geom_bar()

#Pregunta 2 ¿Está aumentando la demanda con el tiempo?

wait_time <- analyzedData %>%
  group_by(reservation_status_date) %>%
    summarize(
      time = mean(lead_time, na.rm = TRUE),
    )

ggplot(wait_time, aes(reservation_status_date, time)) +
        geom_line()

#Pregunta 3 ¿Cuándo se producen las temporadas de reservas: alta, media y baja?

#Pregunta 4 ¿Cuándo es menor la demanda de reservas?




#Pregunta 5 ¿Cuántas reservas incluyen niños y/o bebes?

onlyBabiesorChildrens <- filter(analyzedData, babies > 0 | children > 0)
onlyBabiesandChildrens <- filter(analyzedData, babies > 0 & children > 0)

#Pregunta 6 ¿Es importante contar con espacios de estacionamiento?


parkingGroup <- group_by(analyzedData, required_car_parking_spaces)
parkingCount <- summarise(parkingGroup, 
                          parkingUsed = n() / sum(n()) * 100)

str(parkingCount)


parkingCount$required_car_parking_spaces
parkingCount$parkingUsed

barplot(parkingCount$parkingUsed, 
        main="Usos del parking", 
        names = parkingCount$required_car_parking_spaces,
        ylab = "Porcentaje (%)")
)


#Pregunta 7 ¿En qué meses del año se producen más cancelaciones de reservas?

cancelsMonth <- analyzedData %>%
  group_by(format(reservation_status_date, "%m"), format(reservation_status_date, "%Y")) %>%
  summarize(
    amountOfCanceled = sum(is_canceled, na.rm = TRUE),
  )

colnames(cancelsMonth)
names(cancelsMonth)[1] <- "month"
names(cancelsMonth)[2] <- "year"


ggplot(cancelsMonth) +
  geom_bar(aes(x = month, y = amountOfCanceled), stat = "identity")

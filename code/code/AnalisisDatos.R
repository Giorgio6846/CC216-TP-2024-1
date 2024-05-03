#Instalacion de librerias
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")

#Uso de librerias
library(dplyr)
library(ggplot2)
library(scales) # for percent formatting
library(tidyverse)


#Configuracion de Working Directory
setwd('../../data')
getwd()

#Lectura de archivo
analyzedData <- read.csv('hotel_bookings_modified.csv', header = TRUE, stringsAsFactors = FALSE)

analyzedDataHotel1 <- analyzedData %>% filter(hotel == "City Hotel") 
analyzedDataHotel2 <- analyzedData %>% filter(hotel == "Resort Hotel")

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
  geom_bar(aes(color = hotel, fill = hotel))

#Pregunta 2 ¿Está aumentando la demanda con el tiempo?

wait_time <- analyzedData %>%
  group_by(format(reservation_status_date, "%Y"), hotel) %>%
    summarize(
      time = mean(lead_time, na.rm = TRUE),
    )

view(wait_time)

colnames(wait_time)
names(wait_time)[1] <- "year"

ggplot(data = wait_time, aes(year,time, group = hotel)) +
  geom_line(aes(color = hotel)) + 
  geom_point(aes(group = hotel))

#Pregunta 3 ¿Cuándo se producen las temporadas de reservas: alta, media y baja?

#Pregunta 4 ¿Cuándo es menor la demanda de reservas?

table(analyzedData$arrival_date_month)

nombre_meses <- c("January", "February", "March", "April", "May", "June", 
                  "July", "August", "September", "October", "November", "December")

freq_meses <- table(factor(analyzedData$arrival_date_month, levels = nombre_meses))

barplot(freq_meses, names.arg = nombre_meses, 
        main = "Frecuencia de reservas por mes",
        xlab = "Mes", ylab = "Frecuencia",
        col = "steelblue")

mes_menor_demanda <- nombre_meses[which.min(freq_meses)]


#Pregunta 5 ¿Cuántas reservas incluyen niños y/o bebes?

onlyBabiesorChildrens <- filter(analyzedData, babies > 0 | children > 0)
onlyBabiesandChildrens <- filter(analyzedData, babies > 0 & children > 0)

#Pregunta 6 ¿Es importante contar con espacios de estacionamiento?


calcularUsoEstacionamiento <- function(dataFrame) { # create a function with the name my_function
  parkingGroup <- group_by(dataFrame, required_car_parking_spaces)
  parkingCount <- summarise(parkingGroup, 
                            parkingUsed = n() / nrow(dataFrame) * 100)
  
  parkingCount
  
  ggplot(parkingCount, aes(x = required_car_parking_spaces, y = parkingUsed)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Usos del estacionamiento", x = "Espacios de estacionamiento requeridos", y = "Porcentaje (%)") +
    scale_y_continuous(limits = c(0, 100))
}

#Primero hotel
calcularUsoEstacionamiento(analyzedDataHotel1)
#Segundo hotel
calcularUsoEstacionamiento(analyzedDataHotel2)
#Ambos hoteles
calcularUsoEstacionamiento(analyzedData)


#Pregunta 7 ¿En qué meses del año se producen más cancelaciones de reservas?

cancelsMonth <- analyzedData %>%
  group_by(format(reservation_status_date, "%m"), hotel) %>%
  summarize(
    amountOfCanceled = sum(is_canceled, na.rm = TRUE),
  )

cancelsMonth

colnames(cancelsMonth)
names(cancelsMonth)[1] <- "month"

view(cancelsMonth)

ggplot(data = cancelsMonth, aes(month, amountOfCanceled, hotel)) +
  geom_point(aes(color = hotel)) + 
  geom_line(aes(group = hotel))
<<<<<<< HEAD
  
#Pregunta 9 ¿Los usuarios repetidos tienden a consumir / gastar mas?
=======

#Pregunta 8 ¿Que tipo de comida los clientes han obtenido dependiendo del hotel?

foodClient <- analyzedData %>%
  group_by(hotel, meal) %>%
  filter(meal != "Undefined" & meal != "SC") %>%
  summarize(
    n = n()
  )

foodClient

ggplot(data = foodClient, aes(meal, n)) +
  geom_point(aes(color = hotel)) +
  geom_line(aes(group = hotel))
>>>>>>> 4f09a419eb234a7f91e59426d0e797559cf2dd1c

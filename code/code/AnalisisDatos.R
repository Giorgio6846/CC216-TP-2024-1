#Instalacion de librerias
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("scales")

#Uso de librerias
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)


#Configuracion de Working Directory
setwd('../../data')
getwd()

#Lectura de archivo
analyzedData <- read.csv('hotel_bookings_modified.csv', header = TRUE, stringsAsFactors = FALSE)
analyzedData$AdrFinal <- analyzedData$adr * (analyzedData$stays_in_weekend_nights + analyzedData$stays_in_week_nights)

analyzedDataHotel1 <- analyzedData %>% filter(hotel == "City Hotel")
analyzedDataHotel2 <- analyzedData %>% filter(hotel == "Resort Hotel")

nombre_meses <- c("January", "February", "March", "April", "May", "June", 
                  "July", "August", "September", "October", "November", "December")

freq_meses <- table(factor(analyzedData$arrival_date_month, levels = nombre_meses))
freq_meses_hotel_1 <- table(factor(analyzedData$arrival_date_month[analyzedData$hotel == "Resort Hotel"], levels = nombre_meses))
freq_meses_hotel_2 <- table(factor(analyzedData$arrival_date_month[analyzedData$hotel == "City Hotel"], levels = nombre_meses))

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

  #Pregunta 1a
  typeHotelbyCountry <- analyzedData %>%
    group_by(country) %>%
    summarize(
      n = n(),
    )
  
  typeHotelbyCountryHotel <- analyzedData %>%
    group_by(country, hotel) %>%
    summarize(
      n = n(),
    )
  
  print(typeHotelbyCountry[order(typeHotelbyCountry$n, decreasing = TRUE),])
  print(typeHotelbyCountryHotel[order(typeHotelbyCountryHotel$n, decreasing = TRUE),])

View(typeHotelCountry)

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

tabla_frecuencia <- table(analyzedData$arrival_date_month)


meses_coloreo <-names(freq_meses)


freq_meses_ordenada <- sort(tabla_frecuencia, decreasing = TRUE)
freq_meses_ordenada_hotel_1 <- sort(freq_meses_hotel_1, decreasing = TRUE)
freq_meses_ordenada_hotel_2 <- sort(freq_meses_hotel_2, decreasing = TRUE)

frecuencias <- as.vector(freq_meses_ordenada)
meses <- names(freq_meses_ordenada)

temporada_alta <- meses[frecuencias >= freq_meses_ordenada[4]]
temporada_baja <- meses[frecuencias <= freq_meses_ordenada[9]]
temporada_media <- meses[!(meses %in% c(temporada_alta, temporada_baja))]

temporada_alta_hotel_1 <- meses[frecuencias >= freq_meses_ordenada[4]]
temporada_baja_hotel_1 <- meses[frecuencias <= freq_meses_ordenada[9]]
temporada_media_hotel_1 <- meses[!(meses %in% c(temporada_alta, temporada_baja))]

temporada_alta_hotel_2 <- meses[frecuencias >= freq_meses_ordenada[4]]
temporada_baja_hotel_2 <- meses[frecuencias <= freq_meses_ordenada[9]]
temporada_media_hotel_2 <- meses[!(meses %in% c(temporada_alta, temporada_baja))]

cat("Temporada Alta:", temporada_alta, "\n")
cat("Temporada Media:", temporada_media, "\n")
cat("Temporada Baja:", temporada_baja, "\n")

cat("Temporada Alta en el Resort Hotel:", temporada_alta_hotel_1, "\n")
cat("Temporada Media en el Resort Hotel:", temporada_media_hotel_1, "\n")
cat("Temporada Baja en el Resort Hotel:", temporada_baja_hotel_1, "\n")

cat("Temporada Alta en el City Hotel:", temporada_alta_hotel_2, "\n")
cat("Temporada Media en el City Hotel:", temporada_media_hotel_2, "\n")
cat("Temporada Baja en el City Hotel:", temporada_baja_hotel_2, "\n")

colores <- c("Temporada Alta" = "#FF5851", "Temporada Media" = "#414A6B", "Temporada Baja" = "#F3C130")

barplot(freq_meses, names.arg = nombre_meses,
        main = "Frecuencia de reservas por mes ordenados por temporada",
        xlab = "Mes", ylab = "Frecuencia",
        col = ifelse(meses_coloreo %in% temporada_alta, colores["Temporada Alta"],
                     ifelse(meses_coloreo %in% temporada_baja, colores["Temporada Baja"], colores["Temporada Media"])))

barplot(freq_meses_hotel_1, names.arg = nombre_meses,
        main = "Frecuencia de reservas por mes ordenados por temporada en el Resort Hotel",
        xlab = "Mes", ylab = "Frecuencia",
        col = ifelse(meses_coloreo %in% temporada_alta, colores["Temporada Alta"],
                     ifelse(meses_coloreo %in% temporada_baja, colores["Temporada Baja"], colores["Temporada Media"])))

barplot(freq_meses_hotel_2, names.arg = nombre_meses,
        main = "Frecuencia de reservas por mes ordenados por temporada en el City Hotel",
        xlab = "Mes", ylab = "Frecuencia",
        col = ifelse(meses_coloreo %in% temporada_alta, colores["Temporada Alta"],
                     ifelse(meses_coloreo %in% temporada_baja, colores["Temporada Baja"], colores["Temporada Media"])))


#Pregunta 4 ¿Cuándo es menor la demanda de reservas?
table(analyzedData$arrival_date_month)


barplot(freq_meses, names.arg = nombre_meses, 
        main = "Frecuencia de reservas por mes en total",
        xlab = "Mes", ylab = "Frecuencia",
        col = "blue")

barplot(freq_meses_hotel_1, names.arg = nombre_meses, 
        main = "Frecuencia de reservas por mes en el Resort Hotel ",
        xlab = "Mes", ylab = "Frecuencia",
        col = "green")

barplot(freq_meses_hotel_2, names.arg = nombre_meses, 
        main = "Frecuencia de reservas por mes en el City Hotel",
        xlab = "Mes", ylab = "Frecuencia",
        col = "red")

mes_menor_demanda <- nombre_meses[which.min(freq_meses)]
mes_menor_demanda_hotel_1 <- nombre_meses[which.min(freq_meses_hotel_1)]
mes_menor_demanda_hotel_2 <- nombre_meses[which.min(freq_meses_hotel_2)]

cat("Mes con menor demanda de reservas en total:", mes_menor_demanda, "\n")
cat("Mes con menor demanda de reservas del Resort Hotel:", mes_menor_demanda_hotel_1, "\n")
cat("Mes con menor demanda de reservas del City Hotel:", mes_menor_demanda_hotel_2, "\n")

#Pregunta 5 ¿Cuántas reservas incluyen niños y/o bebes?
onlyBabiesorChildrens <- filter(analyzedData, babies > 0 | children > 0)
onlyBabiesandChildrens <- filter(analyzedData, babies > 0 & children > 0)
noBabiesandChildren <- filter(analyzedData, babies == 0 & children == 0)

groupedOrBabies <- onlyBabiesorChildrens %>%
  group_by(hotel) %>%
  summarize(
    n = n()
  )

groupedAndBabies <- onlyBabiesandChildrens %>%
  group_by(hotel) %>%
  summarize(
    n = n()
  )

names(groupedOrBabies)[2] <- "orBabiesChildren"
names(groupedAndBabies)[2] <- "andBabiesChildren"

babiesChildrens <- merge(groupedOrBabies, groupedAndBabies, by = "hotel")
babiesChildrens
  
ggplot(babiesChildrens) +
  geom_bar(mapping = aes(x = hotel, y = orBabiesChildren), stat = "identity")

ggplot(babiesChildrens) +
  geom_bar(mapping = aes(x = hotel, y = andBabiesChildren), stat = "identity")

#Pregunta 6 ¿Es importante contar con espacios de estacionamiento?


calcularUsoEstacionamiento <- function(dataFrame) {
  parkingGroup <- group_by(dataFrame, required_car_parking_spaces)
  parkingCount <- summarise(parkingGroup, 
                            parkingUsed = n() / nrow(dataFrame) * 100)
  
  print(parkingCount)
  
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

#Pregunta 9 ¿Los usuarios repetidos tienden a consumir / gastar mas respecto a los primerizos?

gastosTipoUsuario <- function(dataFrame) {
  tipoUsuario <- dataFrame %>% group_by(is_repeated_guest) %>% summarise(adrPromedio = mean(AdrFinal))
                                                           
  print(tipoUsuario)
  
  ggplot(tipoUsuario, aes(x = is_repeated_guest, y = adrPromedio)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Gastos vs Tipo Usuario", x = "Tipo de usuario", y = "Gastos ($)")
}

#Primero hotel
gastosTipoUsuario(analyzedDataHotel1)
#Segundo hotel
gastosTipoUsuario(analyzedDataHotel2)
#Ambos hoteles
gastosTipoUsuario(analyzedData)

#Pregunta 10 ¿En que mes los turistas gastan menos?
gasto_promedio_por_mes <- tapply(analyzedData$adr, analyzedData$arrival_date_month, mean)

gasto_promedio_por_mes <- gasto_promedio_por_mes[nombre_meses]

mes_gasto_menor <- names(gasto_promedio_por_mes)[which.min(gasto_promedio_por_mes)]

gasto_promedio_por_mes_hotel_1 <- tapply(analyzedData$adr[analyzedData$hotel == "Resort Hotel"], analyzedData$arrival_date_month[analyzedData$hotel == "Resort Hotel"], mean)
gasto_promedio_por_mes_hotel_1 <- gasto_promedio_por_mes_hotel_1[nombre_meses]

gasto_promedio_por_mes_hotel_2 <- tapply(analyzedData$adr[analyzedData$hotel == "City Hotel"], analyzedData$arrival_date_month[analyzedData$hotel == "City Hotel"], mean)
gasto_promedio_por_mes_hotel_2 <- gasto_promedio_por_mes_hotel_2[nombre_meses]

mes_gasto_menor_hotel_1 <- names(gasto_promedio_por_mes_hotel_1)[which.min(gasto_promedio_por_mes_hotel_1)]
mes_gasto_menor_hotel_2 <- names(gasto_promedio_por_mes_hotel_2)[which.min(gasto_promedio_por_mes_hotel_2)]

cat("El mes en el que los turistas gastan menos dinero en hoteles es:", mes_gasto_menor, "\n")
cat("Para el Resort Hotel, el mes en el que los turistas gastan menos dinero es:", mes_gasto_menor_hotel_1, "\n")
cat("Para el City Hotel, el mes en el que los turistas gastan menos dinero es:", mes_gasto_menor_hotel_2, "\n")

barplot(gasto_promedio_por_mes, names.arg = nombre_meses, 
        main = "Gasto promedio por mes",
        xlab = "Mes", ylab = "ADR",
        col = "steelblue")

barplot(gasto_promedio_por_mes_hotel_1, names.arg = nombre_meses, 
        main = "Gasto promedio por mes en el Resort Hotel",
        xlab = "Mes", ylab = "ADR",
        col = "steelblue")

barplot(gasto_promedio_por_mes_hotel_2, names.arg = nombre_meses, 
        main = "Gasto promedio por mes en el City Hotel",
        xlab = "Mes", ylab = "ADR",
        col = "steelblue")

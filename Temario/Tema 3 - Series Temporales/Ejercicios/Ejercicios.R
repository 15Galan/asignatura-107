# library(tidyverse)



# EJERCICIO 01 =================================================================

# Datos del enunciado: tabla
poblacion <- c(9.47,9.26,8.86,8.25,7.81,8.01,7.55,7.24,7.01,6.88,7.03)

# Crear un objeto de tipo serie temporal
serie <- ts(poblacion, start=c(1973,1))
  

  # Apartado (a) ---------------------------------------------------------------
  
  # Media movil de orden 4

    # Crear un objeto de tipo serie temporal
    serie4 <- ts(poblacion, start=c(1973,1), frequency=4)
    
    # Obtener la tendencia
    filtro4 <- c(.5,1,1,1,.5) / 4
    tenden4 <- filter(serie4, filtro4)

  
  # Media movil de orden 5
    
    # Crear un objeto de tipo serie temporal
    serie5 <- ts(poblacion, start=c(1973,1), frequency=5)
    
    # Obtener la tendencia
    filtro5 <- c(1,1,1,1,1) / 5
    tenden5 <- filter(serie5, filtro5)
  
  
  # Representacion grafica
  plot(serie4)
  lines(tenden4, col=2)
  
  plot(serie5)
  lines(tenden5, col=3)
  
  
  # Apartado (b) ---------------------------------------------------------------
  
  # Prediccion lineal
  modelo <- lm(serie ~ c(1:11)) # Porque la serie tiene 11 valores
  ajuste <- predict.lm(modelo)
  predic <- ts(ajuste, start=c(1973,1))
  
  # Representacion grafica
  plot(serie)
  lines(predic, col=2)
  
  
  # Apartado (c) ---------------------------------------------------------------
  tabla <- matrix(c(tenden4, tenden5, predic), byrow=T, nrow=3)
  


# EJERCICIO 02
  # Resuelto en Notion



# EJERCICIO 03 ================================================================

# Datos del enunciado: tabla
produccion <- c(3.9, 4, 4.8, 5.1, 5, 5.5, 6.1, 6.3, 6.9)

# Crear un objeto de tipo serie temporal
serie <- ts(produccion, start=c(2016,1), frequency=3)


# Obtener la tendencia
filtro <-  c(1, 1, 1) / 3
tenden <- filter(serie, filtro)


# Representacion grafica
plot(serie)
lines(tenden, col=2)


# Obtener la Comp. Estacionaria (E)
esta <- serie / tenden


# Eliminar la Comp. Aleatoria
data   <- matrix(esta, ncol=3, byrow=T)
medias <- colMeans(data, na.rm=T)

  # La media de las columnas debería ser 1, pero
  # como no es el caso, hay que normalizarla
  mean(medias)

  # Normalizar la media de las columnas
  norm <- medias / mean(medias)


# Nueva serie temporal
esta2 <- ts(rep(norm, 3), frequency=3)

# Serie desestacionalizada
deses <- serie / esta2

# Obtener la Comp. Aleatoria (A)
alea <- esta2 / norm

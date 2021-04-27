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


  # Manualmente ---------------------------------------------------------------

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
      indices <- medias / mean(medias)


    # Nueva serie temporal
    esta2 <- ts(rep(indices, 3), frequency=3)

    # Serie desestacionalizada
    deses <- serie / esta2

    # Obtener la Comp. Aleatoria (A)
    alea <- esta2 / norm


  # Automaticamente -----------------------------------------------------------

    # Descomponer la serie
    desc <- decompose(serie, type="mul")


    # Indices de la serie
    indices <- desc$figure



# EJERCICIO 05 ================================================================

# Datos del enunciado: tabla
precios <- c(25,29,34,38,42,45,70,77)
names(precios) <- 1975:1982


# Precios en base al precio de 1975
precios2 <- precios / 25
names(precios2) <- 1975:1982


# Los resultados reflejan la variacion del precio de 1 Kg de azucar entre 1975
# y 1982, tomando como base el precio del primer año, 25 u. Se puede observar
# que el precio fue aumentando hasta 1985, donde el kilo valió 3.08 veces mas
# que en 1975.

  # Si se habla de %, esto serviria para decir que 1 Kg de azucar aumento su
  # precio un 208 % en 7 años (3.08 - 1 = 2.08 -> 208 %).



# EJERCICIO 06 ================================================================

# Datos del enunciado
# 1998 / 1990 = 1.35

  # Apartado (a) --------------------------------------------------------------

  # Aplicando la regla de 3: 1000 / vA = 1.35
  valorA = 1000 / 1.35


  # Apartado (b) --------------------------------------------------------------

  # Aplicando la regla de 3: vB / 1000 = 1.35
  valorB = 1.35 * 1000

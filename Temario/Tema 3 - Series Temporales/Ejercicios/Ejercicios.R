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
  


# EJERCICIO 02 ================================================================

# (a) Falso.
# La tendencia puede obtenerse a traves de otros metodos,
# el de medias moviles solo es uno de ellos.


# (b) Verdadero.
# Una serie esta compuesta por las componentes 'T', 'E', 'C' y 'A',
# siendo 'E' la componente estacionaria.
# Desestacionalizar una serie consiste en eliminar 'E'.


# (c) Falso.
# Si los datos son cuatrimestrales, lo normal es usar 4 datos (uno por cuatrimestre).


# (d) Verdadero.
# Los datos no pueden ser claros, pero la pendiente de la tendencia es positiva,
# lo que implica que los valores crecen.



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



# EJERCICIO 04 ================================================================

# Datos del enunciado: tabla
consumo <- c(178.2,156.7,164.2,153.2,157.5,172.6,185.9,185.8,165.0,163.6,169.0,183.1,
             196.3,162.8,168.6,156.9,168.2,180.2,197.9,195.9,176.0,166.4,166.3,183.9,
             197.3,173.7,173.2,159.7,175.2,187.4,202.6,205.6,185.6,175.6,176.3,191.7,
             209.5,186.3,183.0,169.5,178.2,186.7,202.4,204.9,180.6,179.8,177.4,188.9,
             200.0,188.7,187.5,168.6,175.7,189.4,216.1,215.4,191.5,178.5,178.6,195.6,
             205.2,179.6,185.4,172.4,177.7,202.7,220.2,210.2,186.9,181.4,175.6,195.6)


# Apartado (a) ----------------------------------------------------------------

  # Crear un objeto de tipo serie temporal
  serie <- ts(consumo, start=c(1976,1), frequency=12)


  # Obtener la tendencia
  filtro <- c(.5,1,1,1,1,1,1,1,1,1,1,1,.5) / 12
  tenden <- filter(serie, filtro)


  # Obtener la Comp. Estacionaria (E)
  esta <- serie / tenden


  # Obtener las medias por columnas
  data <- matrix(esta, ncol=12, byrow=T)
  medias <- colMeans(data, na.rm=T)

    # Normalizar las medias
    indices <- medias / mean(medias)


# Apartado (b) ----------------------------------------------------------------

  # Desestacionalizar
  deses <- serie / indices


# Apartado (c) ----------------------------------------------------------------

  # Representacion grafica
  plot(serie)
  lines(deses, col=2)


# Apartado (d) ----------------------------------------------------------------

  # Prediccion lineal
  xx     <- 1:72            # Porque la serie tiene 12 * 6 valores
  modelo <- lm(deses ~ xx)
  ajuste <- predict.lm(modelo)
  predic <- ts(ajuste, start=c(1976,1), frequency=12)


  # Representacion grafica
  plot(tenden)              # Tendencia
  lines(predic, col=3)      # Tendencia real


# Apartado (e) ----------------------------------------------------------------

  # Partiendo de X/E = T·C·A, siendo 'X/E'...



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
# I_{1998 / 1990} = 1.35

  # Apartado (a) --------------------------------------------------------------

  # Aplicando la regla de 3: 1000 / vA = 1.35
  valorA = 1000 / 1.35


  # Apartado (b) --------------------------------------------------------------

  # Aplicando la regla de 3: vB / 1000 = 1.35
  valorB = 1.35 * 1000



# EJERCICIO 07 ================================================================

# Datos del enunciado
# I_{1990 / 1969} = 10.00
# I_{1990 / 2006} =  0.75


# Se sabe que x / 24000 = 0.75, y que x / y = 10.00,
# siendo 'x' el precio en 1990 e 'y' el precio en 1969.

x <- 0.75 * 24000
y <- x / 10.00

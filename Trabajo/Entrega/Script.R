# =============================================================================
#
# Trabajo elaborado por:
#   * Antonio J. Galán Herrera
#
# =============================================================================


# Importacion de paquetes necesarios
library("tidyverse")



# APARTADO 01 -----------------------------------------------------------------

  # Usando la funcion 'read_csv' del paquete 'readr'
  # se exporta el fichero con formato 'tibble'.
  datos <- read_csv("../Recursos/12306.csv", col_types=cols(sexo=col_factor(),
                                                            dietaEsp=col_factor(),
                                                            nivEstPad=col_factor(),
                                                            nivEstudios=col_factor(),
                                                            nivIngresos=col_factor()))



# APARTADO 02 -----------------------------------------------------------------

  # Se crea la columna 'IMC' segun el enunciado
  datos$IMC <- datos$peso / datos$altura^2
  
  
  
# APARTADO 03 -----------------------------------------------------------------

  # Se limpian las columnas con algun NA de los datos,
  # usando la funcion 'na.exclude()' o 'na.omit()'
  datos <- na.exclude(datos)

    # Esta nueva variable 'datos' es de un tamaño menor,
    # ya que no cuenta con algunas de sus columnas anteriores
  
  
  
# APARTADO 04 -----------------------------------------------------------------

  # Medias
  datosMedios <- c(peso_M     = mean(datos$peso),
                   altura_M   = mean(datos$altura),
                   edad_M     = mean(datos$edad),
                   tabaco_M   = mean(datos$tabaco),
                   ubes_M     = mean(datos$ubes),
                   carne_M    = mean(datos$carneRoja),
                   verduras_M = mean(datos$verduras),
                   deporte_M  = mean(datos$deporte),
                   droga_M    = mean(datos$drogas),
                   IMC_M      = mean(datos$IMC))

  # Desviaciones tipicas
  datosDesviados <- c(peso_DT     = sd(datos$peso),
                      altura_DT   = sd(datos$altura),
                      edad_DT     = sd(datos$edad),
                      tabaco_DT   = sd(datos$tabaco),
                      ubes_DT     = sd(datos$ubes),
                      carne_DT    = sd(datos$carneRoja),
                      verduras_DT = sd(datos$verduras),
                      deporte_DT  = sd(datos$deporte),
                      droga_DT    = sd(datos$drogas),
                      IMC_DT      = sd(datos$IMC))
  
  
  
# APARTADO 05 -----------------------------------------------------------------

  # Modelos
  mod01 <- lm(IMC ~ sexo, datos)
  mod02 <- lm(IMC ~ edad, datos)
  mod03 <- lm(IMC ~ tabaco, datos)
  mod04 <- lm(IMC ~ ubes, datos)
  mod05 <- lm(IMC ~ carneRoja, datos)
  mod06 <- lm(IMC ~ verduras, datos)
  mod07 <- lm(IMC ~ deporte, datos)
  mod08 <- lm(IMC ~ drogas, datos)
  mod09 <- lm(IMC ~ nivEstPad, datos)
  mod10 <- lm(IMC ~ nivEstudios, datos)
  mod11 <- lm(IMC ~ nivIngresos, datos)
  
    # Cada variable anterior contiene:
    # * El coeficiente de regresion (modXX$coefficients).
    # * El coeficiente de determinacion (summary(modXX)$r.squared).
  
  
  
# APARTADO 06 -----------------------------------------------------------------
  
  # Graficos de dispersion (variables cuantitativas)
  plot(datos$IMC, datos$edad, col=4)
  abline(mod02)
  
  plot(datos$IMC, datos$tabaco, col=4)
  abline(mod03)
  
  plot(datos$IMC, datos$ubes, col=4)
  abline(mod04)
  
  plot(datos$IMC, datos$carneRoja, col=4)
  abline(mod05)
  
  plot(datos$IMC, datos$verduras, col=4)
  abline(mod06)
  
  plot(datos$IMC, datos$deporte, col=4)
  abline(mod07)
  
  plot(datos$IMC, datos$drogas, col=4)
  abline(mod08)
  
  
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
  datos <- read_csv("../Recursos/12306.csv")


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
  datosMedios <- c(peso_M = mean(datos$peso), altura_M = mean(datos$altura),
                   edad_M = mean(datos$edad), tabaco_M = mean(datos$tabaco),
                   ubes_M = mean(datos$ubes), carne_M = mean(datos$carneRoja),
                   verduras_M = mean(datos$verduras), deporte_M = mean(datos$deporte),
                   droga_M = mean(datos$drogas), estPad_M = mean(datos$nivEstPad),
                   estudios_M = mean(datos$nivEstudios), ingresos_M = mean(datos$nivIngresos),
                   IMC_M = mean(datos$IMC))

  # Desviaciones tipicas
  datosDesviados <- c(peso_DT = sd(datos$peso), altura_DT = sd(datos$altura),
                      edad_DT = sd(datos$edad), tabaco_DT = sd(datos$tabaco),
                      ubes_DT = sd(datos$ubes), carne_DT = sd(datos$carneRoja),
                      verduras_DT = sd(datos$verduras), deporte_DT = sd(datos$deporte),
                      droga_DT = sd(datos$drogas), estPad_DT = sd(datos$nivEstPad),
                      estudios_DT = sd(datos$nivEstudios), ingresos_DT = sd(datos$nivIngresos),
                      IMC_DT = sd(datos$IMC))

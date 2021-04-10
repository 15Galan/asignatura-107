# library(tidyverse)

  # En esta ocasion, no es necesario importar esta libreria,
  # de hecho es mejor no hacerlo, ya que sobreescribe 'filter()'



# Datos que se usaran en estos ejemplos
datos <- scan("C:/ts01.dat")


# Convertir los datos en un objeto de serie temporal
serie <- ts(datos, start=c(1969,1), frequency=4)

  # Separa los datos en 4 columnas (frequency),
  # siendo 1969 la primera etiqueta (start)


# Obtener la tendencia
filtro <- c(.5,1,1,1,.5) / 4    # Filtro que aplica una media movil (orden par)
tenden <- filter(serie, filtro)


# Mostrar la serie temporal junto a la tendencia
plot(serie)
lines(tenden, col=2)  # Añade la tendencia al grafico anterior



# HIPOTESIS MULTIPLICATIVA =====================================================

  # Obtener la Comp. Estacionaria (E)
  esta <- serie / tenden  # Normalizar la tendencia
  plot(esta)
  
  
  # Eliminar la Comp. Aleatoria
  data   <- matrix(esta, byrow=T, ncol=4) # Componente estacionaria como matriz
  medias <- colMeans(data, na.rm=T)       # Calcula las medias por columna (sin NAs)
  
    # La media de las columnas deberia ser 1, pero
    # no es el caso, por lo que debe normalizarse
    medias
    mean(medias)
    
    
    # Normalizar la media de las columnas
    norm <- medias / mean(medias)
    
  
  # Obtener la Comp. Aleatoria (A)
  alea <- esta / norm
  


# HIPOTESIS ADITIVA ============================================================
  
  # Obtener la Comp. Estacionaria (E)
  esta <- serie - tenden  # Normalizar la tendencia
  plot(esta)
  
  
  # Eliminar la Comp. Aleatoria
  data   <- matrix(esta, byrow=T, ncol=4) # Componente estacionaria como matriz
  medias <- colMeans(data, na.rm=T)       # Calcula las medias por columna (sin NAs)
  
    # La media de las columnas deberia ser 0, pero
    # no es el caso, por lo que debe normalizarse
    medias
    mean(medias)
    
    
    # Normalizar la media de las columnas
    norm <- medias - mean(medias)
    
  
  # Obtener la Comp. Aleatoria (A)
  alea <- esta - norm
  
  
  
# FORMA AUTOMATICA =============================================================

  # Descomposicion segun la hipotesis multiplicativa
  desc_M <- decompose(serie, type="mul")
  plot(desc_M)
  
  # Descomposicion segun la hipotesis aditiva
  desc_A <- decompose(serie, type="add")
  plot(desc_A)
  
  
  # Descomponer incorrectamente
  serie2 <- ts(datos, frequency=3)        # Si la Comp. Aleatoria presenta
  error  <- decompose(serie2, type="add") # patrones, posiblemente la frecuencia
  plot(error)                             # sea incorrecta
  
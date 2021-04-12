# library(tidyverse)

  # En esta ocasion, no es necesario importar esta libreria,
  # de hecho es mejor no hacerlo, ya que sobreescribe 'filter()'



# Datos que se usaran en estos ejemplos
ruta  <- paste(getwd(), "/Recursos/ts01.dat", sep="")  # Ruta relativa
datos <- scan(ruta)


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
  
  # DESCOMPOSICION (MANUAL) ----------------------------------------------------
  
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
    
  

  # DESCOMPOSICION (AUTOMATICA) ------------------------------------------------
    
    # Usando 'decompose()'
    desc <- decompose(serie, type="mul")
    plot(desc)
    
    
    
  # DESESTACIONALIZACION -------------------------------------------------------
  
    # Serie desestacionalizada
    deses <- serie / norm
    plot(deses)
    
    
    # Prediccion lineal
    xx     <- 1:80            # Porque la serie tiene 80 valores
    modelo <- lm(deses ~ xx)
    ajuste <- predict.lm(modelo)
    predic <- ts(ajuste, start=c(1969,1), frequency=4)
    
    lines(predic, col=3)  # Añadir la prediccion al grafico anterior
    
    
    
# HIPOTESIS ADITIVA ============================================================
  
  # DESCOMPOSICION (MANUAL) ----------------------------------------------------
    
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
    
    
    
  # DESCOMPOSICION (AUTOMATICA) ------------------------------------------------
    
    # Usando 'decompose()'
    desc <- decompose(serie, type="add")
    plot(desc)
  
  
  # DESESTACIONALIZACION -------------------------------------------------------
  
    # Serie desestacionalizada
    deses <- serie - norm
    plot(deses)
    
    
    # Prediccion lineal
    xx     <- 1:80            # Porque la serie tiene 80 valores
    modelo <- lm(deses ~ xx)
    ajuste <- predict.lm(modelo)
    predic <- ts(ajuste, start=c(1969,1), frequency=4)
    
    lines(predic, col=3)  # Añadir la prediccion al grafico anterior
    
    
    
# DESCOMPOSICION CON FALLOS ====================================================
  
  # Suponiendo que se usan los mismos datos, donde ya se sabe que la frecuencia
  # es 4, pero esta vez se usara 3 de forma erronea
    
  serie2 <- ts(datos, frequency=3)        # Si la Comp. Aleatoria (A) presenta
  error  <- decompose(serie2, type="add") # patrones, posiblemente la frecuencia
  plot(error)                             # sea incorrecta
  
  
  
# AUTOCORRELACION ==============================================================

  # Reajustar los datos
  datos1 <- head(datos, -4) # Todos los valores excepto los 4 ultimos
  datos2 <- tail(datos, -4) # Todos los valores excepto los 4 primeros
  
  # Crear un modelo de regresion lineal
  modelo <- lm(datos1 ~ datos2)
  summary(modelo)
  
    # El modelo ofrece un coef. corr. lineal de 0.9678 con una frecuencia de 4;
    # el coef. corr. lineal varia con distintas frecuencias, y en este caso como
    # se obtiene el mayor coef. corr. lineal, indica que 4 es el mejor valor;
    # significa que «desplazando los valores cada 4 encajan muy bien entre ellos»
    
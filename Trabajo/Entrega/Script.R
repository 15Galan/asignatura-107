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

  # Calcula la desviacion tipica.
  # * datos:  vector con los valores para el calculo
  dt <- function(datos) {
    sqrt(mean(datos^2) - mean(datos)^2)
  }
  
  
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
  datosDesviados <- c(peso_DT     = dt(datos$peso),
                      altura_DT   = dt(datos$altura),
                      edad_DT     = dt(datos$edad),
                      tabaco_DT   = dt(datos$tabaco),
                      ubes_DT     = dt(datos$ubes),
                      carne_DT    = dt(datos$carneRoja),
                      verduras_DT = dt(datos$verduras),
                      deporte_DT  = dt(datos$deporte),
                      droga_DT    = dt(datos$drogas),
                      IMC_DT      = dt(datos$IMC))
  
  
  
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
  
  
  
# APARTADO 07 -----------------------------------------------------------------
  
  # Separa un conjunto de datos en 3 subconjuntos disjuntos: Entrenamiento,
  # Test y Validacion; que se dividiran segun las proporciones indicadas.
  # * datos:  conjunto de datos del que obtener los subconjuntos
  # * p1:     porcentaje del subconjunto de Entrenamiento
  # * p2:     porcentaje que divide a '1-p1'
  obtenerConjuntos <- function(datos, p1, p2) {
    rdatos  <- 1:nrow(datos)
    rTrain  <- sample(rdatos, p1 * length(rdatos))
    rTemp   <- setdiff(rdatos, rTrain)
    rTest   <- sample(rTemp, p2 * length(rTemp))
    rValid  <- setdiff(rTemp, rTest)
    
    list(entrenamiento=datos[rTrain,], test=datos[rTest,], validacion=datos[rValid,])  
  }
  
  
  # Conjuntos obtenidos
  conjuntos <- obtenerConjuntos(datos, .6, .5)  # 0.6 : Entrenamiento
                                                # 0.5 : el resto
    # Los 3 subconjuntos son disjuntos          #   0.4 * 0.5 = 0.2 : Test
                                                #   0.4 * 0.5 = 0.2 : Validacion
                                                #
                                                # 0.6 + 0.2 + 0.2 = 1
  
  
  
# APARTADO 08 -----------------------------------------------------------------
  
  # Declaracion de Funciones --------------------------------------------------
    
    # Realiza el ajuste lineal sobre un conjunto de datos.
    # * datos:  conjunto de datos
    # * y:      variable a explicar
    # * x:      variable explicatoria
    ajusteLineal <- function(df, y, x) {
      lm(str_c(y, "~", x), df)
    }
    
  
    # Calcula el coeficiente de determinacion (R²) a partir de un modelo.
    # * datos:  conjunto de datos del modelo
    # * modelo: modelo
    # * y:      variable a explicar
    calcularR2 <- function(df, mod, y) {
      MSE  <- mean((df[[y]] - predict.lm(mod, df)) ^ 2)
      varY <- mean(df[[y]] ^ 2) - mean(df[[y]]) ^ 2
      
      R2   <- 1 - MSE / varY
      R2a  <- 1 - (1- R2) * (nrow(df) - 1) / (nrow(df) - mod$rank)
      
      
      tibble(MSE=MSE, varY=varY, R2=R2, R2a=R2a)
    }
    
    
    # Calcula un modelo lineal a partir de un conjunto de entrenamiento, y el
    # coeficiente de determinación (R²) a partir de un conjunto de test.
    # * dfTrain:  conjunto de entrenamiento
    # * dfTest:   conjunto de test
    # * y:        variable a explicar
    # * x:        variable explicatoria
    calcularR2modelo <- function(dfTrain, dfTest, y, x) {
      modelo        <- ajusteLineal(dfTrain, y, x)
      coeficientes  <- calcularR2(dfTest, modelo, y)
      
      
      coeficientes$R2a
    }
    
    
  # Calculos ------------------------------------------------------------------
  
    # Variables predictoras
    predictoras <- names(datos[-length(datos)])   # Todas menos 'IMC' (la ultima)
    
    
    # Calcular los 14 R² de los 14 modelos lineales unidimensionales
    r2a <- predictoras %>% map_dbl(calcularR2modelo, dfTrain=conjuntos$entrenamiento, dfTest=conjuntos$test, y="IMC")
    
      # Esto permitira escoger el mejor modelo eliminando fluctuaciones estadisticas
    
    
    # Obtener la mejor variable para una prediccion unidimensional
    mejorVariable <- predictoras[which.max(r2a)]
    
    
    # Obtener el mejor modelo usando el conjunto de entrenamiento
    mejorModelo <- ajusteLineal(conjuntos$entrenamiento, "IMC", mejorVariable)
    
      # Obtener su R² con el conjunto de test
      calcularR2(conjuntos$test, mejorModelo, "IMC")
    
      # Obtener su R² con el conjunto de validacion
      calcularR2(conjuntos$valid, mejorModelo, "IMC")
      

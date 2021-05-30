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
  datos <- mutate(datos, IMC = peso / altura^2)
  
  
  
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
  
  
  # Quitar variables cualitativas (factores) de los datos
  datosN <- Filter(is.numeric, datos)
  
  
  # Medias
  medias <- colMeans(datosN)

  # Desviaciones tipicas
  desviaciones <- map_dbl(datosN, dt)
  
  
  
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
  plot(datos$edad, datos$IMC, col=4)
  abline(mod02)
  
  plot(datos$tabaco, datos$IMC, col=4)
  abline(mod03)
  
  plot(datos$ubes, datos$IMC, col=4)
  abline(mod04)
  
  plot(datos$carneRoja, datos$IMC, col=4)
  abline(mod05)
  
  plot(datos$verduras, datos$IMC, col=4)
  abline(mod06)
  
  plot(datos$deporte, datos$IMC, col=4)
  abline(mod07)
  
  plot(datos$drogas, datos$IMC, col=4)
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
  
  # Funciones -----------------------------------------------------------------
    
    # Realiza el ajuste lineal sobre un conjunto de datos.
    # * datos:  conjunto de datos
    # * y:      variable a explicar
    # * x:      variable explicatoria
    ajusteLineal <- function(df, y, x) {
      
      # lm(str_c(y, "~", x), df)  # Deprecado: Apartado 09
      
      lm(str_c(y, "~", str_c(x, collapse="+")), datos)
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
    
    # Subconjuntos
    entrenamiento <- conjuntos$entrenamiento
    test          <- conjuntos$test
    validacion    <- conjuntos$validacion
    
    # Calcular los 14 R² de los 14 modelos lineales unidimensionales
    r2a <- predictoras %>% map_dbl(calcularR2modelo, dfTrain=entrenamiento, dfTest=test, y="IMC")
    
      # Esto permitira escoger el mejor modelo eliminando fluctuaciones estadisticas
    
    
    # Obtener la mejor variable para una prediccion unidimensional
    mejorVariable <- predictoras[which.max(r2a)]
    
    
    # Obtener el mejor modelo usando el conjunto de entrenamiento
    mejorModelo <- ajusteLineal(entrenamiento, "IMC", mejorVariable)
    
      # Obtener su R² con el conjunto de test
      calcularR2(test, mejorModelo, "IMC")
    
      # Obtener su R² con el conjunto de validacion
      calcularR2(validacion, mejorModelo, "IMC")
      
      
      
# APARTADO 09 -----------------------------------------------------------------

  # Funciones -----------------------------------------------------------------
  
    # La funcion 'ajusteLineal()' del apartado 08 se ha modificado para
    # que pueda reutilizarse en este apartado (ahora es compatible con ambos)
    
    
    # Encuentra el mejor ajuste lineal usando un conjunto de variables y
    # calculando sus R² ajustados con conjuntos de Entrenamiento y de Test.
    # * dfTrain:      conjunto de entrenamiento
    # * dfTest:       conjunto de test
    # * predictoras:  variables explicatorias del modelo
    obtenerMejorAjusteLineal <- function(dfTrain, dfTest, predictoras) {
      
      # Variables iniciales
      mejoresVariables  <- character(0)
      R2a               <- 0
      
      # Bucle del algoritmo
      repeat {
        # Inicializar
        R2as      <- map_dbl(predictoras, ~calcularR2modelo(dfTrain, dfTest, "IMC", c(mejoresVariables, .)))
        i         <- which.max(R2as)
        mejorR2a  <- R2as[i]
        
        if (mejorR2a <= R2a) {
          break
        }
        
        # Mostrar por la consola los valores calculados durante la ejecucion
        # cat(sprintf("%1.4f %s\n", mejorR2a, predictoras[i]))
        
        # Actualizar los valores para la siguiente iteracion
        R2a               <- mejorR2a
        mejoresVariables  <- c(mejoresVariables, predictoras[i])
        predictoras       <- predictoras[-i]
      }
      
      modelo <- ajusteLineal(dfTrain, "IMC", mejoresVariables)
      
      
      list(variables=mejoresVariables, modelo=modelo)
    }
    
    
  # Calculos ------------------------------------------------------------------
  
    # Unidimensional
    predictorasSimple <- names(datos[-length(datos)])   # Todas menos 'IMC' (la ultima)
    mejorModeloSimple <- obtenerMejorAjusteLineal(conjuntos$entrenamiento, conjuntos$test, predictorasSimple)$modelo
      
    
    # Multidimensional
    predictorasCombo <- crossing(var1=predictoras, var2=predictoras) %>% pmap_chr(str_c, sep=":")
    mejorModeloCombo <- obtenerMejorAjusteLineal(conjuntos$entrenamiento, conjuntos$test, predictorasCombo)$modelo
      
      
      
# APARTADO 10 -----------------------------------------------------------------
  
  # Modelo unidimensional
  evaluacionSimple <- calcularR2(conjuntos$validacion, mejorModeloSimple, "IMC")
  
  # Modelo multidimensional
  evaluacionCompleja <- calcularR2(conjuntos$validacion, mejorModeloComplejo, "IMC")

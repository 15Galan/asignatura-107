# ==============================================================================
#
# Trabajo elaborado por:
#   * Antonio J. Galán Herrera
#
# ==============================================================================


# Importacion de paquetes necesarios
library("tidyverse")



# APARTADO 01 ==================================================================

  # Usando la funcion 'read_csv' del paquete 'readr'
  # se exporta el fichero con formato 'tibble'.
  datos <- read_csv("../Recursos/12306.csv", col_types=cols(sexo=col_factor(),
                                                            dietaEsp=col_factor(),
                                                            nivEstPad=col_factor(),
                                                            nivEstudios=col_factor(),
                                                            nivIngresos=col_factor()))



# APARTADO 02 ==================================================================

  # Se crea la columna 'IMC' segun el enunciado
  datos <- mutate(datos, IMC = peso / altura^2)
  
  
  
# APARTADO 03 ==================================================================

  # Se limpian las columnas con algun NA de los datos,
  # usando la funcion 'na.exclude()' o 'na.omit()'
  datos <- na.exclude(datos)

    # Esta nueva variable 'datos' es de un tamaño menor,
    # ya que no cuenta con algunas de sus filas anteriores
  
  
  
# APARTADO 04 ==================================================================

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
  
  
  
# APARTADO 05 ==================================================================

  # Realiza el ajuste lineal sobre un conjunto de datos.
  # * datos:  conjunto de datos
  # * y:      variable a explicar
  # * x:      variable explicatoria
  ajusteLineal <- function(datos, y, x) {
    
    # lm(str_c(y, "~", x), df)  # Deprecado: Apartado 09
    
    lm(str_c(y, "~", str_c(x, collapse="+")), datos)
  }
  
  
  # Variables para los modelos
  variables <- names(datos[3:14]) # Todas excepto "altura", "peso" e "IMC"
  
  
  # Modelos
  modelos <- variables %>% map(ajusteLineal, datos=datos, y="IMC")
  
  # Coeficientes de regresion y determinacion de los modelos
  regresion     <- modelos %>% map(coefficients)
  determinacion <- modelos %>% map(summary) %>% map("r.squared")
  
  
    
# APARTADO 06 ==================================================================
  
  # Igual que 'ajusteLineal()', pero devuelve una lista con los valores
  # 'x' e 'y' del modelo, ademas del propio modelo y los datos.
  # * datos:  conjunto de datos
  # * y:      variable a explicar
  # * x:      variable explicatoria
  ajusteLinealLista <- function(datos, y, x) {
    list(x=x, y=y, modelo=lm(str_c(y, "~", str_c(x, collapse="+")), datos))
  }
  
  # Genera los graficos de dispersion junto a la recta de regresion
  # * datos:  conjunto de datos del que extraer el grafico
  # * modelo: modelo del que extraer la recta de regresion
  dibujarModelos <- function(datos, modelo) {
    
    # Crear el fichero de la imagen (la carpeta debe existir)
    jpeg(str_c("Graficos/", modelo$x, ".jpeg"))
    
    # Valores numericos
    x <- datos[[modelo$x]]
    y <- datos[[modelo$y]]
    
    # Nombres
    ejeX <- modelo$x
    ejeY <- modelo$y
    
    # Representar los graficos
    plot(x, y, xlab=ejeX, ylab=ejeY, col=4)
    
    # Añadir la recta de regresion solo si es una variable cuantitativa
    if (is.numeric(datos[[modelo$x]])) {
      abline(modelo$modelo, col=1)
    }
    
    # Terminar de escribir en el fichero
    dev.off()
  }
  
  
  # Generar los modelos con los valores necesarios para las graficas
  modelos <- variables %>% map(ajusteLinealLista, datos=datos, y="IMC")
  
  # Dibujar todos los modelos
  modelos %>% walk(~dibujarModelos(datos, .))
  
  
  
# APARTADO 07 ==================================================================
  
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
  
  
  
# APARTADO 08 ==================================================================
  
  # Funciones ------------------------------------------------------------------
  
    # Calcula los valores: MSE, varY, R2 y R² a partir de un modelo.
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
    
    
    # Calcula el coeficiente de determinación (R²) ajustado a partir
    # de un conjunto de entrenamiento y un conjunto de test.
    # * dfTrain:  conjunto de entrenamiento
    # * dfTest:   conjunto de test
    # * y:        variable explicada
    # * x:        variable explicatoria
    calcularR2ajustado  <- function(dfTrain, dfTest, y, x) {
      modelo            <- ajusteLineal(dfTrain, y, x)
      coeficientes      <- calcularR2(dfTest, modelo, y)
      
      
      coeficientes$R2a
    }
    
    
  # Calculos -------------------------------------------------------------------
  
    # Variables predictoras
    predictoras <- names(datos[-length(datos)])   # Todas menos 'IMC' (la ultima)
    
    
    # Calcular los 14 R² de los 14 modelos lineales unidimensionales
    r2a <- predictoras %>% map_dbl(calcularR2ajustado, dfTrain=conjuntos$entrenamiento, dfTest=conjuntos$test, y="IMC")
    
      # Esto permitira escoger el mejor modelo eliminando fluctuaciones estadisticas
    
    
    # Obtener la mejor variable para una prediccion unidimensional
    mejorVariable <- predictoras[which.max(r2a)]
    
    
    # Obtener el mejor modelo usando el conjunto de entrenamiento
    mejorModelo <- ajusteLineal(conjuntos$entrenamiento, "IMC", mejorVariable)
    
      # Obtener su R² con el conjunto de test
      evaluacionTest <- calcularR2(conjuntos$test, mejorModelo, "IMC")
    
      # Obtener su R² con el conjunto de validacion
      evaluacionValidacion <- calcularR2(conjuntos$validacion, mejorModelo, "IMC")
      
      
      
# APARTADO 09 ==================================================================

  # Funciones ------------------------------------------------------------------
  
    # La funcion 'ajusteLineal()' del apartado 05 se ha modificado para
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
        R2as      <- map_dbl(predictoras, ~calcularR2ajustado(dfTrain, dfTest, "IMC", c(mejoresVariables, .)))
        i         <- which.max(R2as)
        mejorR2a  <- R2as[i]
        
        if (mejorR2a <= R2a) {
          break
        }
        
        # Mostrar por la consola los valores calculados durante la ejecucion
        cat(sprintf("%1.8f %s\n", mejorR2a, predictoras[i]))
        
        # Actualizar los valores para la siguiente iteracion
        R2a               <- mejorR2a
        mejoresVariables  <- c(mejoresVariables, predictoras[i])
        predictoras       <- predictoras[-i]
      }
      
      modelo <- ajusteLineal(dfTrain, "IMC", mejoresVariables)
      
      
      list(variables=mejoresVariables, modelo=modelo)
    }
    
    
  # Calculos -------------------------------------------------------------------
  
    predictoras <- names(datos[-length(datos)])   # Todas menos 'IMC' (la ultima)
    mejorModelo <- obtenerMejorAjusteLineal(conjuntos$entrenamiento, conjuntos$test, predictoras)$modelo
    
    
    
# APARTADO 10 ==================================================================
  
  evaluacion <- calcularR2(conjuntos$validacion, mejorModelo, "IMC")
  

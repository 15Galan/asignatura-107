library(tidyverse)



# EJERCICIO 06 =================================================================

# Datos del enunciado: tabla
x <- c(2,3,4.5,5.5,6.5,8.5,10)
n <- c(5,7,5,6,4,7,6)

tabla <- table(n,x)
plot <- plot(x,n)


# Apartado (b) -----------------------------------------------------------------
a <- c(2,2,1,1,1,3,3)  # Ancho de las barras
h <- n                 # Alto de las barras

# Areas
total  <- sum(a*h)
pedida <- (a[2]*h[2])/2 + sum(a[3:4]*h[3:4])  # Entre 'x=3' y 'x=6'

# Porcentaje
porcentaje <- pedida*100 / total


# Apartado (c) -----------------------------------------------------------------

# No hay intervalo modal, ya que hay 2 intervalos con el maximo 'ni'



# EJERCICIO 07 =================================================================

# Datos del enunciado: histograma
N <- 850               # Muestra
x <- c(2.5,7.5,15,30)  # Marcas de clase
a <- c(5,5,10,20)      # Ancho de las barras
h <- c(10,30,50,20)    # Alto de las barras


# Apartado (a) -----------------------------------------------------------------
k <- N/sum(a*h)
n <- a * h * k

plot(x)
plot(n)

# Apartado (b) -----------------------------------------------------------------

# Definicion del momento central como funcion,
# siendo 'x' los datos y 'r' el orden
momentoC <- function(x, r) sum((x - mean(x))^r) / length(x)

# Aplicacion de la funcion
momentoC(x,1)
momentoC(x,2)
momentoC(x,3)
momentoC(x,4)


# Apartado (c) -----------------------------------------------------------------
g1 <- momentoC(x,3) / sqrt(momentoC(x,2))^3
g2 <- momentoC(x,4) / momentoC(x,2)^2



# EJERCICIO 12 =================================================================

# Datos del enunciado: valores
x <- c(2,4,6,8)


# Apartado (a) -----------------------------------------------------------------
media    <- mean(x)
varianza <- var(x)


# Apartado (b) -----------------------------------------------------------------

# Tipificacion
z <- (x - media)/sqrt(varianza)

media_z    <- mean(z)
varianza_z <- var(z)


# Apartado (c) -----------------------------------------------------------------



# EJERCICIO 17 =================================================================

# Datos del enunciado: notas
notas <- c(7.5, 3.0, 5.5)
pesos <- c(1,2,2)


# Apartado (a) -----------------------------------------------------------------
nota3 <- (5*sum(pesos) - (notas[1]*pesos[1] + notas[2]*pesos[2])) / 2


# Apartado (b) -----------------------------------------------------------------

# Pesos actualizados
pesos2 <- c(2,1,1)

notaFinal <- function(n,p) sum(n*p)/sum(p)
notaFinal(notas,pesos2)

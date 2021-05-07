library(tidyverse)



# EJERCICIO 03 =================================================================

# Datos del enunciado: tabla
X  <- c(1,2,3,4,5)
Y1 <- c(4,2,3,2,4)
Y2 <- c(1,3,5,7,9)


# Coeficientes de correlacion
cor(X,Y1)
cor(X,Y2)



# EJERCICIO 04 =================================================================

# Datos del enunciado: tabla
densidad  <- c(43,55,40,52,39,33,50,33,44,21)
velocidad <- c(27.0,23.8,30.7,24.0,34.8,41.4,27.0,40.4,31.7,51.2)


# Apartado (a) -----------------------------------------------------------------
plot(densidad, velocidad)


# Apartado (b) -----------------------------------------------------------------


# Apartado (c) -----------------------------------------------------------------
cor(densidad,velocidad)


# Apartado (d) -----------------------------------------------------------------



# EJERCICIO 08 =================================================================

# Datos del enunciado: tabla
X <- c(1,2,3,4,5)
Y <- c(3.0,4.5,7.0,10.0,15.0)


# Ajuste
ajuste <- nls(y ~ a*b^x)

# Representacion
plot(X,Y);abline(ajuste)



# EJERCICIO 09 =================================================================

# Datos del enunciado: tabla
X <- c(1,2,3,4,5)
Y <- c(0.5,2.0,4.5,8.0,12.5)


# Ajuste
ajuste <- nls(y ~ a*x^b)

# Representacion
plot(X,Y);abline(ajuste)



# EJERCICIO 10 =================================================================

# Datos del enunciado: tabla
X <- c(1,2,3,4,5)
Y <- c(1.00,0.50,0.33,0.25,0.20)


# Ajuste
ajuste <- nls(y ~ 1/(a+b^x))

# Representacion
plot(X,Y);abline(ajuste)



# EJERCICIO 11 =================================================================

# Datos del enunciado: puntos
x <- c(1,2,3,4,5)
y <- c(1,1,2,4,8)


# Apartados (a)(b) -------------------------------------------------------------

# Primer ajuste
ajuste_1 <- lm(y ~ x)
summary(ajuste_1)

# Segundo ajuste
y2 <- log(y)
ajuste_2 <- lm(y2 ~ x)
summary(ajuste_2)

# Ajustes resultantes
coeficientes_1 <- ajuste_1$coefficients
coeficientes_2 <- ajuste_2$coefficients

modelo_1 <- coeficientes_1[1] + coeficientes_1[2]*x
modelo_2 <- exp(coeficientes_2[1]) * exp(coeficientes_2[2])^x

# ECMs de ambos ajustes
ECM_1 <- mean((y-modelo_1)^2)
ECM_2 <- mean((y-modelo_2)^2)


# Apartado (c) -----------------------------------------------------------------


# Apartado (d) -----------------------------------------------------------------
p <- c(6,10)

prueba_1 <- coeficientes_1[1] + coeficientes_1[2]*p
prueba_2 <- exp(coeficientes_2[1]) * exp(coeficientes_2[2])^p



# EJERCICIO 12 =================================================================

# Datos del enunciado: puntos
X <- c(20,30,40,50)
Y <- c(0,1,2)

tabla <- matrix(c(2,0,0,1,3,2,1,3,2,2,0,0), nrow=4, byrow=T)
dimnames(tabla) <- list(X,Y)


# Apartado (a) -----------------------------------------------------------------
ajuste <- lm(y ~x)

plot(tabla)



# EJERCICIO 13 =================================================================

# Datos del enunciado: tabla
x <- c(2,4,6,8,10,12)
y <- c(10,19,29,40,48,56)


# Momentos
m11 <- mean(x*y)
m20 <- mean(x*x)

# Pendiente
pendiente <- m11/m20


# Modelo
modelo <- lm(y ~ x + 0)  # Usar '+ 0' indica que no se quiere término independiente

y2 <- modelo$coefficients[1]*x


# Representacion
plot(x,y, xlim=c(0,15), ylim=c(0,60))
abline(modelo, col=2)



# EJERCICIO 15 =================================================================

# Datos del enunciado: valores
v <- c(2,4,6,8)


# Apartado (a) -----------------------------------------------------------------
media    <- mean(v)
varianza <- var(v)


# Apartado (b) -----------------------------------------------------------------
# Caso 1
v1 <- c(2,4,6)

media_1    <- mean(v1)
varianza_1 <- var(v1)


# Caso 2
v2 <- c(2,4,5,6)

media_2    <- mean(v2)
varianza_2 <- var(v2)


# Caso 3
v3 <- c(2,4,6,9)

media_3    <- mean(v3)
varianza_3 <- var(v3)

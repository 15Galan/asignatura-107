library(tidyverse)



x = c(1,2,3,4,5)
y = c(2,4,6,8,9)

table(x,y) # Tabla de doble entrada

cov(x,y)   # Covarianza muestral (dividido por N-1)
cor(x,y)   # Coeficiente de correlación lineal de Pearson


### MODELOS
# Regresión lineal
reg1 <- lm(y ~ x)                # Regresión: y = a0 + a1*x
reg2 <- lm(y ~ x+I(x^2)+I(x^3))  # Regresión: y = a0 + a1*x + a2*x^2 + a3*x^3

# Regresión no-lineal
reg3 <- nls(y ~ a*exp(b*x))      # Regresión: y = a * e^bX
reg4 <- nls(y ~ a*b^x)           # Regresión: y = a * b^X
reg5 <- nls(y ~ a+b*x)           # Regresión: y = a + b * X


### DATOS DEL MODELO: Regresión y Correlación
reg = lm(y ~ x)
plot(x,y);abline(reg)  # Representa la nube de puntos y el modelo ajustado
summary(reg)           # Resumen datos del modelo
names(reg)             # Datos de la Regresión lineal almacenados en "reg"
reg$fitted.values      # Valores estimados de "y" por el modelo
reg$residuals          # Residuos estimados
coef(reg)              # Coeficientes del Modelo
resid(reg)             # Residuos del Modelo
fitted(reg)            # Valores ajustados por el modelo


# Fórmulas para definir R^2 y SSE
1 - var(resid(reg))/var(y)  # Coeficiente de determinación (R^2)
sum(resid(reg)^2)           # Suma de los cuadrados de los residuos (SSE)

#Ejercicio 1.
install.packages("MASS")
library("MASS")
install.packages("caret")
library("caret")
install.packages("STAT")
library("STAT")
install.packages("olsrr")
library("olsrr")
install.packages("kableExtra")
library("kableExtra")
library("knitr")
library("rmarkdown")

#Ejercicio 2. 
y_cuentas <- c(110,2,6,98,40,94,31,5,8,10)
x_distancia <- c(1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)

#Ejercicio 3. 
plot(x_distancia, y_cuentas)
cor(x_distancia, y_cuentas)
cor.test(x_distancia, y_cuentas)

#Ejercicio 4.
hist(x_distancia)
hist(y_cuentas)
#Test de normalidad
shapiro.test(x_distancia)
shapiro.test(y_cuentas)

#Ejercicio 5. 
xy <- x_distancia * y_cuentas
xy

#Ejercicio 6. 
x_cuadrado <- x_distancia^2
x_cuadrado

#Ejercicio 7.
tabla_datos <-data.frame(y_cuentas, x_distancia, xy, x_cuadrado)
tabla_datos

#Ejercicio 8.
kbl(tabla_datos)
row_spec(nrow(sumatorio), bold = TRUE, color = "black", background = "white")

#Ejericio 9.
sumatorio <- y_cuentas + x_distancia + xy + x_cuadrado
sumatorio

#Ejercicio 10.
sum_columnas <- rbind(tabla_datos, sumatorio)

#Ejercicio 11.
sumatorio_def <- lm(y_cuentas ~ x_distancia, data = tabla_datos)
suma_ej11 <- summary(sumatorio_def)
suma_ej11

#Ejercicio 12. 
plot(x_distancia, y_cuentas)
regresion <- lm(x_distancia ~ y_cuentas, data = tabla_datos)
summary(regresion)
abline(regresion)

#Ejercicio 13. 
#Calcular los residuos
ej13 <- lm(y_cuentas ~ x_distancia, data = tabla_datos)
ej13
residuos <- residuals(ej13)
residuos
#Para calcular los residuos estandarizados.
modelo <- lm(y_cuentas ~ x_distancia, data = tabla_datos) 
modelo
residuos_estandarizados <- rstandard(modelo)
residuos_estandarizados
#Para calcular los residuos estudentizados.
modelo <- lm(y_cuentas ~ x_distancia, data = tabla_datos) 
residuos_estudentizados <- rstudent(modelo)
residuos_estudentizados

#Ejercicio. 14

#Ejercicio. 15
entrenamiento <- rnorm(10)
entrenamiento
validacion <- rnorm (10)
validacion

#Ejercicio. 16
#Validación 
data <- data.frame(y_cuentas, x_distancia)
train_sample <- data %>% 
  createDataPartition(p = .8, list = FALSE)
entrenamiento <- data[-train_sample]
test <- data[train_sample]
length(entrenamiento)
#Es erróneo

library(dplyr)
train <- data %>% dplyr::sample_frac(.8)
test <- dplyr::anti_join(data, train)
test

#Ejercicio. 17
cof_regresion <-function(y_cuentas, x_cuadrado)
  cof_regresion
co_regr <- lm(y_cuentas ~ x_cuadrado)
summary(co_regr)

#Ejercicio 18
t.test(y_cuentas, x_cuadrado)$df

#Ejercicio 19. 
pred <- predict(co_regr)
media <- mean(xy)
SSR <- sum((pred - media)^2)
SSE <- sum((xy - pred)^2)
SST <- SSR + SSE
R2 <- SSR / SST
list(SSR = SSR, SSE = SSE, SST = SST, R2 = R2)

#Ejercicio 20. 
val_cruz <- predict(train, data = test)

#Ejercicio 21. 
observaciones <- influence.measures(sumatorio_def)
observaciones$cook
observaciones$hat
observaciones$studentized.residuals

#Ejercicio 22. 
residuos <- residuals(sumatorio_def)
plot(sumatorio_def$fitted.values, residuos, xlab = "Valores ajustados", ylab = "Residuos") 

#Ejercicio 23. 
errores_continuos <- rstandard(sumatorio_def)
plot(sumatorio_def$fitted.values, errores_continuos, xlab = "Valoresajustados", ylab = "Residuos estandarizados")
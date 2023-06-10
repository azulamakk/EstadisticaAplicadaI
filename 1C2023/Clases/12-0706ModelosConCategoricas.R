#Modelos con categóricas - Script de clase

#Seguimos trabajando con mtcars
#Vamos a sacar las variables hp, wt, cyl, vs y am como explicativas

#REFERENCIAS: 
#[, 1]	mpg	Miles/(US) gallon
#[, 2]	cyl	Number of cylinders
#[, 4]	hp	Gross horsepower
#[, 6]	wt	Weight (1000 lbs)
#[, 8]	vs	Engine (0 = V-shaped, 1 = straight)
#[, 9]	am	Transmission (0 = automatic, 1 = manual)

#Seguimos tomando mpg como variable explicada (y)
datos <- mtcars[,c(1,2,4,6,8,9)]

#declaramos las categóricas como factor
datos$cyl <- as.factor(datos$cyl)
datos$am <- as.factor(datos$am)
datos$vs <- as.factor(datos$vs)

attach(datos) #esto nos permite trabajar sin referir con al dataset

#Modelo 1 - Una categórica con dos grupos
mod1 <- lm(mpg ~ am)
summary(mod1) 

#Modelo 2 - Una categórica con más de dos grupos
mod2 <- lm(mpg ~ cyl)
summary(mod2)

#Modelo 3 - Dos categóricas de dos grupos cada una, sin interacción
mod3 <- lm(mpg ~ am + vs)
summary(mod3)
  
#Modelo 4 - Dos categóricas de dos grupos cada una, con interacción
mod4 <- lm(mpg ~ am*vs)
summary(mod4)
  
#Modelo 5 - Interacción de alguna categórica con alguna cuantitativa
mod5 <- lm(mpg ~ wt*am)
summary(mod5)

#---------------------------------------------------------
#Ejemplo de uso del objeto lm
modelo <- lm(mpg ~ wt + hp + cyl)

#Matriz de diseño usada por el modelo
X <- model.matrix(modelo)

#Matriz de correlaciones entre las X 
#Hay que sacar el intercept
cor(X[,-1])

#Vector de betas
modelo$coefficients

#Valores predichos y residuos
modelo$fitted.values
modelo$residuals

#Residuos estandarizados
rstudent(modelo)

#Leverage
hatvalues(modelo)

#Distancia Cook
cooks.distance(modelo)

#Algunos graficos de diagnóstico incluidos en el objeto lm
plot(modelo) 
#Hay que apretar alt+enter para correr los gráficos








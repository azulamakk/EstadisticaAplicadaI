setwd()
data <- read.csv('melbourne properties.csv')



set.seed(12)
u<-x<=1000
sum(u)/400

mean(x<=1000)

# Que calcule la empirica para un x cualquiera que pide el usuario
{
funcion_empirica <- function(x, datos)
  n <- length(datos)
  booleano <- datos <= x
  frec_abs <- sum(booleano)
  return(frec_abs/n)
}

funcion_empirica(1000,x)

x <- sort(x) #ordeno los datos voy probando en forma creciente 
# las posiciones hasta encontrar la mediana

funcion_empirica(sort(x)[200], x)
x[200]

quantile(x,0.5)


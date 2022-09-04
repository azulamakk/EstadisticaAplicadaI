
setwd("~/Desktop/Estadistica Aplicada I/TP Grupo 5/Entrega2")
data=read.csv('DS10.csv')

#Muestreo de datos - Realizado por grupo 10
data=data$prices.amountMax
x=as.numeric(data)
set.seed(66)
y=sample(x,300)

precioOrdyMuestr = sort(y)

n=1:400

#Histograma datos originales
hist(precioOrdyMuestr, col=terrain.colors(8), breaks = 8,
     main='Histograma datos originales', xlab='Precio', ylab = 'Frecuencia')

#Grafico de tendencia de la variable precio ya muestreada (esto lo hicimos para
#Asegurarnos de que no haya tendencias y ver si existían outliers significativos)
plot(y, type = 'l', col=terrain.colors(8), xlab='x', ylab='Precio', main='Grafico de tendencias')

#Generamos funcion empirica 
empirica <- function(x, a) {
  return (sum(x<=a)/length(x))
}
fEmpirica = c()
for(i in 1: length(precioOrdyMuestr)){
 fEmpirica[i] <- empirica(precioOrdyMuestr, precioOrdyMuestr[i])
}

#Grafico de la funcion empirica

plot(precioOrdyMuestr , fEmpirica, main='Funcion Empirica', 
     xlab = 'Precio', ylab = 'Probabilidad Acumulada',
     col = 'dark green')

#Esperanza
esperanza=mean(precioOrdyMuestr)
esperanza

#Varianza
varianza=mean((precioOrdyMuestr-esperanza)^2)
varianza
var(precioOrdyMuestr)

#Desvio estandard
desvioest=sqrt(varianza)
desvioest
sd(precioOrdyMuestr)

#Coef de asimetria
coefdeasimetria=sum(((precioOrdyMuestr-esperanza)/desvioest)^3)/400
coefdeasimetria

#Coef de curtosis
coefdecurtosis= sum(((precioOrdyMuestr-esperanza)/desvioest)^4)/400
coefdecurtosis

#Cuantiles

q0.25=quantile(precioOrdyMuestr,0.25,type=1)
q0.50=quantile(precioOrdyMuestr,0.5,type=1)
q0.75=quantile(precioOrdyMuestr,0.75,type=1)

q0.25
q0.50
q0.75

normalizado=rnorm(y)
plot(normalizado,type="l")

#Estandarizacion de los datos
Qm <- c()
Qm <- (precioOrd-mean(y)/sd(y))
Qm

#Histograma de datos estandarizados
hist(Qm)


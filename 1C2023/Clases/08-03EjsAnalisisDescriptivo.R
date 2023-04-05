data <- read.csv("~/Desktop/Estadistica aplicada/1C2023/Clases/data_credit_card.csv")

#Tomamos la variable credit card limit
x <- data$purchases
x

# Rango y funcion empirica
min(x)
max(x)
# [0, 27790.42]

# Manera 1
plot(ecdf(x))

# Manera 2
empirica <- function(x, a) {
  return (sum(x<=a)/length(x))
}
fEmpirica = c()
for(i in 1: length(x)){
  fEmpirica[i] <- empirica(x, x[i])
}

# Grafico funcion empirica
plot(x , fEmpirica, main='Funcion Empirica', 
     xlab = 'Purchase Amount', ylab = 'Probabilidad Acumulada',
     col = 'dark green')

# Histograma 
hist(x, main='Histograma purchases', col = 'dark green')

# Grafico densidad --- Solo funcionaria para continuas
plot(density(x), lwd=4, col='darkgreen', main='Grafico de densidad')

# Media y mediana
mean(x)
quantile(x, 0.5)
# La media es 932,1472 y la mediana 332,435
# Al haber una diferencia significativa entre ambas, podemos inferir que la muestra
# No posee normalidad, y es probable que la misma presente significativos outliers.

# Cuantil 0.25 y 0.75 
q25 <- quantile(x, 0.25)
q25
q75 <- quantile(x, 0.75)
q75
# [37,39, 1081,495]
boxplot(x, horizontal = TRUE)

#Esperanza
esperanza <- mean(x)

#Varianza
varianza=mean((x-esperanza)^2)
varianza
varianza^0.5

#Desvio estandard
desvioest=sqrt(varianza)
desvioest
desvioPref <- sqrt(mean((x-mean(x))**2))
sd(x)

# Coef variacion
coefVar <- desvioPref /mean(x)

#Coef de asimetria
coefdeasimetria=sum(((x-esperanza)/desvioest)^3)
coefdeasimetria

#Coef de curtosis
coefdecurtosis= sum(((x-esperanza)/desvioest)^4)/length(x)
coefdecurtosis

#Outlier count
RIC = q75-q25
RIC
RIC_superior <- q75 + 1.5 * RIC
RIC_superior
outlier <- x>RIC_max
cantOutliers=0
for(i in 1: length(outlier)){
  if(outlier[i]==TRUE){
    cantOutliers=cantOutliers+1
  }
}
cantOutliers
# Es observable un total de 73 outliers, lo que deja en 
#evidencia una presencia significativa

#Scatterplots
sorted <- sort(x)
plot(sorted, col='pink3', main = 'Scatterplot')

#Beeswarm
beeswarm::beeswarm(data$credit_limit,corral='wrap', corralWidth=1,pch=20)

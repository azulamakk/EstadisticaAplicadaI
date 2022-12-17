

setwd("~/Desktop/Estadistica Aplicada I/TP Grupo 5/Entrega3")
data=na.omit(read.csv("DS3.csv"))
View(data)

set.seed(200)
x=data$culmen_length_mm[data$species=="Adelie" & data$sex=="MALE"]
y=data$culmen_length_mm[data$species=="Adelie" & data$sex=="FEMALE"]
x
y

#Funcion Empirica

funcion_empirica <- function(x,datos){
  n <- length(datos)
  booleano <- datos<=x
  frec_abs <- sum(booleano)
  return(frec_abs/n)
}
x=sort(x)
v1=vector()
for(i in seq(x)){
  
  v1=c(v1,funcion_empirica(x[i],x))
}
print(v1)

plot(x,v1,xlab="Longitud del culmen de un pingüino Adelie macho del archipiélago Palmer (Antártida)
tomado al azar (mm)", ylab="Probabilidad", main="Gráfico de la función empírica de x",col="blue")
plot(ecdf(x))

y=sort(y)
v2=vector()
for(i in seq(y)){
  
  v2=c(v2,funcion_empirica(y[i],y))
}
print(v2)

plot(y,v2,xlab="Longitud del culmen de un pingüino Adelie hembra del archipiélago Palmer (Antártida)
tomado al azar (mm)", ylab="Probabilidad", main="Gráfico de la función empírica de y",col="red")

#Media Muestral
x_raya=mean(x)
x_raya
y_raya=mean(y)
y_raya

#Varianza Muestral
v_xsombrero=mean((x-x_raya)^2)
v_ysombrero=mean((y-y_raya)^2)
v_xsombrero
v_ysombrero

#Coef. de asimetria muestral
gamax=mean(((x-x_raya)/(v_xsombrero)^.5)^3)
gamay=mean(((y-y_raya)/(v_ysombrero)^.5)^3)
gamax
gamay

#Coef. de curtosis
kx=mean(((x-x_raya)/(v_xsombrero)^.5)^4)
ky=gamay=mean(((y-y_raya)/(v_ysombrero)^.5)^4)
kx
ky

#cuantiles
quantile(x,.25,type=1)
quantile(x,.5,type=1)
quantile(x,.75,type=1)
quantile(y,.25,type=1)
quantile(y,.5,type=1)
quantile(y,.75,type=1)

#Datos estandarizados
((x-x_raya)/(v_xsombrero)^(0.5))
(y-y_raya)/(v_ysombrero)^(0.5)

#Histograma estandarizado
hist(((x-x_raya)/(v_xsombrero)^(0.5)),15,main="Histograma de la variable x estandarizada",xlab="Longitud del culmen de un pingüino Adelie macho del archipiélago Palmer (Antártida)
tomado al azar (mm)", ylab="Frecuencia",col="light blue")
hist(((y-y_raya)/(v_ysombrero)^(0.5)),main="Histograma de la variable y estandarizada",xlab="Longitud del culmen de un pingüino Adelie hembra del archipiélago Palmer (Antártida)
tomado al azar (mm)", ylab="Frecuencia",col="pink")

#Media Muestral
x_raya=mean(x)
y_raya=mean(y)

#Varianza Muestral
v_xsombrero=mean((x-x_raya)^2)
v_ysombrero=mean((y-y_raya)^2)
v_xsombrero
v_ysombrero

#Coef. de asimetria muestral
gamax=mean(((x-x_raya)/(v_xsombrero)^.5)^3)
gamay=mean(((y-y_raya)/(v_ysombrero)^.5)^3)
gamax
gamay

#Coef. de curtosis
kx=mean(((x-x_raya)/(v_xsombrero)^.5)^4)
ky=mean(((y-y_raya)/(v_ysombrero)^.5)^4)
kx
ky

#Coef. de Varicacion
CVX = sqrt(v_xsombrero)/x_raya
CVY = sqrt(v_ysombrero)/y_raya
CVX
CVY

#Mediana
medianaX = quantile(x, 0.5, type=1)
medianaX
medianaY = quantile(y, 0.5, type=1)
medianaY

#Desvio
DesVestX <- sqrt(v_xsombrero)
DesVestX

DesVestY <- sqrt(v_ysombrero)
DesVestY

#Analisis de la muestra, para obaervar distribuciones posibles para el método paramétrico

hist(x, col='Light blue', breaks = 10,
     main='Muestras ', xlab='Longitud del culmen de un pinguino adelie macho del archipielago Palmer (Antartida) tomado al azar en ml', ylab = 'Frecuencia')

hist(y, col='Light pink', breaks = 10,
     main='Muestras ', xlab='Longitud del culmen de un pinguino adelie hembra del archipielago Palmer (Antartida) tomado al azar en ml', ylab = 'Frecuencia')


#Calculo de las cuatro cantidades de interes elegidas por metodo no parametrico

#q1 = Estimar la diferencia entre las medias de las dos muestras poblacionales (m1-m2)
q1 = x_raya - y_raya
q1
#Error de q1 no paremetrico por bootstrap
d=c()
set.seed(200)
d<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  YBOOT=sample(y,73,replace=TRUE)
  mean(XBOOT)-mean(YBOOT)
})
d
errorq1_noparametrico <- sqrt(var(d))
errorq1_noparametrico

#q2 = Estimar el cociente entre las dos varianzas poblacionales (v1/v2)
q2 = v_xsombrero / v_ysombrero
q2
#Error de q2 no paremetrico por bootstrap
e=c()
set.seed(200)
e<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  YBOOT=sample(y,73,replace=TRUE)
  mean((XBOOT-mean(XBOOT))^2) / mean((YBOOT-mean(YBOOT))^2)
})
e
errorq2_noparametrico <- sqrt(var(e))
errorq2_noparametrico

#q3 = Estimar el cuantil 0.5 de la muestra poblacional X
q3x = quantile(x, 0.5, type=1)
q3x
#Error de q3 no paremetrico por bootstrap para X
f=c()
set.seed(200)
f<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  quantile(XBOOT, 0.5, type=1)
})
f
errorq3x_noparametrico <- sqrt(var(f))
errorq3x_noparametrico

#q3 = Estimar el cuantil 0.5 de la muestra poblacional Y
q3y = quantile(y, 0.5, type=1)
q3y
#Error de q3 no paremetrico por bootstrap para Y
r=c()
set.seed(200)
r<-replicate(1000,{
  YBOOT=sample(y,73,replace=TRUE)
  quantile(YBOOT, 0.5, type=1)
})
r
errorq3y_noparametrico <- sqrt(var(r))
errorq3y_noparametrico

#q4 = Estimar la P(Y>37.5) para la muestra poblacional X
nx = length(x)
q4x <- sum(x>37.5)/n
q4x
#Error de q4 no paremetrico por bootstrap para X
s=c()
set.seed(200)
s<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  sum(XBOOT>37.5)/n
})
s
errorq4x_noparametrico <- sqrt(var(s))
errorq4x_noparametrico

#q4 = Estimar la P(Y>37.5) para la muestra poblacional Y
ny = length(y)
q4y <- sum(y>37.5)/n
q4y
#Error de q4 no paremetrico por bootstrap para Y
g=c()
set.seed(200)
g<-replicate(1000,{
  YBOOT=sample(y,73,replace=TRUE)
  sum(YBOOT>37.5)/n
})
g
errorq4y_noparametrico <- sqrt(var(g))
errorq4y_noparametrico

plot(x, type="l")
plot(y, type="l")

#Se asume una distribucion normal


x_ordenado <- sort(x)
densidadesX <- dnorm(x_ordenado, x_raya, sqrt(v_xsombrero), log=FALSE)
plot(densidadesX, type='l')



#Calculo de las cantidades parametrizandolas asumiendo distribucion normal

#q1 parametrico y q2 paramétrico se calculan igual que con el método no paramétrico por lo que el codigo es el ya realizado anteriormente
# para el calculo de q3 y q4 asumiendo distribucion normal, en necesario estandarizar la muestra


normalestandarX=(x-x_raya)/DesVestX
normalestandarX
normalestandarY=(y-y_raya)/DesVestY
normalestandarY

#q3 parametrico
medianaparametrizada <- qnorm(0.5, log.p=FALSE)
medianaparametrizada
q3parametrizadaX=(medianaparametrizada*DesVestX)+x_raya
q3parametrizadaX

q3parametrizadaY=(medianaparametrizada*DesVestY)+y_raya
q3parametrizadaY

#Calculamos el error de q3 parametrico por bootstrap para la muestra x
h=c()
set.seed(200)
h<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  XBOOT_raya <- mean(XBOOT)
  XBOOT_sombrero <- sqrt(var(XBOOT))
  qnorm(0.5, log.p=FALSE)
  (medianaparametrizada*XBOOT_sombrero)+XBOOT_raya
})
h
errorq3_parametricoX <- sqrt(var(h))
errorq3_parametricoX

#Calculamos el error de q3 parametrico por bootstrap para la muestra y

s=c()
set.seed(200)
s<-replicate(1000,{
  YBOOT=sample(y,73,replace=TRUE)
  YBOOT_raya <- mean(YBOOT)
  YBOOT_sombrero <- sqrt(var(YBOOT))
  qnorm(0.5, log.p=FALSE)
  (medianaparametrizada*YBOOT_sombrero)+YBOOT_raya
})
s
errorq3_parametricoY <- sqrt(var(s))
errorq3_parametricoY

#q4
#para la muestra X
#conversion de 41 a normal estandar
Zx=(41-x_raya)/DesVestX
Zx
q4parametrizadax <- 1- pnorm(Zx,log.p=FALSE)
q4parametrizadax
#Calculamos el error de q4 parametrico por bootstrap
r=c()
set.seed(200)
r<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  XBOOT_raya <- mean(XBOOT)
  XBOOT_sombrero <- sqrt(var(XBOOT))
  z=(41-XBOOT_raya)/XBOOT_sombrero
  1 - pnorm(z, log.p=FALSE)
})
r
errorq4_parametrizadax <- sqrt(var(r))
errorq4_parametrizadax

#para la muestra Y
#conversion de 37.5 a normal estandar 
Zy=(37.5-y_raya)/DesVestY
Zy
q4parametrizaday <- 1- pnorm(Zy,log.p=FALSE)
q4parametrizaday
#Calculamos el error de q4 parametrico por bootstrap
j=c()
set.seed(200)
j<-replicate(1000,{
  YBOOT=sample(y,73,replace=TRUE)
  YBOOT_raya <- mean(YBOOT)
  YBOOT_sombrero <- sqrt(var(YBOOT))
  z=(37.5-YBOOT_raya)/YBOOT_sombrero
  1 - pnorm(z, log.p=FALSE)
})
j
errorq4_parametrizaday <- sqrt(var(j))
errorq4_parametrizaday

#Calculo de outliers y representatividad por el metodo de caja y bigotes

#En la muestra X
cuantil1x <- quantile(x,.25,type=1)
cuantil2x <- quantile(x,.5,type=1)
cuantil3x <- quantile(x,.75,type=1)
ricx = cuantil3x-cuantil1x
ricx

extremoInfx = cuantil1x - 1.5 * ricx
extremoInfx

extremoSupx = cuantil3x + 1.5 * ricx
extremoSupx

#Observamos el cambio en la varianza que no es un estimador para ver si los outliers en x son representativos

xmod <- sample(x[x>extremoInfx & x<extremoSupx])
xmod

# Ahora creamos la variable xmodOutliers que devuelve TRUE o FALSE 
# en funcion de si hay registros que son menores al extremo inferior o mayores al extremo superior
xmodOutliers <- length(xmod) < length(x)
xmodOutliers

# Calculamos media y varianza para muestra alterada
mean(xmod)
var_xmod <- var(xmod)
var_xmod

#en la muestra Y

cuantil1y <- quantile(y,.25,type=1)
cuantil2y <- quantile(y,.5,type=1)
cuantil3y <- quantile(y,.75,type=1)
ricy = cuantil3y-cuantil1y
ricy

extremoInfy = cuantil1y - 1.5 * ricy
extremoInfy

extremoSupy = cuantil3y + 1.5 * ricy
extremoSupy

# Ahora creamos la variable xmodOutliers que devuelve TRUE o FALSE 
# en funcion de si hay registros que son menores al extremo inferior o mayores al extremo superior
ymod <- sample(y[y>extremoInfy & y<extremoSupy])
ymod

# Ahora creamos la variable xmodOutliers que devuelve TRUE o FALSE 
# en funcion de si hay registros que son menores al extremo inferior o mayores al extremo superior
ymodOutliers <- length(ymod) < length(y)
ymodOutliers

# No hay outliers en la muestra Y.

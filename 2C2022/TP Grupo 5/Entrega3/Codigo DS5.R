#Carga de dataset y variables de grupo anterior
setwd("~/Desktop/Estadistica Aplicada I/TP Grupo 5/Entrega3")
data=na.omit(read.csv('DS3.csv'))
View(data)

set.seed(200)
x=data$culmen_length_mm[data$species=="Adelie" & data$sex=="MALE"]
y=data$culmen_length_mm[data$species=="Adelie" & data$sex=="FEMALE"]
x
y

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

#Aplicamos correcciones indicadas sobre codigo asignado
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

#Analisis de outliers
plot(x)
hist(x, col=terrain.colors(8), breaks = 8,
     main='Muestras ', xlab='Longitud del culmen de un pinguino adelie macho del archipielago Palmer (Antartida) tomado al azar en ml', ylab = 'Frecuencia')
plot(y)
hist(y, col=terrain.colors(8), breaks = 8,
     main='Muestras ', xlab='Longitud del culmen de un pinguino adelie hembra del archipielago Palmer (Antartida) tomado al azar en ml', ylab = 'Frecuencia')

#No hay outliers significativos en ambas muestras

#Calculo de las cuatro cantidades de interes elegidas por metodo no parametrico

#q1 = Estimar la diferencia entre las medias de las dos muestras poblacionales (m1-m2)
q1 = x_raya - y_raya
q1
#Error de q1 no paremetrico
set.seed(20)
d=c()
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
#Error de q2 no paremetrico
e=c()
e<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  YBOOT=sample(y,73,replace=TRUE)
  mean((XBOOT-mean(XBOOT))^2) / mean((YBOOT-mean(YBOOT))^2)
})
e
errorq2_noparametrico <- sqrt(var(e))
errorq2_noparametrico

#q3 = Estimar el cuantil 0.5 de la muestra poblacional X
q3 = quantile(x, 0.5, type=1)
q3
#Error de q3 no paremetrico
f=c()
f<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  quantile(XBOOT, 0.5, type=1)
})
f
errorq3_noparametrico <- sqrt(var(f))
errorq3_noparametrico

#q4 = Estimar la P(Y>37.5) para la muestra poblacional Y
n = length(y)
q4 <- sum(y>37.5)/n
q4

#Error de q4 no paremetrico
g=c()
g<-replicate(1000,{
  YBOOT=sample(y,73,replace=TRUE)
  sum(YBOOT>37.5)/n
})
g
errorq4_noparametrico <- sqrt(var(g))
errorq4_noparametrico

plot(x, type="l")
plot(y, type="l")

#Se asume una distribucion normal
x_ordenado <- sort(x)
densidadesX <- dnorm(x_ordenado, x_raya, sqrt(v_xsombrero), log=FALSE)
plot(densidadesX, type='l')

plot(x)
plot(y)

#Calculo de las cantidades parametrizandolas asumiendo distribucion normal
#q1 parametrico
q1parametrizada <- x_raya - y_raya
q1parametrizada

#q2 parametrico
q2parametrizada <- v_xsombrero / v_ysombrero
q2parametrizada

#q3 parametrico
q3parametrizada <- qnorm(0.5, x_raya, sqrt(v_xsombrero), log.p=FALSE)
q3parametrizada
#Calculamos el error de q3 parametrico
h=c()
h<-replicate(1000,{
  XBOOT=sample(x,73,replace=TRUE)
  XBOOT_raya <- mean(XBOOT)
  XBOOT_sombrero <- sqrt(var(XBOOT))
  qnorm(0.5, XBOOT_raya, XBOOT_sombrero, log.p=FALSE)
})
h
errorq3_parametrico <- sqrt(var(h))
errorq3_parametrico

#q4
q4parametrizada <- 1- pnorm(37.5, y_raya, sqrt(v_ysombrero), log.p=FALSE)
q4parametrizada
#Calculamos el error de q4 parametrico
j=c()
j<-replicate(1000,{
  YBOOT=sample(y,73,replace=TRUE)
  YBOOT_raya <- mean(YBOOT)
  YBOOT_sombrero <- sqrt(var(YBOOT))
  1 - pnorm(37.5, YBOOT_raya, YBOOT_sombrero, log.p=FALSE)
})
j
errorq4_parametrizada <- sqrt(var(j))
errorq4_parametrizada

#Calculo de outliers y representatividad
cuantil1x <- quantile(x,.25,type=1)
cuantil2x <- quantile(x,.5,type=1)
cuantil3x <- quantile(x,.75,type=1)
ricx = cuantil3x-cuantil1x
ricx

extremoInfx = cuantil1x - 1.5 * ricx
extremoInfx

extremoSupx = cuantil3x + 1.5 * ricx
extremoSupx

xmod <- sample(x[x>extremoInfx & x<extremoSupx])
xmod
mean(xmod)
var_xmod <- var(xmod)
var_xmod

cuantil1y <- quantile(y,.25,type=1)
cuantil2y <- quantile(y,.5,type=1)
cuantil3y <- quantile(y,.75,type=1)
ricy = cuantil3y-cuantil1y
ricy

extremoInfy = cuantil1y - 1.5 * ricy
extremoInfy

extremoSupy = cuantil3y + 1.5 * ricy
extremoSupy

ymod <- sample(y[y<extremoInfy])
ymod <- sample(y[y>extremoInfy & y<extremoSupy])
ymod
mean(ymod)

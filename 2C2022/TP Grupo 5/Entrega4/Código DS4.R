setwd("~/Desktop/Estadistica Aplicada I/TP Grupo 5/Entrega4")

data <- read.csv("DS4.csv") 
E <- data$Primary.energy.consumption.per.capita..kWh.person.

#Histograma
hist(E,breaks = 40, main = "Histograma consumo electrico", xlab = "Consumo per capita", ylab = "Frecuencia", col = hcl.colors(20, palette = 'viridis'))

#Funcion empirica
sort(E)
ECDF = ecdf(E)
plot(ECDF,col="red",lwd=3,xlab="Consumo electrico",ylab="",main = "Funcion Empirica")
points(E,rep(0,228),col="blue",pch=20,cex=1)
segments(E,rep(0,228),E,sapply(E,ECDF),col="blue",lwd=1)

#Media muestral
x_raya = mean(E)

#Varianza muestral
v_sombrero = var(E)

#Desv estandar
sigma_sombrero = sqrt(v_sombrero)

#Coeficiente de variacion
coef_var = sigma_sombrero/x_raya

#Coeficiente de asimetria
coef_asim = sum(((E - x_raya) / sigma_sombrero)^3)/228

#Curtosis
curtosis = sum(((E - x_raya) / sigma_sombrero)^4)/228

#Cuantiles
cuant25 = quantile(E,0.25,type=1)
cuant50 = quantile(E,0.5,type=1)
cuant75 = quantile(E,0.75,type=1)

#Histograma de datos estandarizados
hist(((E - x_raya) / sigma_sombrero),breaks = 40, main = "Histograma estandarizado", xlab = "Consumo per capita", ylab = "Frecuencia", col = hcl.colors(20, palette = 'viridis'))

#forma no parametrica
n<- length(E)
#esperanza
ex<- sum(E)/n
#error de esperanza
error_ex<- sigma_sombrero/sqrt(n)
#varianza
vx <- function(E){
  return((sum((E-mean(E))^2))/(n-1))
}
vx(E)

#error de varianza
k=replicate(1000,{
  x_boot=sample(E,n,replace=TRUE)
  vx(x_boot)
})

error_vx <- sd(k)
#coef. de asimetria
cx <- function(E){
  return(sum(((E-ex)/(sqrt(vx(E))))^3)/n)
}
cx(E)
#error coef. de asimetria
set.seed(20);s=replicate(1000,{
  x_boot=sample(E,n,replace=TRUE)
  cx(x_boot)
})

error_cx<- sd(s)
#probabilidad de que los individuos tengan un consumo mayor a 20000
  p=function(E){
    p=sum(E>20000)/n
    return(p)
  }
p(E)

#error probabilidad
set.seed(20);j=replicate(1000,{
  x_boot=sample(E,n,replace=TRUE)
  p(x_boot)
})

error_p <- sd(j)

#valores paramétricos de distribución lognormal por metodo de momentos
m<- log(ex/sqrt(1+(vx(E)/(ex^2))))
m
dcuad<- log(1+vx(E)/(ex^2))
dcuad
#maxima verosimilitud
estimadord2 <- function(E){
  b=0
  for (i in 1:length(E)) {
    b= b+ (log(E[i])-mean(log(E)))^2
  }
  return(b/length(E))
}

estimadorm <- function(E) {
  a=0
  for (i in 1:length(E)) {
    a= a+ log(E[i])
  }
  
  return(a/length(E))
}
estimadorm(E)
#esperanza
exln<- function(E, m, d2){
 esperanza<-exp(m+d2/2)
   return(esperanza)
}
exln(E, m, dcuad)
#esperanza máxima verosimilitud
exlnV<- function(E){
  esperanza<-exp(estimadorm(E)+estimadord2(E)/2)
  return(esperanza)
}
exlnV(E)
#error exln

set.seed(20);v=replicate(1000,{
  
  x_boot=sample(E,n,replace=TRUE)
  mboot<- log(mean(x_boot)/sqrt(1+(vx(x_boot)/(mean(x_boot)^2))))
  dboot<- log(1+vx(x_boot)/(mean(x_boot)^2))
  exln(x_boot, mboot, dboot)
})

error_exln <- sd(v)
#varianza
vxln<- function(E){
  m<- log(ex/sqrt(1+(vx(E)/(ex^2))))
  dcuad<- log(1+vx(E)/(ex^2))
  vxln<-exp(2*m+dcuad)*(exp(dcuad)-1)
  return(vxln)
}
vxln(E)
#error vxln
set.seed(20)
t=replicate(1000,{
  x_boot=sample(E,228,replace=TRUE)
  vxln(x_boot)
})

error_vxln <- sd(t)
#coef de asimetria
cxln<- function(E, dcuad){
  return(exp(dcuad)+2)*sqrt(exp(dcuad)-1)
}
cxln(E, dcuad)
#error cxln
set.seed(20);m=replicate(1000,{
  x_boot=sample(E,n,replace=TRUE)
  dboot<- log(1+vx(x_boot)/(mean(x_boot)^2))
  cxln(x_boot, dboot)
})
error_cxln <- sd(m)
#probabilidad 
pln<- function(E){
  m<- log(ex/sqrt(1+(vx(E)/(ex^2))))
  dcuad<- log(1+vx(E)/(ex^2))
  return(1-pnorm((log(20000)-m)/sqrt(dcuad)))
}
pln(E)
#error de probabilidad pln
set.seed(20);w=replicate(1000,{
  x_boot=sample(E,n,replace=TRUE)
  pln(x_boot)
})
error_pln <- sd(w)

#etapa 4
#inferencia estadistica

#test de hipotesis para la media
T=(x_raya-26000)/(sd(E)/sqrt(length(E)))
T
x_raya
sd(E)/sqrt(length(E))
zobs=1-pnorm(T)
zobs
xrayacritico=0.05*(sd(E)/sqrt(length(E)))+x_raya
xrayacritico


#intervalo de confianza para el coeficiente de asimetria

set.seed(15)
d=c()
d=replicate(1000,{
  xboot=sample(E,228,replace=TRUE)
  coefasimetriaboot = sum(((xboot - mean(xboot)) / sd(xboot))^3)/228
  
})
d
errorcoef=sd(d)
errorcoef

A=quantile(d,0.025,type=1)
A

B=quantile(d,0.975,type=1)
B

#intervalo de confianza para la varianza

set.seed(20)
f=c()
f=replicate(1000,{
  yboot=sample(E,228,replace=TRUE)
  (sd(yboot))^2
  
})
f
errorvar=sd(f)
errorvar

A2=quantile(f,0.025,type=1)
A2

B2=quantile(f,0.975,type=1)
B2


#parte 2 bondad de ajuste

#comparamos la distribucion log normal con la distribucion t

#sacamos los parametros
#parametros de la log normal
m=log(x_raya/(sqrt(1+(sd(E)/x_raya)^2)))
m
d2=log(1+(sd(E)/x_raya)^2)
d2

#parametros de la t
t=(E-x_raya)/sd(E)

# Normalizo la log normal
Y=log(E)
Y
#la estandarizo
Yest=(Y-m)/sqrt(d2)

# log verosimilitud de la Normal
logverosimilitudnormal <- sum(dnorm(Yest), log = TRUE)
logverosimilitudnormal
# Para la t
logverosimilitudt=sum(dt(t,2,log=TRUE))
logverosimilitudt


# Ajuste de cuantiles -- QQ Plot
# Calculamos la funcion empirica
qempiricos = sort(t)
n = length(E)
femp = 1:n/(n+1)
# Calculo de cuantiles teoricos con la Normal(0,1)
qteoricos_N01 = qnorm(femp)

# Calculo de cuantiles teoricos con la t de Student
qteoricos_t = qt(femp, 2)

# Grafico
par(mfrow = c(2, 1), mar = c(2,2,2,2))
plot(qempiricos, qteoricos_N01, main= 'Cuantiles Teoricos con Normal(0,1)',pch = 20, col = 'maroon4')
abline(a = 0, b = 1, lty = 3)
abline(a = 0, b = 0)
abline(v = 0)
plot(qempiricos, qteoricos_t, main= 'Cuantiles Teoricos con t-student', pch = 20, col = 'maroon4')
abline(a = 0, b = 1, lty = 3)
abline(a = 0, b = 0)
abline(v = 0)

#Bondad de ajuste con el test Chi cuadrado

hist(t, main = "Frecuencia de parametros de t", ylab = "Frecuencia", freq=FALSE, col = hcl.colors(20, palette = 'viridis'))
hist(t, main = "Frecuencia de parametros de t", ylab = "Frecuencia", freq=FALSE, col = hcl.colors(20, palette = 'viridis'))$breaks #devuelve los puntos de corte
hist(t, main = "Frecuencia de parametros de t", ylab = "Frecuencia", freq=FALSE, col = hcl.colors(20, palette = 'viridis'))$counts #necesito

#este vector va a tener las frecuencias absolutas observadas
fa= c(hist(t)$counts[1:6],11)
fa

#cantidad de clases
nk= length(fa)
nk
length(E)

#vectores que contienen li y ls

li <- c(NA,-1,-0.5,0.0,0.5,1.0,1.5,2.0)
ls <- c(-1,-0.5,0.0,0.5,1.0,1.5,2.0,NA)
li
ls

#Aca elijo la distribucion
#para la probabilidad tendria que estimar los parametros o poner los que dije en la hipotesis nula

p=c()
for(i in 2:(nk-1)){
  p=c(p,pnorm(ls[i])-pnorm(li[i]))
}
p

pnorm(li[2])
p=c(pnorm(li[2]),p)
p[nk]=1-sum(p)
p

fe_n=p*228
fe_n

w_n=sum(((fa-fe_n)^2)/fe_n)
w_n

#esta w tine edistribucion chi^2
#busco los datos para t
p2=c()
for(i in 2:(nk-1)){
  p2=c(p2,pt(ls[i],df=2)-pt(li[i],df=2))
}
p2

pt(li[2],df=2)
p2=c(pt(li[2],df=2),p2)
p2[nk]=1-sum(p2)
p2
fe_t=p2*228
fe_t

w_t=sum(((fa-fe_t)^2)/fe_t)
w_t

df <- nk-1

#Calculo p-valor
pvalort=1-pchisq(sum((fa-fe_t)^2/fe_t),df)
pvalorn=1-pchisq(w_n,df, ncp = 0, lower.tail = TRUE, log.p = FALSE)

pvalort
pvalorn
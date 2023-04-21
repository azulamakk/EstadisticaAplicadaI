#Ejercicio T7.1---------------------------------



#Cargar archivo melbourne properties


#Tomamos un sample de 300 hogares y registramos los precios
#En miles

x <- sample(data$Price, 300)/100

#Realizamos un ensayo de bondad de ajuste
#utilizando el contraste chi cuadrado

hist(x, freq=FALSE)
#la teoria diria que esta es una lognormal

#vamos a probar hacer el analisis con la weibull

#Ho: X tiene distribucion Weibull
#armamos la tabla para el contraste
#frecuencias absolutas
hist(x, breaks=5)$counts

fa <- c(hist(x, breaks=5)$counts[1:3],8)

#cantidad de clases
nk <- length(fa)


#Adaptar y corregir las siguientes líneas ...

#vectores que contienen li y ls
li<-hist(Price)$breaks[1:nk]
ls<-c(hist(Price)$breaks[2:nk],NA)

#armo un data frame para poner todo
table<-cbind.data.frame(li,ls,fa)

#para sacar las fei necesitamos estimar
#omega y beta
#obtengo estimadores de maxima verosimilitud con optim
Verosim<-function(tita){
  return(sum(log(dweibull(x,tita[1],tita[2]))))
}

omega<-optim(c(2,1000000),Verosim,control=list(fnscale=-1))$par[1]
beta<-optim(c(2,1000000),Verosim,control=list(fnscale=-1))$par[2]

#calculo las probabilidades de todas las clases
p<-c()
for(i in 1:nk-1){
  p[i]<-pweibull(ls[i],omega,beta)-pweibull(li[i],omega,beta)
  }
p[nk]<-1-sum(p)

table<-cbind.data.frame(table,p)


#frecuencias esperadas
fe<-round(length(x)*p,4)
table<-cbind.data.frame(table,fe)

#comparativa con la densidad que buscamos
hist(x,freq=FALSE)
curve(dweibull(x,omega,beta), add=TRUE, col='red')

#buscamos el estadistico del test
attach(table)
w<-sum((fa-fe)^2/fe)

#calculamos el p valor
1-pchisq(w,nk-2)

#rechazamos con mucha holgura la Ho
#podemos afirmar que X NO tiene distribucion Weibull


#-------------------hago lo mismo con Ln
remove(table)
#Ho: X tiene distribucion Lognormal
#armamos la tabla para el contraste
#frecuencias absolutas
fa<-c(hist(Price)$counts[1:12],6)

#cantidad de clases
nk<-length(fa)

#vectores que contienen li y ls
li<-hist(Price)$breaks[1:nk]
ls<-c(hist(Price)$breaks[2:nk],NA)

#armo un data frame para poner todo
table<-cbind.data.frame(li,ls,fa)

#para sacar las fei necesitamos estimar
#omega y beta
#obtengo estimadores de maxima verosimilitud con optim
m<-mean(log(x))
D<-sd(log(x))
  
#calculo las probabilidades de todas las clases
p<-c()
for(i in 1:nk-1){
  p[i]<-plnorm(ls[i],m,D)-plnorm(li[i],m,D)
}
p[nk]<-1-sum(p)

table<-cbind.data.frame(table,p)


#frecuencias esperadas
fe<-round(length(x)*p,4)
table<-cbind.data.frame(table,fe)

#comparativa con la densidad que buscamos
hist(x,freq=FALSE)
curve(dlnorm(x,m,D), add=TRUE, col='red')

#buscamos el estadistico del test
attach(table)
w<-sum((fa-fe)^2/fe)

#calculamos el p valor
1-pchisq(w,nk-2)

#dio muy peque?o por tratarse de una muestra
#muy grande, pero es mucho mejor que 
#el de la weibull.

#una opcion en estos casos
#es comparar el estadistico w
#para ver cual es mejor.




#Ejercicio T7.2----------------------------------

#Carga del archivo Amazon
#Ejercicio T7.1---------------------------------

data <- read.csv("~/Desktop/Estadistica aplicada/1C2023/Clases/melbourne properties.csv")

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

fa <- c(hist(x, breaks=7)$counts[1:5],8)

#cantidad de clases
nk <- length(fa)


#Adaptar y corregir las siguientes líneas ...

#vectores que contienen li y ls
li<-hist(x)$breaks[1:nk]
ls<-c(hist(x)$breaks[2:nk],NA)

#armo un data frame para poner todo
table<-cbind.data.frame(li,ls,fa)
table
#para sacar las fei necesitamos estimar
#omega y beta
#obtengo estimadores de maxima verosimilitud con optim
Verosim<-function(tita){
  return(sum(log(dweibull(x,tita[1],tita[2]))))
}

tita_opt<- optim(c(1.5,20000),
                 Verosim,
                 control = list(fnscale=-1))$par
  
omega<-optim(c(2,1000000),Verosim,control=list(fnscale=-1))$par[1]
beta<-optim(c(2,1000000),Verosim,control=list(fnscale=-1))$par[2]

# Estimadores para log-normal - Maxima verosimilitud
eme <- log(sum(x))/300
d <- sqrt(sum(log(x)-eme)^2/300)

sum(log(dweibull(x, omega,beta)))
sum(log(dlnorm(x, eme, d)))

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
fa<-c(hist(x)$counts[1:12],6)

#cantidad de clases
nk<-length(fa)

#vectores que contienen li y ls
li<-hist(x)$breaks[1:nk]
ls<-c(hist(x)$breaks[2:nk],NA)

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

# Calcula la log verosimilidus de los valores que yo tenga para un tita


#Ejercicio T7.2----------------------------------

#Carga del archivo Amazon
datos <- read.csv("~/Desktop/Estadistica aplicada/1C2023/Clases/amazon.txt", sep="")

x<-datos$X0

tau <- mean(x)
tau
rho <- sd(x)
rho

#Extraemos los residuos
e <- (x-tau)/rho
plot(density(e))

#Test Jarque Bera
qchisq(0.95, 2)

gamma <- mean(e**3)
k <- mean(e**4)

JB <- length(x)/6 * (gamma**2+0.25*(k-3)**2)
JB

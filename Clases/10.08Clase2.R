#Ejercicios de clase Intro a R

#Ejercicio 1
#Variable 1
u <- runif(20, 10, 11)
u

#Setear semilla
set.seed(2)
x <- runif(20,10,11)
x

y <- x[c(3,4,5)]
y[c(FALSE, TRUE, FALSE)]
y[2]
y[c(2,3)]

#Ejercicio 2

#Objetivo B
#Simular una realización de la variable N
#A partir del vector x ya simulado

length(x[x>10.6])

#Inciso C
n <- c()
n[1] <- length(x[x>10.6])

for(i in 2:30){
  x <- runif(20,10,11)
  n[i] <- length(x[x>10.6])
}
n

#Variable 3
#Simular la produccion de 30 dias y cargas en un vector los t datos

t <- replicate(30, {runif(1,10,11)})
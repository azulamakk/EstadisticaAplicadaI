#Simulacion de un test de hipotesis

#Parametros del test
n <- 25
k <- 309.86
  
#Creamos la realidad
mu <- 320
mean(replicate(10000,{
  #Simulamos la muestra
  x <- rnorm(n, mean = mu, sd = 30)
  
  #Vemos si rechaza
  mean(x) > k
}))


#Armamos la curva de potencia
potencia_empirica_1 <- function(mu){
  mean(replicate(10000,{
    #Simulamos la muestra
    x <- rnorm(n, mean = mu, sd = 30)
    
    #Vemos si rechaza
    mean(x) > k
  }))
  
}

potencia_empirica_2 <- function(mu){
  mean(replicate(10000,{
    #Simulamos la muestra
    x <- rexp(n, rate = 1/mu)
    
    #Vemos si rechaza
    mean(x) > k
  }))
  
}

potencia_empirica_3 <- function(mu){
  mean(replicate(10000,{
    #Simulamos la muestra
    x <- rexp(500, rate = 1/mu)
    
    #Vemos si rechaza
    mean(x) > k
  }))
  
}


grid <- as.matrix(seq(300, 340, 1))
pi <- apply(grid, 1, potencia_empirica_1)
pi2 <- apply(grid, 1, potencia_empirica_2)
pi3 <- apply(grid, 1, potencia_empirica_3)

plot(grid, pi, type = 'l', lwd=4)
lines(grid, pi2, type='l', lwd=4, col='red')
lines(grid, pi3, type='l', lwd=4, col='green')







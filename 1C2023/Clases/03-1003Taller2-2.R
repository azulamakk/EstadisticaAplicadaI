
fEmprica <- function(x, x0){
  cumple = 0 
  for(i in 1: length(x)){
    if(x[i]<x0){
      cumple=cumple+1
    }
  }
  n <- length(x)
  return(cumple/n)
}

fEmpirica1 <- function(x, x0){
  cumple <- sum(x<=x0)
  n <- length(x)
  return(cumple/n)
}

x <- data$purchases
x0 <- 10000

fEmpirica1(x,x0)

ecdf(data$purchases)(x0)

xCredito <- data$credit_limit
xLimite<-mean(xCredito)

1-fEmpirica1(xCredito, xLimite)
i=0
while(fEmpirica1(xi, omega)){
  i=i+1
}
quantile(x,0.9,type=1)
cuantil_muestral<- function(x, cuantilBuscado){
  cuantil <- quantile(x, cuantilBuscado, type=1)
  return(cuantil)
}
cuantil_muestral(x, 0.9)

# Covarianza y correlacion
#Metodo 1
Z <- as.matrix(scale(data))
R <- t(Z)%*%Z*1/(nrow(Z)-1)
det(R)
#Cuanto mas cerca del 0, mas correlacion hay en entre las variables
#Metodo 2
1/(nrow(Z)-1)*t(Z) %*% Z
#Metodo 3
cor(data)
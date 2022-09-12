import math

x = c(118.4 , 112.96 , 120.88 , 145.4 , 126.59 , 133.71 , 97.59 , 114.51 , 136.42 , 114.96, 70.66 , 150.51 , 129.74 , 69.97 , 82.77 , 84.96 , 97.79 , 146.27 , 68.24 , 108.35 ,150.68 , 95.12 , 80.92 , 120.17 , 94.5 , 102.99 , 166.53 , 115.48 , 123.32 , 92.17 ,129.18 , 121.46 , 124.15 , 113.68 , 147.97 , 104.31 , 89.41 , 61.81 , 77.53 , 100.91, 110.86 , 147.94 , 104.48 , 126.08 , 147 , 150.78 , 121.25 , 188.61 , 147.13 ,170.24 , 114.42 , 123.36 , 147.26 , 167.96 , 132.95 , 139.76 , 96.49 , 135.92 ,172.71 , 89.18 , 91.62 , 73.09 , 148.85 , 160.65 , 135.9 , 114.51 , 185.65 , 130.65 ,153.54 , 78.12 , 91.52 , 137.89 , 90.4 , 121.58 , 140.68 , 109.16 , 152.83 , 132 ,137.32 , 127.85 , 139.76 , 99.34 , 113.63 , 143.43 , 187.01 , 114.44 , 116.36 ,151.84 , 107.02 , 134.38 , 221.53 , 142.03 , 112.05 , 103.2 , 144.75 , 110.56 ,142.39 , 75.05 , 117.44 , 135.78)

hist(x)

#Grupo 1
q1 <- function(x){
  return(quantile(x, 0.9, type=1))
}
q1(x)

q2 <- function(x){
  beta = sd(x)^2/mean(x)
  alfa = (mean(x)/sd(x))^2
  return(qgamma(0.9,shape = alfa, scale = beta))
}
q2(x)

q3 <- function(x){
  m = log(mean(x)/sqrt(1+(sd(x)/mean(x)^2)))
  d = sqrt(log(1+(sd(x)/mean(x))^2))
  return(exp((qnorm(0.9)*d+m)))
}
q3(x)


mean(sample(0:1,1000000, replace = TRUE))

#Grupo 2

#Se desea estimar la proporción de individuos cuyo saldo es superior a $ 150.000
#Forma no parametrica

#Suponiendo que la variable tiene distribución Normal
n = length(x)
a <- sum(x>150)/n
qa <- function(x){
  return(sum(x>150)/n)
}
qa(x)
#b
mu_sombrero <- sum(x)/n
sigma_sombrero <- sqrt(sum(x-mu_sombrero)^2/n)
1-pnorm((150-mu_sombrero)/sigma_sombrero)

qb <- function(mu, sigma){
  return(1-pnorm((150-mu_sombrero)/sigma_sombrero))
}
qb(x)
#c
qc = function(x){
  x_raya=sum(x)/length(x)
  s=sqrt(sum(x-x_raya)**2/length(x))
  m_est=log(x_raya/sqrt(1+(s/x_raya)**2))
  d2_est=log(1+(s/x_raya)**2)
  cuantil=exp(1)**(qnorm(0.9)*sqrt(d2_est)+m_est)
  return(cuantil)
}




# --- Grupo 2 ---

x = c(118.4 , 112.96 , 120.88 , 145.4 , 126.59 , 133.71 , 97.59 , 114.51 , 136.42 , 114.96, 70.66 , 150.51 , 129.74 , 69.97 , 82.77 , 84.96 , 97.79 , 146.27 , 68.24 , 108.35 ,150.68 , 95.12 , 80.92 , 120.17 , 94.5 , 102.99 , 166.53 , 115.48 , 123.32 , 92.17 ,129.18 , 121.46 , 124.15 , 113.68 , 147.97 , 104.31 , 89.41 , 61.81 , 77.53 , 100.91, 110.86 , 147.94 , 104.48 , 126.08 , 147 , 150.78 , 121.25 , 188.61 , 147.13 ,170.24 , 114.42 , 123.36 , 147.26 , 167.96 , 132.95 , 139.76 , 96.49 , 135.92 ,172.71 , 89.18 , 91.62 , 73.09 , 148.85 , 160.65 , 135.9 , 114.51 , 185.65 , 130.65 ,153.54 , 78.12 , 91.52 , 137.89 , 90.4 , 121.58 , 140.68 , 109.16 , 152.83 , 132 ,137.32 , 127.85 , 139.76 , 99.34 , 113.63 , 143.43 , 187.01 , 114.44 , 116.36 ,151.84 , 107.02 , 134.38 , 221.53 , 142.03 , 112.05 , 103.2 , 144.75 , 110.56 ,142.39 , 75.05 , 117.44 , 135.78)


# Proba sin parametros -----------------
qa <- function(datos){
  return(sum(datos>150)/length(datos))
}
qa(x)

# Proba con Normal -------------------------
qb <- function(x,mu,sigma){
  return(1-pnorm((x-mu)/sigma))
}
mu <- sum(x)/length(x)
mu
sigma <- sqrt(sum((x-mu)^2)/length(x))
sigma

qb(150,mu,sigma)


# Proba con Log Normal MV ------------------
qc <- function(x){
  m <- sum(log(x))/length(x)
  D <- sqrt(sum((log(x)-m)^2)/length(x))
  return(1-pnorm((log(150)-m)/D))
}

# ----- m y D con Momentos y MV -------

m_mom <- log(mu/sqrt(1+(sigma/mu)^2))
m_mom
D_mom <- sqrt(log(1+(sigma/mu)^2))
D_mom
m <- sum(log(x))/length(x)
D <- sqrt(sum((log(x)-m)^2)/length(x))
D

# ---------------------------------------
# qc(150,m,D)

set.seed(20)

q_boot <- replicate(1000, {
  qc(sample(x,100,replace=TRUE))
})
q_boot
sd(q_boot)


# ESPERANZA sin parametro------------

esperanza <- function(x){
  return(sum(x)/length(x)) 
}
esperanza(x)
sd(x)/sqrt(length(x))

# ESPERANZA con log normal MV ---------

qespLN <- function(x){
  m <- sum(log(x))/length(x)
  D <- sqrt(sum((log(x)-m)^2)/length(x))
  return(exp(m+(D^2/2)))
} 
qespLN(x)
set.seed(20)
qesp_boot <- replicate(1000,{
  qespLN(sample(x,100,replace=1000))
})
sd(qesp_boot)

# ESPERANZA con Gamma MV ---------

alfa <- (mu/sigma)^2
alfa
beta <- (sigma^2/mu)
beta

# Método de máxima verosimilutud
#  dgamma(x,shape=alfa,scale=beta)

log(prod(dgamma(x,shape=17.55,scale=7))) #log para que sea un numero más lindo


#optim(#vector con parametros que quiero que vaya cambiando, 
  #funcion que depende de los parametros y arroja el objetivo,
  #control=list(-1))

par <- c(17.55,7) # alfa y beta 
log_verosim <- function(par){
  log(prod(dgamma(x,shape=par[1],scale=par[2])))
}

opt <- optim(par,log_verosim,control=list(fnscale=(-1)))
opt$par
opt$value

# función de esperanza con GAMMA de MV

qespG <- function(x){
  log_verosim <- function(par){
    log(prod(dgamma(x,shape=par[1],scale=par[2])))
  }
  opt <- optim(par,log_verosim,control=list(fnscale=(-1)))
  return(prod(opt$par))
}

qespG(x)
qesp_boot <- replicate(1000, {
  qespG(sample(x,100,replace=TRUE))
})

sd(qesp_boot)




x <- c( 15.6 , 78.8 , 10.3 , 1326.8 , 105.5 , 10.6 , 144.7 , 239 , 172.7 , 29.6 , 1122.8 , 119.1
        , 15.8 , 0.7 , 517.9 , 49.9 , 52.9 , 360.6 , 282.2 , 179.1 , 343.1 , 260.9 , 63.4 , 1 ,
        188.6 , 48.8 , 40 , 2.9 , 21 , 125.9 , 826.6 , 44.5 , 118.6 , 49 , 3.5 , 23.8 , 24.8 ,
        48.5 , 492.8 , 251.2 , 39.3 , 32.9 , 220.1 , 166.2)
mean(x)
sd(x)

# Metodo Bootstrap percentil
boot <- replicate(1000,{
  as.numeric(quantile(sample(x, length(x), replace=TRUE), 0.5, type=1))
})

quantile(boot, c(0.05, 0.95))
# Metodo con pivote normal estimando el error estandar
set.seed(1)
boot2a<- replicate(1000,{
  as.numeric(quantile(sample(x, replace = TRUE), 0.5, type=1))
})
sd(boot2a)
boot2b<- replicate(1000,{
  as.numeric(mean(sample(x, length(x), replace = TRUE)))+1.6448*sd(sample(x, length(x), replace = TRUE))
})
boot2b

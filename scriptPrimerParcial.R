x <- c(860, 810, 1060, 860, 840, 2120, 1200, 920, 1090, 980, 920, 2750, 880, 740, 700, 900, 1450, 1030, 3120, 960, 1000, 703, 950, 990, 970, 850, 900, 1040, 1020, 840)
plot(ecdf(x))
hist(x)
sd(x)
mean(x)
length(x)
x-mean(x)
asimetria <- sum((x-eme)/de)^4
asimetria
eme <- sum(log(x))/length(x)
de <- sum((log(x)-eme)^2)/length(x)

n <- length(x)
sd(x)
set.seed(13)


quantile(x, 0.95)
#Proporcion no parametrica
p <-sum(x>200)/n
set.seed(1167)
proporcion=c()
proporcion<-replicate(1000,{
  XBOOT=sample(x,n,replace=TRUE)
  p <-sum(XBOOT>200)/n
})
proporcion
mean(proporcion)
# Error estandar
errorq12 <- sqrt(var(proporcion))
errorq12


#
# Curtosis muestral
curtosis=c()
curtOriginal = sum(((x-mean(x))/sd(x))^4)
curtOriginal
curtosis<-replicate(1000,{
  XBOOT=sample(x,n,replace=TRUE)
  sum(((XBOOT-mean(XBOOT))/sd(XBOOT))^4)
})
curtosis
mean(curtosis)
# Error estandar
errorq12 <- sqrt(var(curtosis))
errorq12

# Asimetria muestral
asimetria=c()
asimOriginal = sum((x-mean(x))^3/sd(x))*(1/n)
asimetria<-replicate(1000,{
  XBOOT=sample(x,n,replace=TRUE)
  sum((XBOOT-mean(XBOOT))^3/sd(XBOOT))*(1/n)
})
asimetria
mean(asimetria)
errorq12 <- sd(asimetria)
errorq12


# Cuantil no parametrico
cuantil=c()
qOriginal = quantile(x, 0.9, type=1)
f<-replicate(1000,{
  XBOOT=sample(x,n,replace=TRUE)
  quantile(XBOOT, 0.9, type=1)
})
f
errorq12 <- sqrt(var(f))
errorq12


# Contraste chi-cuadrado
x <- c(-0.16, -0.19, 1.52, -1.4, -0.5, -4.84, 15.9, 4.05, 19.27, -0.57, -2.74, -15.29, 2.06,5.2, 
       -0.88, 3.54, 18.45, -15.66, -0.28, 4.41, 2.31, 4.83, 17.88, -17.91, 2.5, 4.15,4.49, -0.93, 8.49, 1.31, 2.98, 11.3, -10.48,
       -1.53, -4.71, 6.54, -13.43, -2.1, 5.21,4.21, 12.26, 0.46, -7.4, -9.33, 4.82)

mean(x)
sd(x)
length(x)

hist(x, freq=FALSE)

hist(x, breaks=5)$counts

fa <- c(hist(x, breaks=5)$counts)

nk <- length(fa)

li<-hist(x)$breaks[1:nk]
ls<-c(hist(x)$breaks[2:nk],NA)

table<-cbind.data.frame(li,ls,fa)
table



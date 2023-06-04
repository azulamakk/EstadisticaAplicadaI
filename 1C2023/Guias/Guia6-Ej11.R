x <- c(14.75,13.25, 11.85, 14.45, 15.75, 17)
y <- c(13.25, 12.75, 12.15, 13.55, 14.25, 15)

mean(x)
mean(y)

sd(x)
sd(y)

var(x)
var(y)

c <- c(1.12, 0.06, 0.08, -0.01)
x1 <- c(0.43, -1.73, -0.09, -2.21)
x2 <- c(2.21, -1.98, 0.8, -1.32)

X <- cbind(c, x1, x2) # matriz de datos
apply(X, 2, function(i) sum(2*i))

#SCE
promedio <- mean(X)
SCE <- sum(apply(X, 2, function(i) length(i)*(mean(i)-promedio)^2))

#SCD
SCDParciales <- apply(X, 2, function(i) (i-mean(i))^2)

SCD <- sum(SCDParciales)

SCTOTAL <- SCD + SCE

Fobs <- (SCE/ (3-1)) / (SCD / (12-3))

1-pf(Fobs)
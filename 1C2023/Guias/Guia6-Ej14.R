e1 <- c(353, 294, 329, 282, 317, 250)
e2 <- c(325, 333, 327, 281, 285, 383)
e3 <- c(285, 316, 393, 393, 336, 397)
e4 <- c(417, 408, 431, 425, 390, 408)
e5 <- c(344, 331, 272, 275, 272, 308)

mean(e4)-mean(e5)

p=5
n=30

# ----------- inciso a
X <- cbind(e1, e2, e3, e4, e5) # matriz de datos
sd(X)^2
apply(X, 2, function(i) sum(2*i))

#SCE
promedio <- mean(X)
SCE <- sum(apply(X, 2, function(i) length(i)*(mean(i)-promedio)^2))

#SCD
SCDParciales <- apply(X, 2, function(i) (i-mean(i))^2)

SCD <- sum(SCDParciales)

SCTOTAL <- SCD + SCE

Fobs <- (SCE/ (p-1)) / (SCD / (n-3))

# ----------- inciso b
qtukey(0.95, 30, 27)

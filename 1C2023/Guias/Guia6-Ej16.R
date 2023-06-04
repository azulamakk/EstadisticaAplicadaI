t1 <- c(9,3,2,7,7,6,3,7)
t2 <- c(8,2,4,9)
t3 <- c(6,3,1,4)
t4 <- c(9,7,4,8)

N = 20
p = 5 
X <- cbind(t1, t2, t3, t4)
SpoolParciales <- apply(X, 2, function(i) (i-mean(i))^2)

Spool <- sum(SpoolParciales) / (N-p)

# ----------- inciso b
# residuos
rt1 <- mean(t1) - t1
rt2 <- mean(t2) - t2
rt3 <- mean(t3) - t3
rt4 <- mean(t4) - t4

residuos <- cbind(rt1,rt2,rt3,rt4)
qqnorm(residuos, col='pink3', pch=16)

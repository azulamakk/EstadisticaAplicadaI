x1 <- c(22, 15, 37, 19, 21, 14, 28)
x2 <- c(54, 23, 33, 35, 11)
x3 <- c(40, 33, 51, 42, 31, 47, 32, 55)

# ----------------------- Inciso b
X <- cbind(x1, x2, x3) # matriz de datos

apply(X, 2, function(i) sum(2*i))

#SCE
promedio <- mean(X)
SCE <- sum(apply(X, 2, function(i) length(i)*(mean(i)-promedio)^2))

#SCD
SCDParciales <- apply(X, 2, function(i) (i-mean(i))^2)

SCD <- sum(SCDParciales)

SCTOTAL <- SCD + SCE

Fobs <- (SCE/ (3-1)) / (SCD / (20-3))


# ----------------------- Inciso c
residuosx1 <- mean(x1) - x1
residuosx2 <- mean(x2) - x2
residuosx3 <- mean(x3) - x3

residuos <- c(residuosx1, residuosx2, residuosx3)

programas <- factor(rep(c("Programa 1", "Programa 2", "Programa 3"), lengths(list(residuosx1, residuosx2, residuosx3))))

# Calcular los cuantiles esperados de una distribución normal
cuantiles_esperados <- qnorm(ppoints(length(residuos)))

# Crear el QQ-Plot Normal coloreando los puntos según el programa de origen
plot(cuantiles_esperados, residuos, xlab = "Cuantiles esperados (Distribución normal)",
     ylab = "Residuos", main = "QQ-Plot Normal", pch = 16)

# Agregar los puntos según el programa de origen con colores específicos
points(cuantiles_esperados[programas == "Programa 1"], residuos[programas == "Programa 1"], col = "blue", pch = 16)
points(cuantiles_esperados[programas == "Programa 2"], residuos[programas == "Programa 2"], col = "red", pch = 16)
points(cuantiles_esperados[programas == "Programa 3"], residuos[programas == "Programa 3"], col = "green", pch = 16)

# Agregar una línea de referencia para comparar con los puntos
abline(0, 1, col = "black", lwd=4)

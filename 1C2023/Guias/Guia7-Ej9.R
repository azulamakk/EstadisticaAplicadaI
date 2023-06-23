caucho <- read.csv("~/Desktop/Estadistica aplicada/1C2023/Guias/datasets/caucho.txt")

concentracion <- caucho$concentracion
vida <- caucho$vida

plot(concentracion, vida)

B1 <- cov(concentracion,vida)/var(concentracion)
B0 <- mean(vida) - B1*mean(concentracion)
abline(a = B0, b = B1, col = "red", lwd = 4)

residuos <- mean(vida) - vida

modelo <- lm(vida ~ concentracion)
plot(concentracion, rstandard(modelo), pch=20)

# con ln
logConcentracion <- log(concentracion)
logVida <- log(vida)

plot(logConcentracion, logVida)
logB1 <- cov(logConcentracion,logVida)/var(logConcentracion)
logB0 <- mean(logVida) - B1*mean(logConcentracion)
abline(a = logB0, b = logB1, col = "red", lwd = 4)

modeloLog <- lm(logVida ~ logConcentracion)
plot(logConcentracion, rstandard(modeloLog))

 # Intervalo de prediccion
x0 = 150
n <- length(logConcentracion)
leverage <- (x0 - mean(logConcentracion))^2 / sum((logConcentracion - mean(logConcentracion))^2)

logVida_sombrero <- logB0 + logB1*logConcentracion
S2 <- sum((logVida-logVida_sombrero)**2)/(n-2)
sigma2 <- sqrt(S2 * (1 + (1/n) + leverage))

y0 <- logB0 + logB1 * 150

LI <- y0 + qt(0.05, length(logVida)) * sigma2
LS <- y0 + qt(0.95, length(logVida)) * sigma2

# Tengo que hacer e^(limite que me de) para obtener un valor original
LIReal <- exp(LI)
LSReal <- exp(LS)

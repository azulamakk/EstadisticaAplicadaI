paises <- read.delim("~/Desktop/Estadistica aplicada/1C2023/Guias/datasets/paises.txt")

origx1 <- paises$tasanat
origx2 <- paises$tasamort
origx3 <- paises$mortinf
origx4 <- paises$espH
origx5 <- paises$espM
plot(origx1, origx2, xlab = "x1 (log)", ylab = "x2 (log)", main = "Scatter Plot") # Ajusta las etiquetas según tus variables
points(origx1, origx3, col = "blue") # Agregar otra variable en un color diferente
points(origx1, origx4, col = "green") # Agregar otra variable en un color diferente
points(origx1, origx5, col = "red") # Agregar otra variable en un color diferente
legend("topleft", legend = c("x2", "x3", "x4", "x5"), col = c("black", "blue", "green", "red"), pch = 1)

x1 <- log(paises$tasanat)
x2 <- log(paises$tasamort)
x3 <- log(paises$mortinf)
x4 <- log(paises$espH)
x5 <- log(paises$espM)

plot(x1, x2, xlab = "x1 (log)", ylab = "x2 (log)", main = "Scatter Plot") # Ajusta las etiquetas según tus variables
points(x1, x3, col = "blue") # Agregar otra variable en un color diferente
points(x1, x4, col = "green") # Agregar otra variable en un color diferente
points(x1, x5, col = "red") # Agregar otra variable en un color diferente
legend("topleft", legend = c("x2", "x3", "x4", "x5"), col = c("black", "blue", "green", "red"), pch = 1)

X <- cbind(1, x1, x2, x3, x4, x5)
y <- paises$PNB

beta_somb <- solve(t(X) %*% X) %*% t(X) %*% y

n <- length(y)
p <- ncol(X)
df <- n - p

residuos <- y - X %*% beta_somb
sigma2 <- sum(residuos^2) / df
var_cov <- sigma2 * solve(t(X) %*% X)

SE_beta <- sqrt(diag(var_cov))

estadisticos_t <- beta_somb / SE_beta
p_valores <- 2 * (1 - pt(abs(estadisticos_t), df))

# inciso c
# Coeficiente DET
det(cor(cbind(x1, x2, x3, x4, x5)))

# Fatores de inflacion de la varianza
solve(cor(cbind(x1, x2, x3, x4, x5)))


# inciso d
# Cálculo de la suma de cuadrados de los residuos
SSR <- sum(residuos^2)
# Cálculo de la suma de cuadrados de Y respecto a su promedio
SST <- sum((y - mean(y))^2)

R2 <- 1 - SSR/SST
R2_ajustado <- 1 - (SSR/(df))/(SST/(n - 1))

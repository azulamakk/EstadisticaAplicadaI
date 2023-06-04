azufre <- c(5, 5, 5, 5.26, 5.26, 5.26, 5.51, 5.51, 5.51)
neopreno <- c(3, 4.23, 5.57, 3, 4.23, 5.57, 3, 4.23, 5.57)
dureza <-  c(64.36, 61.92, 60.68, 63.76, 63.5, 62.64, 69.04, 63.88, 64.75)

# Matrices
X <- cbind(1, azufre, neopreno)
y <- matrix(dureza)

# Beta Sombrero
beta_somb <- solve(t(X) %*% X) %*% t(X) %*% y
cat("Intercepto (β0):", beta_somb[1], "\n")
cat("Azufre (β1):", beta_somb[2], "\n")
cat("Neopreno (β2):", beta_somb[3], "\n\n")

# Grados de libertad
n <- length(dureza)
p <- ncol(X)
df <- n - p

# Cálculo de parametros necesarios
residuos <- y - X %*% beta_somb
sigma2 <- sum(residuos^2) / df
var_cov <- sigma2 * solve(t(X) %*% X)

# Cálculo de los errores estándar de los coeficientes
SE_beta <- sqrt(diag(var_cov))

# Matriz centrada
X_cent <- scale(X, scale = FALSE)

# Cálculo de los estadísticos t y p-valores
estadisticos_t <- beta_somb / SE_beta
p_valores <- 2 * (1 - pt(abs(estadisticos_t), df))

# Chequeo con lm()
data = data.frame(azufre, neopreno, dureza)
modelo <- lm(dureza ~ azufre + neopreno, data = data)
summary(modelo)

# Cálculo de la suma de cuadrados de los residuos
SSR <- sum(residuos^2)
# Cálculo de la suma de cuadrados de Y respecto a su promedio
SST <- sum((y - mean(y))^2)

R2 <- 1 - SSR/SST
R2_ajustado <- 1 - (SSR/(df))/(SST/(n - 1))

# Imprimir R² y R² ajustado
cat("Coeficiente de determinación R²:", R2, "\n")
cat("Coeficiente de determinación R² ajustado:", R2_ajustado, "\n")

#Matriz de Leverage
leverage <- diag(X %*% solve(t(X) %*% X) %*% t(X))

cat("Diagonal de la Matriz de Leverage:\n")
print(leverage)

# Intervalo de confianza
x0 <- matrix(c(1, 5, 7))
h0 <- diag(x0 %*% solve(t(x0) %*% x0) %*% t(x0))

mu0 <- t(x0) %*% betaSombrero
# ----------------------------------------
# Multi colinealidad
plot(mtcars[,c(4,6)])

# Coeficiente DET
det(cor(mtcars[,c(4,6,7,8)]))

# Fatores de inflacion de la varianza
solve(cor(mtcars[,c(4,6,7,8)])) 
# Un buen truco es ver cual es mayor, es el que mas molesta y esta muy perjudicado
# Dos soluciones
# - Sacar variables y se mide de nuevo la bondad del modelo (con el Raj, se tiene que volver a calcular el error, el determinante, etc etc)
# - Reduccion de la dimension (combnacion de las variables)
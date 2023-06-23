helados <- read.csv2("~/Desktop/Estadistica aplicada/1C2023/Guias/datasets/helados(1).csv")

x1 <- helados$ingreso
x2 <- helados$precio
x3 <- helados$temp_media

y <- helados$consumo

X <- cbind(1, x1, x2, x3)
y <- matrix(y)

beta_somb <- solve(t(X) %*% X) %*% t(X)%*%y

# Grados de libertad
n <- length(y)
p <- ncol(X)
df <- n - p

# Cálculo de parametros necesarios
residuos <- y - X %*% beta_somb
sigma2 <- sum(residuos^2) / df
var_cov <- sigma2 * solve(t(X) %*% X)

SE_beta <- sqrt(diag(var_cov))

SSR <- sum(residuos^2)
SST <- sum((y - mean(y))^2)

R2 <- 1 - SSR/SST
R2_ajustado <- 1 - (SSR/(df))/(SST/(n - 1))

# inciso c
det(cor(cbind(x1,x2,x3)))

# inciso d
X1 <- cbind(1, x1, x3)
beta_somb1 <- solve(t(X1) %*% X1) %*% t(X1)%*%y

residuos1 <- y - X1 %*% beta_somb1
sigma21 <- sum(residuos1^2) / df
var_cov1 <- sigma21 * solve(t(X1) %*% X1)

SE_beta1 <- sqrt(diag(var_cov1))
SSR1 <- sum(residuos1^2)

R21 <- 1 - SSR1/SST
R2_ajustado1 <- 1 - (SSR1/(df))/(SST/(n - 1))

det(cor(cbind(x1,x3)))

# inciso e
x0 <- c(1, 80, 0.27, 70)  # Valores de x1, x2 y x3 respectivamente
Y_ajustado <- sum(beta_somb * x0)

residuos <- y - Y_ajustado

varianza_residual <- sum(residuos^2) / (n - p - 1)

error_estandar <- sqrt(varianza_residual)

t_value <- qt(1 - 0.05/2, n - p - 1)  # Obtener el valor crítico de t
intervalo_confianza <- c(Y_ajustado - t_value * error_estandar, Y_ajustado + t_value * error_estandar)
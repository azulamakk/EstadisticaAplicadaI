# Datos proporcionados
sum_x <- 97
sum_y <- 21.16
sum_xy <- 180.93
sum_x2 <- 1169
sum_y2 <- 48.68

# Tamaño de la muestra
n <- 10

# Estimación del coeficiente de pendiente
b1 <- (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x^2)

Q <- (sum_y2 - (sum_y^2 / n) - b1 * (sum_xy - (sum_x * sum_y / n)))
# Cálculo del error estándar de la pendiente (SE)
SE_b1 <- sqrt(Q/ (n - 2)) / sqrt(sum_x2 - (sum_x^2 / n))


# Cálculo del error estándar de la pendiente (SE)
SE_b1 <- sqrt(Q/ (n - 2)) / sqrt(sum_x2 - (sum_x^2 / n))

# ------------ insico b

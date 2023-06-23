# Datos proporcionados
sum_x <- 39
sum_y <- 46
sum_xy <- 366
sum_x2 <- 287
sum_y2 <- 478

# Tamaño de la muestra
n <- 8

# Estimación del coeficiente de pendiente
b1 <- (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x^2)

Q <- (sum_y2 - (sum_y^2 / n) - b1 * (sum_xy - (sum_x * sum_y / n)))
# Cálculo del error estándar de la pendiente (SE)
SE_b1 <- sqrt(Q/ (n - 2)) / sqrt(sum_x2 - (sum_x^2 / n))

x0=5
leverage <- (x0 - (sum_x/10)) / (sum_x2 - (sum_x^2 / n))

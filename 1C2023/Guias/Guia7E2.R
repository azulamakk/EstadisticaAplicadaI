aditivo <- c(0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22, 0.23)
resistencia <- c(3.34, 3.23, 3.62, 3.61, 3.75, 3.86, 3.87, 3.89, 4.01, 4.03)

plot(aditivo, resistencia)

# Variabilidad total

varTotal <- sum((resistencia-mean(resistencia))**2) 


# Intervalo 

# B1
B1 <- cov(aditivo,resistencia)/var(aditivo)

# B0
B0 <- mean(resistencia) - B1*mean(aditivo)

#S2
resistencia_sombrero <- B0 + B1*aditivo
S2 <- sum((resistencia-resistencia_sombrero)**2)/(length(aditivo)-2)
mean(aditivo)

SCx <- sum((aditivo-mean(aditivo))^2)
0.185-0.193

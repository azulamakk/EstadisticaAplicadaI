x <- c(18.27, 18.41, 17.85, 19.80, 18.20, 19.96, 18.75, 17.63)
y <- c(17.12, 16.99, 16.55, 18.20, 16.80, 17.70, 16.50, 15.40)

length(x)
plot(density(x))
plot(density(y))
plot(x, y)
length(x)
# B1
B1 <- cov(x,y)/var(x)
# Ante un aumento de una unidad de la humedad a la salida del secadero, la 
# esperanza de la humedad a la entrada del silo aumenta en 0.83112

# B0
B0 <- mean(y) - B1*mean(x)

#S2
y_sombrero <- B0 + B1*x
S2 <- sum((y-y_sombrero)**2)

Q <- sum((y-y_sombrero)**2)
Te <- sum((y-mean(y))**2)

Se_B1 <- sqrt(Q/(length(x)-2))
B1/Se_2

# Inciso d
options(scipen=999)
SE_y0 <- sqrt(S2*((19-mean(x))^2)/((sum(x-mean(x))^2)))

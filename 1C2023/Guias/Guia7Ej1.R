x <- c(18.27, 18.41, 17.85, 19.80, 18.20, 19.96, 18.75, 17.63)
y <- c(17.12, 16.99, 16.55, 18.20, 16.80, 17.70, 16.50, 15.40)


plot(density(x))
plot(density(y))

# B1
B1 <- cov(x,y)/var(x)
# Ante un aumento de una unidad de la humedad a la salida del secadero, la 
# esperanza de la humedad a la entrada del silo aumenta en 0.83112

# B0
B0 <- mean(y) - B1*mean(x)

#S2
y_sombrero <- B0 + B1*x
S2 <- sum((y-y_sombrero)**2)/(length(x)-2)


volumen <- c(45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
presion <- c(82, 64.7, 61.1, 51.3, 42.6, 40.5, 33, 36.6, 29.9, 25.9, 13.5, 7.8)

# Sin logaritmo
plot(volumen, presion)

B11 <- cov(volumen,presion)/var(volumen)

B01 <- mean(presion) - B11*mean(volumen)
abline(a = B01, b = B11, col = "red", lwd = 4)

# con logaritmo
volumenLog <- log(volumen)
presionLog <- log(presion)
plot(volumenLog, presionLog)

B12 <- cov(volumenLog,presionLog)/var(volumenLog)

B02 <- mean(presionLog) - B12*mean(volumenLog)
abline(a = B02, b = B12, col = "red", lwd = 4)

presion_sombreroLog <- B02 + B12*volumenLog
S2 <- sum((presionLog-presion_sombreroLog)**2)/(length(volumenLog)-2)
Se_B1 <- sqrt(S2/sum((volumenLog-mean(volumenLog))**2))


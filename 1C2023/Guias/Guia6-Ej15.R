a1 <- c(82.5, 83.7, 80.9, 95.2, 80.8)
a2 <- c(82.7, 81.9, 78.9, 83.6, 78.6)
a3 <- c(92.2, 106.8, 104.6, 94.5, 100.7)
a4 <- c(96.5, 93.8, 92.1, 87.4, 89.6)
a5 <- c(88.9, 89.2, 94.2, 91.4, 90.1)
a6 <- c(75.6, 78.7, 92.2, 87.2, 83.8)

# ------------------ inciso a
X <- cbind(a1, a2, a3, a4, a5, a6) # matriz de datos

apply(X, 2, function(i) sum(2*i))

#SCE
promedio <- mean(X)
SCE <- sum(apply(X, 2, function(i) length(i)*(mean(i)-promedio)^2))

#SCD
SCDParciales <- apply(X, 2, function(i) (i-mean(i))^2)

SCD <- sum(SCDParciales)

SCTOTAL <- SCD + SCE

Fobs <- (SCE/ (5-1)) / (SCD / (30-6))

# ------------------ inciso b
lista_datos <- list(a1, a2, a3, a4, a5, a6)

# Calcular las diferencias con respecto a la media utilizando apply
diferencias <- lapply(lista_datos, function(x) mean(x) - x)


# ------------------ inciso c
residuosa1 <- mean(a1) - a1
residuosa2 <- mean(a2) - a2
residuosa3 <- mean(a3) - a3
residuosa4 <- mean(a4) - a4
residuosa5 <- mean(a5) - a5
residuosa6 <- mean(a6) - a6

# Combinar los residuos en un solo vector
residuos <- c(residuosa1, residuosa2, residuosa3, residuosa4, residuosa5, residuosa6)

# Crear QQ-plot normal
qqnorm(residuos, pch = 16, col='pink3')
qqline(residuos)  # Agregar una línea de referencia

# Etiquetas y título
mtext(xlabel, side = 1, line = 3)
mtext(ylabel, side = 2, line = 3)

# ------------------ inciso d 
# Tukey con correccion de Bonferroni

unodos <- (mean(a1) - mean(a2))/sqrt(SCD*(2/5))
unotres <- (mean(a1) - mean(a3))/sqrt(SCD*(2/5))
unocuatro <- (mean(a1) - mean(a4))/sqrt(SCD*(2/5))
unocinco <- (mean(a1) - mean(a5))/sqrt(SCD*(2/5))
dostres <- (mean(a2) - mean(a3))/sqrt(SCD*(2/5))
doscuatro <- (mean(a2) - mean(a4))/sqrt(SCD*(2/5))
doscinco <- (mean(a2) - mean(a5))/sqrt(SCD*(2/5))
trescuatro <- (mean(a3) - mean(a4))/sqrt(SCD*(2/5))
trescinco <- (mean(a3) - mean(a5))/sqrt(SCD*(2/5))
cuatrocinco <- (mean(a4) - mean(a5))/sqrt(SCD*(2/5))
unoseis <- (mean(a1) - mean(a6))/sqrt(SCD*(2/5))
dosseis <- (mean(a2) - mean(a6))/sqrt(SCD*(2/5))
tresseis <- (mean(a3) - mean(a6))/sqrt(SCD*(2/5))
cuatroseis <- (mean(a4) - mean(a6))/sqrt(SCD*(2/5))
cincoseis <- (mean(a5) - mean(a6))/sqrt(SCD*(2/5))
locales <- read.delim("~/Desktop/Estadistica aplicada/1C2023/Guias/datasets/locales.txt")

View(locales)

# Estimamos los parámetros:
x <- locales$superficie
y <- locales$ventas

B1 <- cov(x,y)/var(x)
# Por cada unidad de ventas, el metro cuadrado aumenta en 85,28 

B0 <- mean(y) - B1*mean(x)

y_sombrero <- B0 + B1*x
s2 <- sum((y-y_sombrero)**2)/(length(x)-2)

se <- sqrt(s2/sum((x-mean(x))**2))

plot(x,y,pch=20)
abline(a = B0, b = B1, col = "red", lwd = 4)

modelo <- lm(locales$superficie~locales$ventas, data=locales)

plot(rstandard(modelo), y_sombrero)

# inciso e 

y0_sombrero <- B0 + B1*5
h0 <- ((5-mean(x))**2)/sum((x-mean(x))**2)

tObs <- B1 / se
n <- length(y)
pValor <- 1 - pt(tObs, n-2)

prediccionSe = sqrt(s2*((1/n)+h0+1))
t05 <- qt(0.05, n-2)

y0.9 <- y0_sombrero + t05 * prediccionSe
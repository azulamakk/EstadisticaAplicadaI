salarios <- read.csv("~/Desktop/Estadistica aplicada/1C2023/Guias/datasets/salarios.txt")

x <- salarios$productividad
y <- salarios$salarios

plot(x, y)

B1 <- cov(x,y)/var(x)

B0 <- mean(y) - B1*mean(x)

y_sombrero <- B0 + B1*x
s2 <- sum((y-y_sombrero)**2)/(length(x)-2)

se <- sqrt(s2/sum((x-mean(x))**2))

LI <- B1 + qt(0.05, 30) * se
LS <- B1 + qt(0.95, 30) * se

modelo <- lm(y ~x)
plot(x, rstandard(modelo))

# Reescribir variable 
YPrima <- y[2:length(y)] - y[1:(length(y)-1)]
XPrima <- x[2:length(x)] - x[1:(length(x)-1)]
print(YPrima)

B11 <- cov(XPrima,YPrima)/var(XPrima)

B00 <- mean(YPrima) - B11*mean(XPrima)

y_sombreroPrim <- B00 + B11*XPrima
s2 <- sum((YPrima-y_sombreroPrim)**2)/(length(XPrima)-2)

se <- sqrt(s2/sum((XPrima-mean(XPrima))**2))

LI1 <- B11 + qt(0.05, 29) * se
LS1 <- B11 + qt(0.95, 29) * se

modelo2 <- lm(YPrima ~ XPrima)
plot(XPrima, rstandard(modelo2))

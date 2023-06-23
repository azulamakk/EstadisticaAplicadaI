electricidad <- read.csv("~/Desktop/Estadistica aplicada/1C2023/Guias/datasets/electricidad.txt")

consumo <- electricidad$consumo
potencia <- electricidad$potmax

plot(x,y)

x <- log(consumo)
y <- log(potencia)

B1 <- cov(x,y)/var(x)
B0 <- mean(y) - B1*mean(x)

n=length(x)
p=2
y_sombrero <- B0 + B1*x

S2 <- sum((y-y_sombrero)**2)/(n-p)

Se_B1 <- sqrt(S2/sum((y-mean(y))**2))

LI <- B1 + qt(0.05, 98) * Se_B1
LS <- B1 + qt(0.95, 98) * Se_B1


Y0 <- B0+B1*100

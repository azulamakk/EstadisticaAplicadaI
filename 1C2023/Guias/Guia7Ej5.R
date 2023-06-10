dosarn <- c(421, 412, 443, 347)
seisarn <- c(526, 581, 560, 570)
nuevearn <- c(630, 560, 590, 672)

x <- c(2,2,2,2,6,6,6,6,9,9,9,9)
y <- c(421, 412, 443, 347, 526, 581, 560, 570, 630, 560, 590, 672)

plot(x,y)

B1 <- cov(x,y)/var(x)
mean(x)
B0 <- mean(y) - B1*mean(x)

n=length(x)
p=2
y_sombrero <- B0 + B1*x

S2 <- sum((y-y_sombrero)**2)/(n-p)

Se_B1 <- sqrt(S2/sum((y-mean(y))**2))

# ---------- inciso c

x0 = 6
leverage <- (x0 - mean(x)) / sum((x-mean(x))**2)
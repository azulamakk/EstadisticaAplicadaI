# Ejercicio 1
# Obtener un IC para q de nivel 0.95
x <- c(2850,3315,2799,3043,2635,2715)

alfa <- 0.05
gl <- length(x)
a <- mean(x) - qt(1-alfa/2, gl) * sd(x) /sqrt(6) 
b <- mean(x) + qt(1-alfa/2, gl) * sd(x) /sqrt(6) 
round(c(a,b),2)

#Error
0.5 * (b/a) / mean(x) * 100

# ¿Como encuentro n para que E=130,778?
n <- 6:20
E<- qt(1-alfa/2,n-1)*sd(x)/sqrt(n)
plot(6:20,E, pch-20)
abline(h-130.778)
# A medida que aumenta n el error va disminuyendo

# Ejercicio 2
y <- c(315,329,514,336,403,299,329,602,368,321)

quantile(y, 0.5, type=1)

set.seed(3)
boot <- replicate(1000, (
  unname(quantile(sample(y, 10, replace=TRUE), 0.5, type=1))
))

boot

library(beeswarm)
beeswarm::beeswarm(boot, horizontal=TRUE,side=1,
                   pch=20, ylim=c(0,5000), spacing=0.1)

# Metodo 1
quantile(boot, 0.025, type=1)
quantile(boot, 0.975, type=1)

# Metodo 2
m <- quantile(y, 0.5, type=1)
m + qnorm(0.975) * sd(boot)
m - qnorm(0.975) * sd(boot)



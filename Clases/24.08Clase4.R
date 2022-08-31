

x <- rnorm(10,8,0.4)
sum((x > 8.3)|(x < 7.7))

#graficamos la funcion de probabilidad de la binomial
plot(0:10, dbinom(0:10, 10, 0.225), type = 'l')

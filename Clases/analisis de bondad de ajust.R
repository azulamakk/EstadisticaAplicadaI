market = read.csv('acciones.csv')

x = market$JPMorgan_Bank

hist(x, freq = FALSE)


# LOG VEROSIMILITUD

# Para la normal

t = (x-mean(x))/sd(x)

# log verosimilitud de la Normal    -- NO verosimilitud (son diferentes)
sum(dnorm(t), log = TRUE)

# Para la t de Student
sum(dt(t, 2, log = TRUE))

# Grafico
hist(t, freq = FALSE, xlim = c(-4, 4))
curve(dnorm(x), add = TRUE, col = 'firebrick', lwd = 4)
curve(dt(x, 2), add = TRUE, col = 'steelblue', lwd = 4)



# AJUSTE DE CUANTILES -- QQ Plot

# calculamos la funcion empirica
qempiricos = sort(t)
n = length(x)
femp = 1:n/(n+1)

# Calculo de cuantiles teoricos con la Normal(0,1)
qteoricos_N01 = qnorm(femp)

# Calculo de cuantiles teoricos con la t de Student
qteoricos_t = qt(femp, 2)

# Grafico
par(mfrow = c(2, 1), mar = c(2,2,2,2))
plot(qempiricos, qteoricos_N01, pch = 20, col = 'maroon4')
abline(a = 0, b = 1, ldw = 3, lty = 3)
abline(a = 0, b = 0)
abline(v = 0)
plot(qempiricos, qteoricos_t, pch = 20, col = 'maroon4')
abline(a = 0, b = 1, ldw = 3, lty = 3)
abline(a = 0, b = 0)
abline(v = 0)



# INFERENCIA SOBRE LOS MOMENTOS
# Hay algunos momentos con momentos fijos (E(X), Var(X), curtosis, asimetria)
# Calculo intervalo de confianza empirico del momento y veo si el fijo del modelo esta adentro

# CONTRASTE CHI-CUADRADO
# Agarro datos en forma de clases
# Definir frecuancia absoluta de cada clase
# Estimar la probabilidad de que x pertenezca a esa clase con el modelo elegido
# Fei    esperanza del numero que deberia haber en la frecuencia absoluta si X sigue al modelo
# ESTO ESTA EN EL VIDEO QUE HABIA QUE VER PARA LA CLASE


# HAY SCRIPT PARA CONTRASTE CHI-CUADRADO
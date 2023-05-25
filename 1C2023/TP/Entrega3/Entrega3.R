winequality.red <- read.csv("~/Desktop/Estadistica aplicada/1C2023/TP/winequality-red.csv")

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggbeeswarm")
#install.packages("gridExtra")
#install.packages("reshape2")
library("reshape2")
library("gridExtra")
library("ggbeeswarm")
library('tidyverse')
library('dplyr')
library("corrplot")

#-------------------------Parte 1: Analisis descriptivo
#-------------------------Limpieza de datos
set.seed(13)
DS2 <- winequality.red %>% filter(winequality.red$quality==4 | winequality.red$quality==6 | winequality.red$quality==8)
DS2 <- DS2[sample(nrow(DS2), 500, replace = FALSE, prob = NULL),]
DS2 <- DS2 %>% select(c(fixed.acidity, density, alcohol, pH, quality))

#-------------------------Análisis descriptivo de los datos

# Antes de comenzar con el analisis de las variables, 
# mostramos como desarrollamos la función empírica para la ejecución de este tipo de gráficos.
#empirica <- function(x, a) {
#  return (sum(x<=a)/length(x))}

#1.1. ----- fixed.acidity
x1 <- DS2$fixed.acidity

# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo_x1 <- min(x1)
maximo_x1 <- max(x1)
rango_x1 <- maximo_x1 - minimo_x1

# Función de distribución empírica = Función de probabilidad acumulada
f.distribucion.empirica <- function(datita, variable, colorete, ejex, titulo){
  ggplot(datita, aes({{variable}})) + 
    stat_ecdf(geom = "point", 
              color= colorete) +
    labs(title = titulo,
         y = "Probabilidad Acumulada", 
         x = ejex) +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey')) 
}

f.distribucion.empirica(DS2, fixed.acidity,"#00688B", "Valores de Acidez Fija", "Función de distribución empírica para Acidez Fija")

#Histograma - estima la función de densidad - Continuas
histograma <- function(datita, variable, color.bordes, color.fill, ejex, titulo, media){
  ggplot(DS2, aes(variable)) + 
    geom_histogram(color=color.bordes,
                   fill = color.fill,
                   bins = 15) +
    labs(title=titulo,
         y = "Frecuencia", 
         x=ejex) +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey'))+
    geom_vline(xintercept = media)
}

histograma(DS2, DS2$fixed.acidity, "#104E8B", "#00688B", "Valores de Acidez Fija", "Histograma para la Acidez Fija", mean(x1))

#Gráfico de densidad - Continuas 
densidad <- function(datita, variable, colorete, ejex, titulo){
  ggplot(DS2, aes(variable)) + 
    geom_density(color=colorete, size = 2) +
    labs(title=titulo,
         y = "Densidad", 
         x= ejex) +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey')) 
}

densidad(DS2, DS2$fixed.acidity,"#00688B", "Valores de Acidez Fija", "Función de Densidad para la Acidez Fija")

#Media y mediana
media_x1 <- mean(x1)
mediana_x1 <- median(x1)
# Como la media es mayor a la mediana entonces la distribución tiene asimetria positiva

#Cuantiles y Rango intercuantil (RIC)
quantile(x1, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
beeswarm_boxplot <- function(datita, variable, colorete, ejey, titulo){
  ggplot(DS2, aes(y = {{variable}}, x = frequency({{variable}}))) + 
    geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = colorete) +
    labs(title=titulo,
         y = ejey, 
         x= "") +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey'),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    geom_boxplot(color="black", 
                 fill = colorete,
                 alpha = 0.7)
}

beeswarm_boxplot(DS2, DS2$fixed.acidity, "#00688B", "Valores de Acidez Fija", "Beeswarm y Boxplot para Acidez Fija")

#Desvío estándar
varianza_x1 <- sum((x1 - media_x1)^2)/500
desvio_estandar_x1 <- sqrt(varianza_x1)

#Coeficiente variación
coef_vari_x1 <- desvio_estandar_x1/media_x1

#Coeficiente de asimetría
coef_asim_x1 <- sum(((x1 - media_x1)/desvio_estandar_x1)^3)/500

#Coeficiente de kurtosis
coef_kurtosis_x1 <- sum(((x1 - media_x1)/desvio_estandar_x1)^4)/500


#1.2. ----- density
x2 <- DS2$density

# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo_x2 <- min(x2)
maximo_x2 <- max(x2)
rango_x2 <- maximo_x2 - minimo_x2

# Función de distribución empírica = Función de probabilidad acumulada
f.distribucion.empirica(DS2, density,"#698B22", "Valores de Densidad", "Función de distribución empírica para Densidad")

#Histograma - estima la función de densidad - Continuas
histograma(DS2, DS2$density, "#556B2F", "#698B22", "Valores de Densidad", "Histograma para la Densidad", mean(x2))

#Gráfico de densidad - Continuas 
densidad(DS2, DS2$density,"#698B22", "Valores de Densidad", "Función de Densidad para la Densidad del vino")

#Gráficos de densidad sin outliers y con
extraer_outliers = function(x){
  sup = quantile(x,0.75, na.rm = T)+IQR(x, na.rm = T)*1.5 #bigote de la derecha
  inf = quantile(x,0.25, na.rm = T)-IQR(x, na.rm = T)*1.5 #bigote de la izquierda
  outliers <- na.omit(x[(x > sup) | (x<inf)])
  return(outliers)
}
outliers_x2 <- extraer_outliers(DS2$density)
chau_outliers <- DS2[!(DS2$density %in% outliers_x2),]

ggplot(NULL, aes(density)) + 
  geom_density(data = chau_outliers, color= "#C0FF3E", size = 2) +
  labs(title="Función de Densidad para la Densidad del vino",
       y = "Densidad", 
       x="Valores de Densidad del vino",
       subtitle = "(Verde oscuro = con outliers, Verde claro = sin outliers)") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) +
  geom_density(data = DS2, color = "#698B22", size = 2)
#En verde claro --> la distribución sin outliers
#En verde oscuro --> la distribución con outliers

#Media y mediana
media_x2 <- mean(x2)
mediana_x2 <- median(x2)
# Como la media y la mediana son iguales entonces la distribución es simetrica.

#Cuantiles y Rango intercuantil (RIC)
quantile(x2, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
beeswarm_boxplot(DS2, DS2$density, "#698B22", "Valores de Densidad", "Beeswarm y Boxplot para la Densidad")

#Desvío estándar
varianza_x2 <- sum((x2 - media_x2)^2)/500
desvio_estandar_x2 <- sqrt(varianza_x2)

#Coeficiente variación
coef_vari_x2 <- desvio_estandar_x2/media_x2

#Coeficiente de asimetría
coef_asim_x2 <- sum(((x2 - media_x2)/desvio_estandar_x2)^3)/500

#Coeficiente de kurtosis
coef_kurtosis_x2 <- sum(((x2 - media_x2)/desvio_estandar_x2)^4)/500


#1.3. ----- alcohol
x3 <- DS2$alcohol

# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo_x3 <- min(x3)
maximo_x3 <- max(x3)
rango_x3 <- maximo_x3 - minimo_x3

# Función de distribución empírica = Función de probabilidad acumulada
f.distribucion.empirica(DS2, alcohol,"hotpink3", "Valores de Alcohol", "Función de distribución empírica para Alcohol")

#Histograma - estima la función de densidad - Continuas
histograma(DS2, DS2$alcohol, "hotpink4", "hotpink3", "Valores de Alcohol", "Histograma para el Alcohol", mean(x3))

#Gráfico de densidad - Continuas 
densidad(DS2, DS2$alcohol,"hotpink3", "Valores de Densidad", "Función de Densidad para el Alcohol")

#Media y mediana
media_x3 <- mean(x3)
mediana_x3 <- median(x3)
# Como la media y la mediana son iguales entonces la distribución es simetrica.

#Cuantiles y Rango intercuantil (RIC)
quantile(x3, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
beeswarm_boxplot(DS2, DS2$alcohol, "hotpink3", "Valores de Alcohol", "Beeswarm y Boxplot para el Alcohol")

#Desvío estándar
varianza_x3 <- sum((x3 - media_x3)^2)/500
desvio_estandar_x3 <- sqrt(varianza_x3)

#Coeficiente variación
coef_vari_x3 <- desvio_estandar_x3/media_x3

#Coeficiente de asimetría
coef_asim_x3 <- sum(((x3 - media_x3)/desvio_estandar_x3)^3)/500

#Coeficiente de kurtosis
coef_kurtosis_x3 <- sum(((x3 - media_x3)/desvio_estandar_x3)^4)/500


#1.4. ----- pH
x4 <- DS2$pH

# El rango de los datos es el maximo menos el mínimo de la muestra.
minimo_x4 <- min(x4)
maximo_x4 <- max(x4)
rango_x4 <- maximo_x4 - minimo_x4

# Función de distribución empírica = Función de probabilidad acumulada
f.distribucion.empirica(DS2, pH,"orange", "Valores de pH", "Función de distribución empírica para pH")

#Histograma - estima la función de densidad - Continuas
histograma(DS2, DS2$pH, "orange3", "orange", "Valores de pH", "Histograma para el pH", mean(x4))

#Gráfico de densidad - Continuas
densidad(DS2, DS2$pH,"orange", "Valores de Densidad", "Función de Densidad para el pH")

#Media y mediana
media_x4 <- mean(x4)
mediana_x4 <- median(x4)
#Como la media y la mediana son iguales entonces la distribución es simetrica.

#Cuantiles y Rango intercuantil (RIC)
quantile(x4, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
beeswarm_boxplot(DS2, DS2$pH, "orange", "Valores de pH", "Beeswarm y Boxplot para el pH")

#Desvío estándar
varianza_x4 <- sum((x4 - media_x4)^2)/500
desvio_estandar_x4 <- sqrt(varianza_x4)

#Coeficiente variación
coef_vari_x4 <- desvio_estandar_x4/media_x4

#Coeficiente de asimetría
coef_asim_x4 <- sum(((x4 - media_x4)/desvio_estandar_x4)^3)/500

#Coeficiente de kurtosis
coef_kurtosis_x4 <- sum(((x4 - media_x4)/desvio_estandar_x4)^4)/500


#-------------------------Parte 2: Relacion entre variables

# Funcion de relación entre variables
relacion <- function(variable.x, variable.y, ejex, ejey, titulo, colorete){
  ggplot(DS2, aes(x = {{variable.x}}, y = {{variable.y}})) + 
    geom_point(color=colorete, size = 2) +
    labs(title=titulo,
         y = ejey, 
         x=ejex) +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey')) 
  
}

# Relacion entre Acidez Fija y ph
r1 <- relacion(DS2$fixed.acidity, DS2$pH, "Valores de Acidez Fija", "Valores de pH", "Relación entre Acidez Fija y pH", "#FF4040")

# Relacion entre Alcohol y Densidad
r2 <-relacion(DS2$density, DS2$alcohol, "Valores de Densidad", "Valores de Alcohol", "Relación entre Densidad y Alcohol", "#E066FF")

# Relacion entre Alcohol y pH
r3 <-relacion(DS2$alcohol, DS2$pH, "Valores de Alcohol", "Valores de pH", "Relación entre Alcohol y pH", "#63B8FF")

# Relacion entre Densidad y ph
r4<- relacion(DS2$density, DS2$pH, "Valores de Densidad", "Valores de pH", "Relación entre Densidad y pH", "#CDCD00")

#Las 4 relaciones lindas
grid.arrange(r1, r2, r3, r4, ncol=2)


col_numericas <- DS2[,-5] #almacenamos las columnas numéricas en un dataframe
matriz_correlacion <- cor(col_numericas) #creamos la matriz de correlación
det(matriz_correlacion)
# Como el determinante de la matriz da cercano a cero, significa que las variables tienen mucha correlación entre sí. 

#Haciendo el gráfico de las correlaciones
cormelt <- melt(matriz_correlacion)
ggplot(cormelt, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  labs(title= "Diagrama de correlaciones entre variables") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) + 
  coord_fixed()

#-------------------------Parte 3: Analisis variable categórica

# pH en Calidad Baja 
histograma2 <- function(calidad, color.bordes, color.fill, titulo){
  DS2 %>% filter(DS2$quality==calidad) %>% ggplot(DS2, mapping = aes(pH)) +
    geom_histogram(color=color.bordes,
                   fill = color.fill,
                   bins = 10,
                   alpha = 0.5) +
    labs(title=titulo,
         y = "Frecuencia", 
         x= "Valores del pH") +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey')) 
}

histograma2(4,"orange3", "orange", "Histograma para el pH en Calidad Baja")

# pH en Calidad Media
histograma2(6,"plum4", "plum3", "Histograma para el pH en Calidad Media")

# pH en Calidad Alta
histograma2(8,"royalblue3", "royalblue4", "Histograma para el pH en Calidad Alta")

# Todos los histogramas superpuestos
ggplot(DS2,aes(x=pH)) + 
  geom_histogram(data=subset(DS2 %>% filter(DS2$quality==6)),
                 color="plum4",
                 fill = "plum3",
                 bins = 10,
                 alpha = 0.5) +
  geom_histogram(data=subset(DS2 %>% filter(DS2$quality==4)),
                 color="orange3",
                 fill = "orange",
                 alpha = 0.7,
                 bins = 10) +
  geom_histogram(data=subset(DS2 %>% filter(DS2$quality==8)),
                 color="royalblue3",
                 fill = "royalblue4",
                 bins = 10,
                 alpha = 0.4) +
  labs(title="Histogramas Superpuestos",
       y = "Frecuencia", 
       x="Valores del pH",
       subtitle = "(Amarillo = Calidad Baja, Violeta = Calidad Media, Azul = Calidad Alta)")+
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))  

# Densidades del pH segun la categoría superpuestas

DS2 %>%
  ggplot(mapping = aes(pH, color = factor(quality))) +
  geom_density(aes(linetype = factor(quality)), size = 1.5) +
  scale_linetype_manual(values = rep("solid", nlevels(factor(DS2$quality)))) +
  scale_color_manual(values = c("orange", "plum3", "royalblue4")) +
  labs(title = "Densidades de pH para distintas calidades de vino",
       y = "Densidad",
       x = "Valores del pH",
       color = "Calidad") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) +
  guides(color = FALSE, linetype = FALSE)

#-------------------------Parte 3: Estimación Puntual
datos <- DS2 %>% filter(DS2$quality==6) %>% pull(pH)

#q11 = Cuantil 0.9 con metodo no paramétrico
estimador_11 <- function(x){
  return(quantile(x, 0.9, type = 1))
}
estimador_11(datos)

error_estandar <- function(x, estimador_fun){
  error <- sd(replicate(1000,{
    x_nuevo <- sample(x,500, replace = TRUE)
    estimador_fun(x_nuevo)
  })) 
  return(error)
}
error_estandar(datos, estimador_11)

# q12 = Cuantil 0.9 con metodo de momentos con distribucion normal
estimador_12 <- function(x){
  return(qnorm(0.9, mean(x), sqrt(var(x)), log.p = FALSE))
}
estimador_12(datos)
error_estandar(datos, estimador_12)

# q21 = Probabilidad de que x > 3.4 con metodo de momentos con distribucion normal 
estimador_21 <- function(x){
  return(1-pnorm(3.4, mean = mean(x), sd = sd(x)))
}
estimador_21(datos)
error_estandar(datos, estimador_21)

# q22 = probabilidad de que x > 3.4 con maxima verosimilitud con distribucion log-normal
estimador_22 <- function(x){
  m_mv = sum(log(x))/100 
  D2_mv = sqrt(sum(log(x)-m_mv)^2)/100
  return(1-pnorm(log(3.4), mean = m_mv, sd = D2_mv))
}
estimador_22(datos)
error_estandar(datos, estimador_22)

#-------------------------Parte 4: Bondad de Ajuste

# ---- a) Cálculo de log-verosimilitud 
# Para la Normal
desvio = sd(x4) #calculo los parametros para estandarizar los datos
media = mean(x4)
Xest = (x4-media)/desvio #estandarizo los datos
logverosimilitudnormal <- sum(dnorm(Xest), log = TRUE) #suma del logaritmo de la función de densidad
logverosimilitudnormal

# Para la Gamma
alpha = (mean(x4)/sd(x4))^2
beta = (sd(x4))^2/mean(x4)
logverosimilitudgamma <- sum(dgamma(x4, shape = alpha, scale = beta, log = TRUE))
logverosimilitudgamma


# ---- b) QQ-Plots
# ---- Distribucion Gamma
cuantilesTeoricos <- qgamma(ppoints(length(x4)), shape = alfa, scale = beta)

# Creamos un data frame con los datos
df1 <- data.frame(cuantilesTeoricos1 = cuantilesTeoricos, dataEmpirica = x4)

# Crear el respectivo QQ plot utilizando ggplot2 para distribucion Gamma
grafico1 <- ggplot(df1, aes(sample = dataEmpirica)) +
  geom_qq(distribution = qgamma, dparams = list(shape = alfa, scale = beta),pch=20, col='maroon4') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
  labs(title = "QQ Plot - Distribución Gamma", x='Valores de ph', y='')+
  theme_bw() +
  ggtitle("QQ Plot - Distribucion Gamma") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))  

# ---- Distribucion Normal
df2 <- data.frame(datos = x4)

grafico2 <- ggplot(df2, aes(sample = datos)) + # Creamos el QQ plot utilizando ggplot2
  stat_qq(distribution = qnorm, dparams = list(mean = media_x4, sd = desvio_estandar_x4),pch=20, col='maroon4') +
  stat_qq_line(distribution = qnorm, dparams = list(mean = media_x4, sd = desvio_estandar_x4), linetype = 'dashed', size = 1) +
  theme_bw() +labs(title = "QQ Plot - Distribucion Normal", x='Valores de ph', y='')+
  ggtitle("QQ Plot - Distribución Normal") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))  

grid.arrange(grafico1, grafico2, ncol=1)

# ---- c) Distribución empírica con las distribuciones

f.distribucion.empirica(DS2, x4,"maroon4", "Valores de pH", "Función de distribución empírica para pH")

histograma(DS2, x4, "#104E8B", "maroon4", "Valores de pH", "Histograma para el pH", mean(x4))

# ---- d) Comparación del histograma con la densidad ajustada por cada modelo

densidad(DS2, x4,"#00688B", "Valores de pH", "Función de Densidad para el pH")

curve(dnorm(x, mean = 0, sd = 1, log = F))
curve(dgamma(x4,alpha,beta),xlim=c(0,20),yaxs="i",ylim=c(0,0.15),ylab="f(x)",
      main='Densidad gamma(2,3)') 
box()

#-------------------------Parte 5: Intervalos de Confianza
# Intervalo de confianza para cuantil0.9
cuant=c()
cuant=replicate(1000,{
  xboot=sample(DS2$pH,500,replace=TRUE)
  q = quantile(xboot, 0.9, type = 1)
})

cuantilBoot = mean(cuant)
sigmaBoot1 = sd(cuant)

A1 = cuantilBoot - qnorm(0.95, mean = 0, sd=1) * sigmaBoot1

B1 = cuantilBoot + qnorm(0.95, mean = 0, sd=1) * sigmaBoot1

# Intervalo de confianza para P>3.4
proba=c()
proba=replicate(1000,{
  xboot=sample(DS2$pH,500,replace=TRUE)
  prop=(1-pnorm(3.4, mean = mean(xboot), sd = sd(xboot)))
})

probaBoot = mean(proba)
sigmaBoot2 = sd(proba)

A2 = probaBoot - qnorm(0.95, mean = 0, sd=1) * sigmaBoot2

B2 = probaBoot + qnorm(0.95, mean = 0, sd=1) * sigmaBoot2
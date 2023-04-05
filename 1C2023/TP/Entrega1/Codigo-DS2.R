winequality.red <- read.csv("~/Desktop/Estadistica aplicada/1C2023/TP/Entrega1/winequality-red.csv")

# install.packages("tidyverse")
# install.packages("dplyr")
#install.packages("ggbeeswarm")
library("ggbeeswarm")
library('tidyverse')
library('dplyr')

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

# Sin ggplot, de la forma más teórica
#fEmpirica1 = c()
#for(i in 1: length(x1)){
#  fEmpirica1[i] <- empirica(x1, x1[i])}
#plot(x1, fEmpirica1, main = "Funcion de distribución empírica para Acidez fija",
#     xlab = "Valores de acidez",
#     ylab = "Probabilidad acumulada",
#     col = "blue3")

# Con ggplot 
ggplot(DS2, aes(fixed.acidity)) + 
  stat_ecdf(geom = "point", 
            color="#00688B") +
  labs(title="Función de distribución empírica para Acidez Fija",
       y = "Probabilidad Acumulada", 
       x="Valores de Acidez") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Histograma - estima la función de densidad - Continuas
ggplot(DS2, aes(fixed.acidity)) + 
  geom_histogram(color="#104E8B",
                 fill = "#00688B",
                 bins = 15) +
  labs(title="Histograma para la Acidez Fija",
       y = "Frecuencia", 
       x="Valores de Acidez Fija") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Gráfico de densidad - Continuas 
ggplot(DS2, aes(fixed.acidity)) + 
  geom_density(color="#00688B", size = 2) +
  labs(title="Función de Densidad para la Acidez Fija",
       y = "Densidad", 
       x="Valores de Acidez Fija") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Media y mediana
media_x1 <- mean(x1)
mediana_x1 <- median(x1)
# Como la media es mayor a la mediana entonces la distribución tiene asimetria positiva

#Cuantiles y Rango intercuantil (RIC)
quantile(x1, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
ggplot(DS2, aes(y = fixed.acidity, x = frequency(fixed.acidity))) + 
  geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = "#00688B") +
  labs(title="Beeswarm y Boxplot para Acidez Fija",
       y = "Valores de Acidez Fija", 
       x= "") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(color="black", 
               fill = "#00688B",
               alpha = 0.7)

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

# Sin ggplot, de la forma más teórica
#fEmpirica2 = c()
#for(i in 1: length(x2)){
#  fEmpirica2[i] <- empirica(x2, x2[i])}
#plot(x2, fEmpirica2, main = "Funcion de distribución empírica para Densidad",
#     xlab = "Valores de densidad",
#     ylab = "Probabilidad acumulada",
#     col= "green4")

# Con ggplot
ggplot(DS2, aes(density)) + 
  stat_ecdf(geom = "point", 
            color="#698B22") +
  labs(title="Función de distribución empírica para Densidad",
       y = "Probabilidad Acumulada", 
       x="Valores de Densidad") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 


#Histograma - estima la función de densidad - Continuas
ggplot(DS2, aes(density)) + 
  geom_histogram(color="#556B2F",
                 fill = "#698B22",
                 bins = 15) +
  labs(title="Histograma para la Densidad",
       y = "Frecuencia", 
       x="Valores de Densidad") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 


#Gráfico de densidad - Continuas 
ggplot(DS2, aes(density)) + 
  geom_density(color="#698B22", size = 2) +
  labs(title="Función de Densidad para la Densidad del vino",
       y = "Densidad", 
       x="Valores de Densidad del vino") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

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
ggplot(DS2, aes(y = density, x = frequency(density))) + 
  geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = "#698B22") +
  labs(title="Beeswarm y Boxplot para la Densidad",
       y = "Valores de la Densidad", 
       x= "") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(color="black", 
               fill = "#698B22",
               alpha = 0.7)

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

# Sin ggplot, de la forma más teórica
#fEmpirica3 = c()
#for(i in 1: length(x3)){
#  fEmpirica3[i] <- empirica(x3, x3[i])}
#plot(x3, fEmpirica3, main = "Funcion de distribución empírica para el Alcohol",
#     xlab = "Valores de densidad",
#     ylab = "Probabilidad acumulada",
#     col= "red3")

# Con ggplot 
ggplot(DS2, aes(alcohol)) + 
  stat_ecdf(geom = "point", 
            color="hotpink3",) +
  labs(title="Función de distribución empírica para Alcohol",
       y = "Probabilidad Acumulada", 
       x="Valores de Alcohol") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Histograma - estima la función de densidad - Continuas
ggplot(DS2, aes(alcohol)) + 
  geom_histogram(color="hotpink4",
                 fill = "hotpink3",
                 bins = 15) +
  labs(title="Histograma para el Alcohol",
       y = "Frecuencia", 
       x="Valores de Alcohol") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) +
  geom_vline(xintercept = mean(x3))

#Gráfico de densidad - Continuas 
ggplot(DS2, aes(alcohol)) + 
  geom_density(color="hotpink3", size = 2) +
  labs(title="Función de Densidad para el Alcohol",
       y = "Densidad", 
       x="Valores de Alcohol") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Media y mediana
media_x3 <- mean(x3)
mediana_x3 <- median(x3)
# Como la media y la mediana son iguales entonces la distribución es simetrica.

#Cuantiles y Rango intercuantil (RIC)
quantile(x3, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
ggplot(DS2, aes(y = alcohol, x = frequency(alcohol))) + 
  geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = "hotpink3") +
  labs(title="Beeswarm y Boxplot para el Alcohol",
       y = "Valores de la Alcohol", 
       x= "") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(color="black", 
               fill = "hotpink3",
               alpha = 0.7)

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

# Sin ggplot, de la forma más teórica
#fEmpirica4 = c()
#for(i in 1: length(x4)){
#  fEmpirica4[i] <- empirica(x4, x4[i])}
#plot(x4, fEmpirica4, main = "Funcion de distribución empírica para el pH",
#     xlab = "Valores de pH",
#     ylab = "Probabilidad acumulada",
#     col= "orange")

# Con ggplot 
ggplot(DS2, aes(pH)) + 
  stat_ecdf(geom = "point", 
            color="orange",) +
  labs(title="Función de distribución empírica para pH",
       y = "Probabilidad Acumulada", 
       x="Valores del pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

#Histograma - estima la función de densidad - Continuas
ggplot(DS2, aes(pH)) + 
  geom_histogram(color="orange3",
                 fill = "orange",
                 bins = 15) +
  labs(title="Histograma para el pH",
       y = "Frecuencia", 
       x="Valores del pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) +
  geom_vline(xintercept = mean(x4))

#Gráfico de densidad - Continuas 
ggplot(DS2, aes(pH)) + 
  geom_density(color="orange", size = 2) +
  labs(title="Función de Densidad para el pH",
       y = "Densidad", 
       x="Valores de pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 


#Media y mediana
media_x4 <- mean(x4)
mediana_x4 <- median(x4)
#Como la media y la mediana son iguales entonces la distribución es simetrica.

#Cuantiles y Rango intercuantil (RIC)
quantile(x4, c(0.25,0.5,0.75))

#Beeswarm y Boxplot
ggplot(DS2, aes(y = pH, x = frequency(pH))) + 
  geom_beeswarm(aes(), size = 1,cex = 4.5, stroke = 0.2, col = "orange") +
  labs(title="Beeswarm y Boxplot para el pH",
       y = "Valores de pH", 
       x= "") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot(color="black", 
               fill = "orange",
               alpha = 0.7)

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

# Relacion entre acidez fija y ph
ggplot(DS2, aes(x = fixed.acidity, y = pH)) + 
  geom_point(color="#CD3700", size = 2) +
  labs(title="Relación entre Acidez Fija y pH",
       y = "Valores de pH", 
       x="Valores de Acidez") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

# Relacion entre alcohol y densidad
ggplot(DS2, aes(x = density, y = alcohol)) + 
  geom_point(color="#CD3700", size = 2) +
  labs(title="Relación entre Densidad y Alcohol",
       y = "Valores de Alcohol", 
       x="Valores de Densidad") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

# Relacion entre Alcohol y pH
ggplot(DS2, aes(x = alcohol, y = pH)) + 
  geom_point(color="#CD3700", size = 2) +
  labs(title="Relación entre Alcohol y pH",
       y = "Valores de pH", 
       x="Valores de Alcohol") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))

# Relacion entre densidad y ph
ggplot(DS2, aes(x = density, y = pH)) + 
  geom_point(color="#CD3700", size = 2) +
  labs(title="Relación entre Densidad y pH",
       y = "Valores de pH", 
       x="Valores de Densidad") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))

col_numericas <- DS2[,-5] #almacenamos las columnas numéricas en un dataframe
matriz_correlacion <- cor(col_numericas) #creamos la matriz de correlación
det(matriz_correlacion)
# Como el determinante de la matriz da cercano a cero, significa que las variables tienen mucha correlación entre sí. 


#-------------------------Parte 3: Analisis variable categórica

# pH en Calidad Baja 
hist1 <- DS2 %>% filter(DS2$quality==4) %>% ggplot(DS2, mapping = aes(pH)) +
  geom_histogram(color="orange3",
                 fill = "orange",
                 alpha = 0.5,
                 bins = 10) +
  labs(title="Histograma para el pH en Calidad Baja",
       y = "Frecuencia", 
       x="Valores del pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 

# pH en Calidad Media
hist2 <- DS2 %>% filter(DS2$quality==6) %>% ggplot(DS2, mapping = aes(pH)) +
  geom_histogram(color="plum4",
                 fill = "plum3",
                 bins = 10,
                 alpha = 0.5) +
  labs(title="Histograma para el pH en Calidad Media",
       y = "Frecuencia", 
       x="Valores del pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey')) 


# pH en Calidad Alta
hist3 <- DS2 %>% filter(DS2$quality==8) %>% ggplot(DS2, mapping = aes(pH)) +
  geom_histogram(color="royalblue3",
                 fill = "royalblue4",
                 bins = 10,
                 alpha = 0.5) +
  labs(title="Histograma para el pH en Calidad Alta",
       y = "Frecuencia", 
       x="Valores del pH") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))  

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


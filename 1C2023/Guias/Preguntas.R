## Regresion

# cargue el dataset Wine subido al campus

base=read_csv('wine.csv')
view(base)

# 1) Haga un listado de las variables que componen el dataset

summary(base)
#Las variables que componen el dataset son:
#Wine- Alcohol- Malic.acid- Ash- Acl- Mg- Phenols- Flavanoids- Nonflavanoid.phenols-Proanth- Color.int- Hue- OD- Proline      

# 2) indique cual de ellas son (numericas o categoricas)

#Todas las variables son numericas menos wine que es categorica

# 3) Realice una regresion de la cantidad de alcohol en las varaibles

#Realizo una regresion lineal sacando la variable categorica

dim(base)
auxiliar= base[2:15]
lineal =glm(Alcohol~.,data=auxiliar)
lineal

#    Malic.acid (nivel de acidez), Acl (concentracion de Alcalil),
#    Color.int (intensidad del color) y Proline (acido organico)

lm.fit1 =lm(Alcohol~Malic.acid+Acl+Color.int+Proline,data= base )

# 3-a) Explique de que manera estas variables explican el contenido de alcohol
lm.fit1
summary(lm.fit1)

#La variable Malic.acid explica el 3,28% de la variacion del acohol, se relaciona de manera directa con ella y cuando el Malic.acid cambia en una unidad, el Alcohol cambia en 0.102448 unidades
#si el resto de las variables se mantienen constantes
#La variable Acl explica el 1,39% de la variacion del acohol, se relaciona de manera indirecta con ella y cuando el Acl cambia en una unidad, el Alcohol cambia en -0.033704 unidades
#si el resto de las variables se mantienen constantes
#La variable Color.int explica el 1,96% de la variacion del acohol, se relaciona de manera directa con ella y cuando el Color.int cambia en una unidad, el Alcohol cambia en 0.12494 unidades
#si el resto de las variables se mantienen constantes
#La variable Proline explica el 0,01% de la variacion del acohol, se relaciona de manera directa con ella y cuando el Proline cambia en una unidad, el Alcohol cambia en 0.001281 unidades
#si el resto de las variables se mantienen constantes
#La variable que mas explica al alcohol es el color.int, ya que es la que mayor coeficiente tiene, por lo tanto es la que mas incide en ella.

# 4) La funcion siguiente tiene la varaible "color.int" al cuadrado
#    Explique el significado de esta variable, cual seria el nivel de color que
#    maximice o minimice, segun sea el caso, al nivel de acohol 

lm.fit2 =lm(Alcohol~Malic.acid+Acl+Color.int+Proline+I(Color.int ^2),data= base )
summary(lm.fit2)

#Como la variable color.int está al cuadrado y su coeficiente es negativo, no importa que valor de color tenga, siempre hará que el alcohol
#disminuya o se mantenga. A medida que el color se intensifica (va aumentando su valor), el alcohol disminuirá cada vez más.
#El punto en el que se maximiza el alcohol es en Color.int=0, ya que es aquí el unico momento en donde la variable no causara que 
#el alcohol disminuya. 
#Este analisis se realizo teniendo en cuenta que las variables no toman valores negativos.


# 5) La funcion siguiente tiene una no linealidad en (Color.int*Malic.acid) 
#    Explique el significado de esta variable, como se interpreta 

lm.fit3 =lm(Alcohol~Malic.acid+Acl+Color.int+Proline+Color.int*Malic.acid+I(Color.int ^2),data= base )
summary(lm.fit3)

#las variables Color.int y Malic.acid tienen coeficientes positivos y la variable que multiplica a ambas tiene coeficiente negativo.
#Esto significa que mientras mayor sea Color.int y mayor sea Malic.acid, la nueva variable más hará disminuir el alcohol. Igualmente,
#es necesario aclarar que por mas de que la nueva variable haga disminuir el alcohol, es más lo que este aumenta debido a Malic.acid
#y Color.int por separado ya que sus coeficientes son mayores. En conlusión, siempre será más lo que aumente el alcohol por estas dos
#variables a lo que pueda disminuir con la multiplicacion de ellas. 
#Este analisis se realizo teniendo en cuenta que las variables no toman valores negativos.


# 6) Explique las diferencias existentes ente los distintos tipos de vinos
#    utilizando una regresion logistica 
#                wine en funcion de Malic.acid, Acl, Proline

#    Esta particion, es estadisticamente significativa, que significa esto?

logit = multinom(Wine ~ Malic.acid + Acl + Proline, data=base)
logit
summary(logit)

#por una unidad de aumento en Malic.acid, el tipo de vino 2 en relacion con el 1 disminuirá en 1,19. En otras palabras, si Malic.Acid aumenta en una unidad, 
#las posibilidades de que sea un vino de tipo 1 son mas altas a que sea un tipo de vino 2.
#Mientras que por una unidad de aumento de Malic.acid el tipo de vino 3 en relacion con el 1 aumentará un 0.0877, por lo que probablemente se seguirá
#considerando un vino de tipo 3, antes que de tipo 1. 
#Este analisis puede repetirse para las dos variables restantes.
#Que la particion sea estadisticamente significativa significa que las variables seleccionadas logran explicar lo suficiente sobre los tipos
#de vinos segun el nivel de confianza que se haya tenido en cuenta.

# 7) Realice un analisis de componentes pricipales
#     7.a) Defina la cantidad optima de componentes ppales

#al ser variables numericas utilizo PCA
pca =prcomp (base , scale =TRUE) 
summary(pca)
screeplot(pca, type = "l", npcs = 20, main = "Screeplot of the first 10 PCs")

#La cantidad optima de componentes principales es de 3

#     7.b) Explique el criterio

#Para tomar la decision primero observé la desviacion estandar de los componentes. Hasta el componente 3 la desviacion es mayor a 1, por lo que
#vale la pena incorporarlos al analisis, pero ya en el componente 4 no es mayor a 1. Tambien observe el grafico de los primeros 20 
#PCs, donde se puede analizar como  hasta el componente 3, la linea continua disminuyendo de manera significativa, pero a partir del
#componente 4 la linea comienza a disminuir de manera sumamente lenta.


#     7.c) Cuanto explica el primer componente ppal?, 

#el primer componente principal explica un 39,54% de la variabilidad de los datos

#     7.d) Cuantos componentes ppales debe reunir para explicar 
#          el 85% de la variabilidad conjunta

#para reunir el 85% de la variabilidad conjunta debo reunir los 6 primeros componentes principales. Esto se puede analizar observando
#el "Cumulative proportion" el cual va sumando las desviaciones acumuladas.

# 8) ###KMEAN
#Con la base de datos Wine:
  # 8a) Haga un resumen estadistico para todo el conjunto

summary(base)
describe(base)
cor(base)
#haciendo el summary y el describe de la base podemos observar informacion estadistica de la base como por ejemplo minimos, 
#maximos, media, mediana, cuartiles, etc. 
#Tambien realize un analisis de correlacion entre variables para observar como se comportan entre ellas


  # 8b) Determine la cantidad de agrupamientos optimos, ?que m?todo utiliz?? ?por qu??
K.max=10
wss=c()

for(i in 1:K.max){
  wineclasses= kmeans(base,i,nstart = 15)
  wss[i] = wineclasses$tot.withinss   
}
wss

plot(1:K.max, wss, 
     type="b",pch = 12,  
     xlab= "Number of clusters",
     ylab="within ss/ total ss")

#La cantidad de agrupamientos optimos es 4, ya que se puede observar en el grafico como hasta este valor, cada cluster crea
#un conjunto nuevo significativamente mas coersivo. A partir del 4 en adelante, la creacion de nuevos grupos no hace una diferencia 
#significativa.

#agrego los clusters a la base de datos
km =kmeans (base,4)
base$cluster=km$cluster
base

# 8c) Realice un examen estadistico por grupo, comparela con la del conjunto. 
#     Compare el resultado de los grupos con los distintos tipos de vinos y con la poblacion general

#para comparar los grupos elegi las variables Alcohol, Malic.acid, Color.int, Acl Y Proline

#comparacion de medias
subset = base %>% group_by(cluster) %>% summarise(
  mean(Alcohol), mean(Malic.acid) , mean(Color.int), mean(Acl), mean(Proline)
) 
subset

#El segundo grupo de vinos es el que posee menos alcohol, menos intensidad de color y menos Proline, pero es el que posee mas Acl.
#Se podria decir que el primer y tercer grupo se mantienen en los intermedios en todas las variables sin destacarse ni por los maximos ni minimos.
#El cuarto grupo es el que mas alcohol, Proline y intensidad del color tiene, pero tambien es el que menor Malic.acid y menor Acl tiene.
#En comparacion con la poblacion general, vemos como el promedio general de alcohol es de 13, por lo que el primer y cuarto grupo de vinos lo 
#superan y el segundo y tercero no logran superarlo. 
#En cuanto al Color.int, la media es de 5.058, en este caso tres promedios de los grupos se encuantran por encima del valor y el grupo 2 es el
#unico que no logra superarlo. 


#comparacion de medianas
subset2 = base %>% group_by(cluster) %>% summarise(
  median(Alcohol), median(Malic.acid) , median(Color.int), median(Acl), median(Proline)
) 
subset2

#Podemos realizar un analisis similar al anterior, pero en este caso hablando sobre las medianas de los grupos.
#algo que sobresale del analisis es la enorme diferencia entre las medianas de los grupos de la variable proline.
#Comparandola con la mediana de Proline de la poblacion general que es 673.5, concluyo que los grupos de vinos 1 y 4
#la superan enormemente.



#comparacion de desvios
subset3 = base %>% group_by(cluster) %>% summarise(
  sd(Alcohol), sd(Malic.acid) , sd(Color.int), sd(Acl), sd(Proline)
) 

subset3

#Tambien realicé un analsis comparando los desvios de las variables segun sus grupos.

#Asi como realizé estos 3 analisis, podria realizar los mismos para otros estadisticos tales como maximos, minimos, varianzas, etc.


df = read_csv('dataMCA.csv')
##MCA
##  9 Los datos corresponden a una encuesta que hizo un concesionario:
##   a) ?Se encuentra, en general, satisfecho, con el servicio?
##   b) ?Su problema fue resuelto?
##   c) Califique del 1 al 5 que tan buena fue la bienvenida
##   d) ?Es buena la relacion precio/calidad?
##   e) ?Volveria a usar nuestros servicios?

view(df)

#Realice un an?lisis de MCA que incluya 
#a) Interpretaci?n general del primer y segundo componente
#b) ?Cuanto del primer componente se explica por la relacion percio/calidad?
#c) ?Cuanto del segundo componente se explica por la Bienvenida?

#Para el correcto analisis primero debo quitar la variable customer y pasar la variable welcome a factor.

df$Welcome=as.factor(df$Welcome)
nuevabase=df[2:6]
nuevabase


mca = MCA(nuevabase, graph = T)
summary(mca)

fviz_mca_var(mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

#El primer componente explica el 26.968% de la variabilidad y el segundo explica el 16,22%. Juntos explican el 43,19%. 
#en el grafico de la respresentacion de las variables se puede observar como el precio explica mucho del primer componente
#y casi nada del segundo, lo mismo pasa con la satisfaccion. En cambio el "come back", welcome y repaired explican bastante a ambos componentes. 
#Esto tambien se puede analizar el el summary(mca) en "categorical variables".
#En el grafico de variables categoricas podemos analizar como si a una persona le parecio buena la relacion precio/calidad, es muy
#probable que vuelva y que este satisfecha. Por otro lado si no le parecio buena la relacion precio/calidad, lo mas probable en que no
#este satisfecha.

nuevabase2=cbind(df,mca$svd$U[,1:2] %>% as_tibble()) #agrega a la base los dos primeros componentes principales

explv1 = lm(V1~ `Q/Price`, nuevabase2) %>% summary()
explv1

#La relacion precio/calidad explica un 64,7% del primer componente

explv2 = lm(V2~ Welcome , nuevabase2) %>% summary()
explv2

#La bienvenida explica un 38,31% del segundo componente
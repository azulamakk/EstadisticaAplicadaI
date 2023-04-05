setwd("C:/Users/nacho/OneDrive/Escritorio/ITBA/3er año/Estadistica/TP entreg 1")
data=read.csv('DS10.csv')
data=data$prices.amountMax
x=as.numeric(data)
set.seed(66)
y=sample(x,300)




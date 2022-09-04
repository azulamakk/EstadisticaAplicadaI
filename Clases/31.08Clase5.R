
# Simulacion de duracion de 100 heladeras
replicate(1000,(
  sum(rweibull(50,3,50))
))

quantile(x,0.2,type=1)

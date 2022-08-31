
# Simulacion de duracion de 100 heladeras
replicate(1000,(
  sum(rweibull(100,1.5,150))
))

quantile(x,0.2,type=1)

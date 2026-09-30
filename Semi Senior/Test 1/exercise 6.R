library(dplyr)
library(actuar)
library(tidyverse)
library(lifecontingencies)


#  Tabla incorporada basada en la ley de Makeham --------------------------

data("soa08Act", package = "lifecontingencies")
tabla_mortalidad <- soa08Act


# Parámetros del producto -------------------------------------------------

edad <- 40
plazo <- 20
suma_asegurada <- 100000000
tasas <- c(0.03,0.04,0.05,0.06)
#v <- 1/(1+i)

# Seguro temporal, anualidad anticipada y prima neta ----------------------
# m = 0 el periodo empieza a cubrir de inmediato
# k = 1 el beneficio se paga al final del año del fallecimiento (frecuencia anual) 

resultados_prima <- do.call(
  rbind,
  lapply(tasas,function(tasa){
    valor_seguro <- Axn(
      actuarialtable = tabla_mortalidad,
      x = edad,
      n = plazo,
      i = tasa, 
      m = 0,
      k = 1,
    )

valor_anualidad <- axn(
  actuarialtable = tabla_mortalidad,
  x = edad,
  n = plazo,
  i = tasa,
  m = 0,
  k = 1,
  payment = "advance"
)

prima_neta <- suma_asegurada*valor_seguro/valor_anualidad

data.frame(
  tasa = tasa,
  Axn = valor_seguro,
  axn = valor_anualidad,
  prima_neta_anual = prima_neta
)
})
)
resultados_prima

# Sensibilidad de la prima frente a la tasa de interés --------------------

plot(
  resultados_prima$tasa * 100,
  resultados_prima$prima_neta_anual,
  type = "b",
  pch = 19,
  col = "steelblue",
  xlab = "Tasa de interés técnico (%)",
  ylab = "Prima neta anual (COP)",
  main = "Sensibilidad de la prima neta anual"
)
grid()

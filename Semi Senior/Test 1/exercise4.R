library(fitdistrplus)
library(actuar)
library(dplyr)
siniestros <- c(
  120, 450, 820, 230, 1200, 340, 95, 2100, 510, 670,
  180, 340, 890, 1500, 290, 440, 760, 1100, 380, 620
)


# Ajuste por máxima verosimilitud -----------------------------------------

ajuste_lognormal <- fitdist(siniestros,"lnorm")

ajuste_pareto <- fitdist(
  siniestros,
  "pareto",
  start = list(shape=1, scale=min(siniestros)*0.95),
  lower = c(1e-8,1e-8),
  upper = c(Inf,min(siniestros))
)

ajuste_gamma <- fitdist(siniestros,"gamma")

# Estadístico KS para comparar los ajustes --------------------------------

ks_stat <- function(x,p_acumulada){
  x <- sort(x)
  n <- length(x)
  Fx <- p_acumulada(x)
  max(c(seq_len(n) / n - Fx, Fx - (seq_len(n) - 1) / n))
}
# Que tan bien se ajusta a una lognormal
ks_lognormal <- ks_stat(
  siniestros,
  function(x) plnorm(
    x,
    meanlog = ajuste_lognormal$estimate["meanlog"],
    sdlog = ajuste_lognormal$estimate["sdlog"]
    
  )
)

ks_pareto <- ks_stat(
  siniestros,
  function(x) ppareto(
    x,
    shape = ajuste_pareto$estimate["shape"],
    scale = ajuste_pareto$estimate["scale"]
  )
)

ks_gamma <- ks_stat(
  siniestros,
  function(x) pgamma(
    x,
    shape = ajuste_gamma$estimate["shape"],
    rate = ajuste_gamma$estimate["rate"]
  )
)

n <- length(siniestros)

tabla_ajustes <- data.frame(
  model = c("Lognormal","Pareto","Gamma"),
  AIC = c(
    ajuste_lognormal$aic,
    ajuste_pareto$aic,
    ajuste_gamma$aic
  ),
  BIC = c(
    -2*ajuste_lognormal$loglik + length(ajuste_lognormal$estimate)*log(n),
    -2*ajuste_pareto$loglik + length(ajuste_pareto$estimate)*log(n),
    -2*ajuste_gamma$loglik + length(ajuste_pareto$estimate)*log(n)
  ),
  KS = c(ks_lognormal,ks_pareto,ks_gamma)
  
) %>% 
  arrange(AIC)
tabla_ajustes


# Selección automática del modelo con menor AIC ---------------------------

mejor_modelo <- tabla_ajustes[1,1]
cdf_mejor <- switch(
  mejor_modelo,
  Lognormal = function(x) plnorm(
    x,
    meanlog = ajuste_lognormal$estimate["meanlog"],
    sdlog = ajuste_lognormal$estimate["sdlog"]
  ),
  Pareto = function(x) ppareto(
    x,
    shape = ajuste_pareto$estimate["shape"],
    scale = ajuste_pareto$estimate["scale"]
  ),
  Gamma = function(x) pgamma(
    x,
    shape = ajuste_gamma$estimate["shape"],
    rate = ajuste_gamma$estimate["rate"]
  )
)

# E[min((X - 500)+, 500)] = integral entre 500 y 1000 de S(x) dx ----------

perdida_esperada_capa <- integrate(
  function(x) 1 - cdf_mejor(x),
  lower = 500,
  upper = 1000
)$value
mejor_modelo
perdida_esperada_capa

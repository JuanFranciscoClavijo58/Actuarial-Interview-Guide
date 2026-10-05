library(tidyr)
library(tibble)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
# Calculo Loss Ratio ------------------------------------------------------

datos <- tibble(
  grupo = rep(c("A", "B", "C", "D", "E"), each = 4),
  anio = rep(2021:2024, times = 5),
  prima = c(
    850000, 920000, 980000, 1020000,
    2100000, 2300000, 2250000, 2400000,
    430000, 480000, 510000, 490000,
    1650000, 1700000, 1750000, 1800000,
    320000, 350000, 380000, 400000
  ),
  siniestros = c(
    510000, 470000, 630000, 560000,
    1470000, 1380000, 1620000, 1680000,
    258000, 240000, 357000, 294000,
    990000, 1020000, 1225000, 1170000,
    224000, 210000, 304000, 320000
  )
) %>%
  mutate(loss_ratio = siniestros / prima)

# Agrupar por grupo  LR promedio y ponderado por prima ---------------------------

datos %>%
  select(grupo, anio, loss_ratio) %>%
  print(n = Inf)

resumen <- datos %>%
  group_by(grupo) %>%
  summarise(
    lr_promedio = mean(loss_ratio),
    lr_ponderado = sum(siniestros) / sum(prima),
    desviacion_estandar = sd(loss_ratio),
    .groups = "drop"
  )
resumen %>%
  mutate(across(where(is.numeric), ~ round(.x, 5)))

# Grupos con mayor variabilidad y mayor LR ponderado ----------------------

resumen %>% slice_max(desviacion_estandar,n=1)
resumen %>% arrange((desc(desviacion_estandar)))
resumen %>% slice_max(lr_ponderado,n=1)
resumen %>% arrange(desc(lr_ponderado))

#  Numero de grupos y observaciones por grupo -----------------------------

datos
r <- n_distinct(datos$grupo)
n <- datos %>% count(grupo) %>% pull(n) %>% unique()


# Medias y varianzas dentro de cada grupo ---------------------------------
estadisticos <- datos %>% 
  group_by(grupo) %>% 
  summarise(
    x_bar = mean(loss_ratio),
    s2 = var(loss_ratio),
    .groups = "drop"
  )

# Media global ------------------------------------------------------------

mu_hat <- mean(estadisticos$x_bar)


# Varianza del proceso ----------------------------------------------------

v_hat <- datos %>% 
  group_by(grupo) %>% 
  summarise(
    suma_cuadrados = sum((loss_ratio - mean(loss_ratio))^2),
    .groups = "drop"
  ) %>% 
  summarise(v = sum(suma_cuadrados)/ (r*(n-1))) %>% 
  pull(v)

# Varianza de las medias hipoteticas y cociente k -------------------------

a_hat <- sum((estadisticos$x_bar - mu_hat)^2) / (r - 1) - v_hat / n
k_hat <- v_hat / a_hat


# Factor de credibilidad para cuatro anos de experiencia ------------------

Z <- n/(n+k_hat)

tibble(
  mu_hat = mu_hat,
  v_hat = v_hat,
  a_hat = a_hat,
  k_hat = k_hat,
  Z = Z
) %>% 
  mutate(across(everything(),~ round(.x, 6)))

# prima de credibilidad  --------------------------------------------------

Primas_credibilidad <- estadisticos %>% 
  transmute(
    grupo,
    lr_propio = x_bar,
    mu_global = mu_hat,
    Primas_credibilidad = Z*lr_propio + (1-Z)* mu_global,
    Z = Z,
    efecto = case_when(
      Primas_credibilidad > lr_propio ~ "Sube",
      Primas_credibilidad < lr_propio ~ "Baja",
      T~ "Sin cambio"
    ),
    credibilidad_plena = lr_propio,
    credibilidad_nula = mu_global
  )
Primas_credibilidad %>% 
  mutate(across(where(is.numeric),~round(.x,6)))



# Buhlmann–Straub ---------------------------------------------------------

datos_bs <- datos %>% 
  arrange(grupo,anio) %>% 
  mutate(loss_ratio = siniestros/prima)

grupos_bs <- sort(unique(datos_bs$grupo))
anios_bs <- sort(unique(datos$anio))

numero_grupos <- length(grupos_bs)
numero_anios <- length(anios_bs)

stopifnot(
  numero_grupos > 1,
  numero_anios >1,
  !anyNA(datos_bs[c("grupo","anio","prima","siniestros")]),
  !anyDuplicated(datos_bs[c("grupo","anio")]),
  nrow(datos_bs) == numero_grupos*numero_anios,
  all(is.finite(datos_bs$loss_ratio)),
  all(is.finite(datos_bs$prima)),
  all(datos_bs$prima > 0)
)


X <- matrix(
  datos_bs$loss_ratio,
  nrow = numero_grupos,
  ncol = numero_anios,
  byrow = TRUE,
  dimnames = list(grupos_bs, anios_bs)
)
w <- matrix(
  datos_bs$prima / 1e6,
  nrow = numero_grupos,
  ncol = numero_anios,
  byrow = TRUE,
  dimnames = dimnames(X)
)

exposicion_total <- rowSums(w)
lr_ponderado <- rowSums(w * X) / exposicion_total
exposicion_cartera <- sum(exposicion_total)
#mu_hat
media_exposicion <- sum(exposicion_total * lr_ponderado) /  exposicion_cartera

residuos_bs <- sweep(X, 1, lr_ponderado, FUN = "-")
#s^2_hat
s2_bs <- sum(w * residuos_bs^2) /
  (numero_grupos * (numero_anios - 1))

denominador_bs <- exposicion_cartera -  sum(exposicion_total^2) / exposicion_cartera

a_momentos <- (
  sum(exposicion_total * (lr_ponderado - media_exposicion)^2) -
    (numero_grupos - 1) * s2_bs) / denominador_bs
#a_hat 
a_bs <- max(0, a_momentos)

# con mu_bs Se garantiza que el estimador de la media colectiva sea el 
# de máxima verosimilitud y mínima varianza dentro de la estructura del modelo

if (a_bs == 0) {
  k_bs <- Inf
  factores_bs <- rep(0, numero_grupos)
  mu_bs <- media_exposicion
} else {
  k_bs <- s2_bs / a_bs
  factores_bs <- exposicion_total / (exposicion_total + k_bs)
  mu_bs <- sum(factores_bs * lr_ponderado) / sum(factores_bs)
}

primas_bs <- factores_bs * lr_ponderado +  (1 - factores_bs) * mu_bs
parametros_bs <- tibble(
  mu = mu_bs,
  s2 = s2_bs,
  a_momentos = a_momentos,
  a = a_bs,
  k = k_bs
)

resultados_bs <- tibble(
  grupo = rownames(X),
  exposicion = exposicion_total,
  lr_ponderado = lr_ponderado,
  Z_bs = factores_bs,
  prima_bs = primas_bs
)

medias_clasicas <- rowMeans(X)
mu_clasica <- mean(medias_clasicas)
v_clasica <- mean(apply(X, 1, var))
a_clasica <- max(0, var(medias_clasicas) - v_clasica / numero_anios)
Z_clasica <- if (a_clasica == 0) 0 else
  numero_anios / (numero_anios + (v_clasica / a_clasica))

comparacion <- resultados_bs %>%
  mutate(
    prima_clasica = Z_clasica * medias_clasicas +
      (1 - Z_clasica) * mu_clasica,
    diferencia = prima_bs - prima_clasica,
    diferencia_absoluta = abs(diferencia)
  ) %>% print()

datos_grafico <- datos %>% 
  mutate(loss_ratio = siniestros/ prima ) %>% 
  group_by(grupo) %>% 
  summarise(
    n_anios =n(),
    lr_propio = mean(loss_ratio),
    varianza_lr = var(loss_ratio),
    .groups = "drop"
  )
stopifnot(
  nrow(datos_grafico) > 1,
  all(datos_grafico$n_anios > 1),
  length(unique(datos_grafico$n_anios)) == 1,
  all(is.finite(datos_grafico$lr_propio)),
  all(is.finite(datos_grafico$varianza_lr))
)

n_grafico <- unique(datos_grafico$n_anios)
mu_grafico <- mean(datos_grafico$lr_propio)
v_grafico <- mean(datos_grafico$varianza_lr)
a_grafico <- max(0,var(datos_grafico$lr_propio)- (v_grafico/n_grafico))

if (a_grafico == 0) {
  Z_grafico <- 0
} else {
  k_grafico <- v_grafico / a_grafico
  Z_grafico <- n_grafico / (n_grafico + k_grafico)
}

datos_grafico <- datos_grafico %>% 
  mutate(
    Z = Z_grafico,
    prima_credibilidad = Z*lr_propio +(1-Z)*mu_grafico,
    etiqueta = sprintf("%s (Z = %.3f)",grupo,Z)
  )
limites <- range(c(datos_grafico$lr_propio,
                 datos_grafico$primas_credibilidad, mu_grafico)) + c(-0.02,0.02)
grafico_credibilidad <- ggplot(
  datos_grafico,
  aes(x = lr_propio, y = prima_credibilidad)
) +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed", colour = "grey50"
  ) +
  geom_hline(
    yintercept = mu_grafico,
    linetype = "dotted", colour = "firebrick"
  ) +
  geom_point(size = 3, colour = "steelblue") +
  geom_text(aes(label = etiqueta), nudge_y = 0.006, size = 3) +
  scale_x_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  coord_fixed(ratio = 1, xlim = limites, ylim = limites) +
  labs(
    title = "Efecto de credibilidad - regresión a la media (Z = n/(n+k))",
    subtitle = sprintf(
      "Diagonal: Z = 1 | Horizontal: Z = 0, media = %.2f%%",
      100 * mu_grafico
    ),
    x = "LR propio del grupo",
    y = "Prima de credibilidad"
  ) +
  theme_minimal(base_size = 12) 

print(grafico_credibilidad)

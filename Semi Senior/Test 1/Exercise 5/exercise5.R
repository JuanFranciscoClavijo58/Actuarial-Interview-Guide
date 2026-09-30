library(readr)
library(tidyr)
library(tidyverse)
# Data --------------------------------------------------------------------

datos_ap <- data.frame(
  edad = factor(
    c(
      "18-30", "18-30", "31-45", "31-45",
      "46-60", "18-30", "46-60", "31-45",
      "60+", "60+", "46-60", "60+"
    ),
    levels = c("18-30", "31-45", "46-60", "60+")
  ),
  actividad = factor(
    c(
      "alto", "bajo", "bajo", "alto", "bajo", "alto",
      "alto", "bajo", "alto", "bajo", "alto", "alto"
    ),
    levels = c("alto", "bajo")
  ),
  exposicion = c(1, 0.5, 1, 2, 1, 0.25, 1.5, 1, 3, 0.75, 2, 1),
  n_siniestros = c(3, 0, 1, 5, 2, 0, 4, 1, 7, 2, 6, 3)
)
summary(datos_ap)

# GLM Poisson Offset ------------------------------------------------------

modelo_poisson <- glm(
  n_siniestros ~ edad + actividad + offset(log(exposicion)),
  family = poisson(link = "log"),
  data = datos_ap
)
summary(modelo_poisson)

# Coeficientes y relatividades tarifarias ---------------------------------

coeficientes <- data.frame(
  termino = names(coef(modelo_poisson)),
  beta = unname(coef(modelo_poisson)),
  relatividad = unname(exp(coef(modelo_poisson)))
)

tasa_base <- exp(coef(modelo_poisson)["(Intercept)"])
# relatividad : Es el factor multiplicador de riesgo para cada 
#             grupo en comparación con el grupo base.
# tasa: Es el riesgo final calculado para cada rango de edad,
#       asumiendo que mantienen el nivel de actividad base.
tabla_edad <- data.frame(
  variable = "edad",
  nivel = c("18-30", "31-45", "46-60", "60+"),
  relatividad = c(
    1,
    exp(coef(modelo_poisson)["edad31-45"]),
    exp(coef(modelo_poisson)["edad46-60"]),
    exp(coef(modelo_poisson)["edad60+"])
  )
) %>%
  mutate(tasa = tasa_base * relatividad)


tabla_actividad <- data.frame(
  variable = "actividad",
  nivel = c("alto", "bajo"),
  relatividad = c(
    1,
    exp(coef(modelo_poisson)["actividadbajo"])
  )
) %>%
  mutate(tasa = tasa_base * relatividad)

tabla_tarifaria <- bind_rows(tabla_edad, tabla_actividad)
rownames(tabla_tarifaria) <- NULL
coeficientes
tabla_tarifaria

# Predicción: 55 años, actividad alta y exposición de un año --------------

nuevo_asegurado <- data.frame(
  edad = factor("46-60", levels = levels(datos_ap$edad)),
  actividad = factor("alto", levels = levels(datos_ap$actividad)),
  exposicion = 1
)

frecuencia_esperada <- predict(
  modelo_poisson,
  newdata = nuevo_asegurado,
  type = "response"
)
frecuencia_esperada

# Bondad de ajuste y prueba de sobredispersión ----------------------------

deviance_residual <- deviance(modelo_poisson)
grados_libertad <- df.residual(modelo_poisson)
p_deviance <- pchisq(
  deviance_residual,
  df = grados_libertad,
  lower.tail = FALSE
)

chi_pearson <- sum(residuals(modelo_poisson,type = "pearson")^2)
dispersion <- chi_pearson/grados_libertad
p_sobredispersion <- pchisq(
  chi_pearson,
  df= grados_libertad,
  lower.tail = FALSE
)

frecuencia_esperada
c(
  deviance = deviance_residual,
  gl = grados_libertad,
  p_deviance = p_deviance,
  dispersion = dispersion,
  p_sobredispersion = p_sobredispersion
)
#No hay evidencia para rechazar que tu dispersión sea 1", descartando la sobredispersión.
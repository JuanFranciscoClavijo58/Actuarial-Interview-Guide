library(ChainLadder)
library(dplyr)

# as.triangle -------------------------------------------------------------
matriz <- matrix(
  c(
    1200, 1560, 1680, 1740, 1760,
    1350, 1755, 1890, 1958, NA,
    1500, 1950, 2100, NA, NA,
    1650, 2145, NA, NA, NA,
    1800, NA, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(
    origen = 2020:2024,
    desarrollo = paste0("Dev_", 1:5)
  )
)
triangulo <- as.triangle(matriz)

# Modelo mack  ------------------------------------------------------------
modelo_mack <- MackChainLadder(
  triangulo,
  est.sigma = "Mack"
)

# Factores de desarrollo ata ----------------------------------------------
ldf <- modelo_mack$f
ldf
# IBNR y error estándar por año de origen ####
resumen_mack <- summary(modelo_mack)$ByOrigin
resultado_origen <- data.frame(
  año_origen = as.integer(rownames(resumen_mack)), 
  IBNR       = as.numeric(resumen_mack$IBNR), 
  Mack_SE    = as.numeric(resumen_mack$Mack.S.E)
) %>% 
  mutate( 
    limite_inferior_95 = pmax(0, IBNR - 1.96 * Mack_SE), 
    limite_superior_95 = IBNR + 1.96 * Mack_SE, 
    CV = if_else(IBNR > 0, Mack_SE / IBNR, NA_real_) 
  )
resultado_origen
# IBNR total e intervalo de confianza aproximado al 95 % ------------------
ibnr_total <- sum(summary(modelo_mack)$ByOrigin$IBNR)
se_total <- modelo_mack$Total.Mack.S.E
intervalo_total <- c(
  inferior = max(0, ibnr_total - 1.96 * se_total),
  estimacion = ibnr_total,
  superior = ibnr_total + 1.96 * se_total
)

# Año con mayor incertidumbre relativa ------------------------------------
mayor_incertidumbre <- resultado_origen %>%
  filter(!is.na(CV)) %>%
  slice_max(CV, n = 1, with_ties = FALSE)
resultado_origen
intervalo_total
mayor_incertidumbre


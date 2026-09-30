library(dplyr)

polizas <- read.csv("C:/Users/User/Desktop/Archivos/polizas.txt", sep="|")
siniestros <- read.csv("C:/Users/User/Desktop/Archivos/siniestros.txt", sep="|")

siniestros_por_poliza <- siniestros %>%
  group_by(id_poliza) %>%
  summarise(
    n_siniestros = n(),
    monto_siniestros = sum(monto_sinistro),
    .groups = "drop"
  )
resumen_segmento <- polizas %>%
  left_join(siniestros_por_poliza, by = "id_poliza") %>%
  mutate(
    n_siniestros = coalesce(n_siniestros, 0L),
    monto_siniestros = coalesce(monto_siniestros, 0)
  ) %>%
  group_by(segmento) %>%
  summarise(
    n_polizas = n_distinct(id_poliza),
    exposicion_total = sum(exposicion),
    n_siniestros = sum(n_siniestros),
    monto_total = sum(monto_siniestros),
    frecuencia = n_siniestros / n_polizas,
    severidad_media = monto_total / n_siniestros,
    prima_pura = frecuencia * severidad_media,
    prima_total = n_polizas * 800000,
    loss_ratio = monto_total / prima_total,
    .groups = "drop"
  )
resumen_segmento

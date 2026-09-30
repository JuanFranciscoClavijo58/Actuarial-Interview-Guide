library(readr)
library(dplyr)
library(ChainLadder)
library(tidyr)

pagos_siniestros <- read_delim("C:/Users/User/Desktop/Archivos/Exercise 2/pagos_siniestros.txt", 
                               delim = "|", escape_double = FALSE, trim_ws = TRUE)

triangulo_df <- pagos_siniestros %>% 
            mutate(año_desarrollo = año_pago - año_ocurrencia + 1) %>% 
            group_by(año_ocurrencia,año_desarrollo) %>% 
            summarise(
              pago_incremental = sum(monto_pago),
              .groups = "drop"
            ) %>% 
            group_by(año_ocurrencia) %>% 
            arrange(año_desarrollo, .by_group = TRUE) %>% 
            mutate(
              pago_acumulado = cumsum(pago_incremental)
            ) %>% 
            ungroup() %>% 
            select(año_ocurrencia,año_desarrollo,pago_acumulado) %>% 
            pivot_wider(
              names_from = año_desarrollo,
              values_from = pago_acumulado
            ) %>% 
            arrange(año_ocurrencia)

matriz_triangulo <- as.matrix(triangulo_df[,-1])
rownames(matriz_triangulo) <- triangulo_df$año_ocurrencia
triangulo_chainladder <- as.triangle(matriz_triangulo)


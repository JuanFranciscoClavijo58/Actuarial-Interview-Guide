library(tidyverse)
library(readxl)

bases_ejercicio <- read_excel("C:/Users/User/Downloads/bases_ejercicio.xlsx")
polizas   <- read_excel("C:/Users/User/Downloads/bases_ejercicio.xlsx", sheet = "POLIZAS")
siniestros <- read_excel("C:/Users/User/Downloads/bases_ejercicio.xlsx", sheet = "SINIESTROS")
glimpse(polizas)
glimpse(siniestros)

Partea<-polizas %>% 
        mutate(FECEXP = as.Date(FECEXP)) %>% 
        filter(year(FECEXP) == 2020) %>% 
        group_by(TIPORIESGO) %>% 
        summarise(numero_polizas= n())
print(Partea)

Parteb <- polizas %>% 
          mutate(FECIVIG = as.Date(FECIVIG),
                 FECVEN = as.Date(FECVEN)) %>% 
          filter(FECIVIG < as.Date("2020-03-25") & FECVEN>= as.Date("2020-03-25")) %>% 
          summarise(numero_polizas = n())
print(Parteb)          

SINIESTROS2 <- siniestros %>% 
          left_join(
            polizas %>% select(IDPOLIZA,TIPORIESGO), by = "IDPOLIZA")
print(SINIESTROS2)

Parted <- siniestros %>% 
          group_by(IDVICTIMA,COBERTURA) %>% 
          summarise(total_pagado = sum(VLRPAGO,na.rm = T), .groups = 'drop') %>%
          
          pivot_wider(
            names_from = COBERTURA,
            values_from = total_pagado,
            values_fill = 0
            )
print(Parted)
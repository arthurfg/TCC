### Primeiro teste pra ver se a memória aguenta :)

# Pacotes ####
library(magrittr)
library(dplyr)

###### Filtra pro Acre (isso aqui pode ser facilmente convertido em um map, mas ok) #####

#morador

morador <- readRDS("MORADOR.rds")
acre <- morador %>%
  filter(UF == 12) 

saveRDS(acre,"MORADOR_ACRE.rds")

#caderneta

caderneta_coletiva <- readRDS("CADERNETA_COLETIVA.rds")
acre_cc <- caderneta_coletiva %>%
  filter(UF ==12)

saveRDS(acre_cc, "CADERNETA_COLETIVA_ACRE.rds")

rm(caderneta_coletiva)

#domicilio

domicilio <- readRDS("DOMICILIO.rds")
acre_domic <- domicilio %>%
  filter(UF == 12)

saveRDS(acre_domic,"DOMICIO_ACRE.rds")

rm(domicilio)

#outros rendimentos e rendimento do trabalho

outros_rendimentos <- readRDS("OUTROS_RENDIMENTOS.rds")
rendimento_trabalho <- readRDS("RENDIMENTO_TRABALHO.rds")

acre_or <- outros_rendimentos %>%
  filter(UF == 12)
saveRDS(acre_or, "OUTROS_RENDIMENTOS_ACRE.rds")

acre_rt <- rendimento_trabalho %>%
  filter(UF == 12)
saveRDS(acre_rt, "RENDIMENTO_TRABALHO.rds")


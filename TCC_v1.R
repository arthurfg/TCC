library(tidyverse)

pof_domicilio <- read_rds("/Users/apple/Documents/TCC/dados_rds/pof_domicilio_v1.rds") # nolint
pof_morador <- read_rds("/Users/apple/Documents/TCC/dados_rds/pof_morador_v1.rds") # nolint

pof_domicilio %>% glimpse()


pof_domicilio <- pof_domicilio %>% 
  mutate(id_dom = str_c(COD_UPA, NUM_DOM))


pof_domicilio <- pof_domicilio %>% 
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM))

pof_morador <- pof_morador %>% 
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
         COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM),
         id_uc  = str_c(COD_UPA, NUM_DOM, NUM_UC),
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE))

pof_domicilio %>%
  summarise(numero_domicilios = n_distinct(id_dom),
            numero_linhas = n())

pof_morador %>%
  summarise(numero_domicilios = n_distinct(id_dom),
            numero_uc = n_distinct(id_uc),
            numero_pessoas = n_distinct(id_pes),
            numero_linhas = n())

pof_morador %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_morador_v1.rds", compress = "gz") # nolint
pof_domicilio %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_domicilio_v1.rds", compress = "gz") # nolint


pof_domicilio <- pof_domicilio %>%
    select(V6199, NUM_DOM, id_dom, TIPO_SITUACAO_REG, V0205, V0207, V0209,
    V02111, V0212, V0215) ##V6199 - Insegurança Alimentar

pof_morador <- pof_morador %>%
    #filter(V0403 <=19) %>% ## V0403 - Idade
    #filter(V0306 != 01 & 17 & 18 & 19 ) %>% ## V0306 - Condição na UC - filho, neto e etc... # nolint
    select(UF, ESTRATO_POF, PESO, PESO_FINAL, V0403, V0306,
    INSTRUCAO, COMPOSICAO, PC_RENDA_DISP, PC_RENDA_MONET,
    PC_RENDA_NAO_MONET, id_dom, id_uc, id_pes,
    V0404, V0405, ANOS_ESTUDO)

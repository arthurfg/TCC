library(tidyverse)
library(janitor)

pof_domicilio <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_domicilio.rds") # nolint
pof_morador <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_morador.rds") # nolint
pof_rendimento_trabalho <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_rendimento_trabalho.rds") # nolint

#### Tradutores

indice <- readxl::read_xls("/Users/apple/Documents/curso_pof_drive/memoria_de_calculo/Indice_Despesa.xls") # nolint
tradutor <- readxl::read_xls("/Users/apple/Documents/curso_pof_drive/tradutores/Tradutor_Despesa_Geral.xls") # nolint
cadastro <- readxl::read_xls("/Users/apple/Documents/curso_pof_drive/documentacao/Cadastro de Produtos.xls",  # nolint
                             col_types = "text")

tradutor_rendimento <- readxl::read_xls("/Users/apple/Documents/curso_pof_drive/tradutores/Tradutor_Rendimento.xls") # nolint
indice_rendimento <- readxl::read_xls("/Users/apple/Documents/curso_pof_drive/memoria_de_calculo/Indice_Rendimento.xls") # nolint

tradutor_rendimento <- tradutor_rendimento %>% 
  janitor::clean_names() %>% 
  mutate(codigo = str_pad(codigo, 5, "left", "0"))

cadastro <- cadastro %>% 
  janitor::clean_names() %>% 
  mutate(codigo_7 = str_pad(codigo_do_produto, 7, "left", "0"))

cadastro <- cadastro %>% 
  mutate(codigo_5 = str_sub(codigo_7, 1, -3))

cadastro %>% 
  group_by(codigo_5) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  arrange(n, codigo_5) %>%
  select(quadro, codigo_do_produto, codigo_7, codigo_5, descricao_do_produto, n)

tradutor <- tradutor %>% 
  janitor::clean_names() %>% 
  mutate(codigo = str_pad(codigo, 5, "left", "0"),
         descricao_2 = case_when(descricao_2 == "Despesas de consumo" ~ "Despesas de Consumo", # nolint
                                 TRUE ~ descricao_2))

#### Rendimento do trabalho #####
pof_rendimento_trabalho <- pof_rendimento_trabalho %>%
  mutate(codigo_5 = str_sub(V9001, 1, -3))

pof_rendimento_trabalho <- pof_rendimento_trabalho %>% 
  left_join(tradutor_rendimento, by = c("codigo_5" = "codigo"))

pof_rendimento_trabalho <- pof_rendimento_trabalho %>%
    mutate(valor = V8500_DEFLA)

pof_rendimento_trabalho <- pof_rendimento_trabalho %>% 
  mutate(valor = as.numeric(valor),
         FATOR_ANUALIZACAO = as.numeric(FATOR_ANUALIZACAO)) %>% 
  mutate(valor_fator_anu = valor * FATOR_ANUALIZACAO)

pof_rendimento_trabalho <- pof_rendimento_trabalho %>% 
  mutate(V9011 = as.numeric(V9011)) %>% 
  mutate(valor_anualizado = valor_fator_anu * V9011)


pof_rendimento_trabalho <- pof_rendimento_trabalho %>%
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM),
         id_uc  = str_c(COD_UPA, NUM_DOM, NUM_UC)) %>% 
  select(id_dom, id_uc, codigo_5, valor_anualizado, PESO_FINAL, V5304, V5314,
         nivel_0, descricao_0,
         nivel_1, descricao_1,
         nivel_2, descricao_2,
         nivel_3, descricao_3)

######
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


###### Salvando em .rds ######
pof_morador %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_morador_v1.rds", compress = "gz") # nolint
pof_domicilio %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_domicilio_v1.rds", compress = "gz") # nolint
pof_rendimento_trabalho %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_rendimento_trabalho_v1.rds", compress = "gz") # nolint
cadastro %>% write_rds("/Users/apple/Documents/TCC/dados_rds/cadastro_v1.rds", compress = "gz") # nolint
indice %>% write_rds("/Users/apple/Documents/TCC/dados_rds/indice_v1.rds", compress = "gz") # nolint
tradutor %>% write_rds("/Users/apple/Documents/TCC/dados_rds/tradutor_v1.rds", compress = "gz") # nolint



##### Selecionando as variáveis ######
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

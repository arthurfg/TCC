library(tidyverse)

### Tradutor
tradutor_alimentacao <- readxl::read_excel("/Users/apple/Documents/curso_pof_drive/tradutores/Tradutor Alimentacao.xls")

tradutor_alimentacao <- tradutor_alimentacao %>%
    select(Codigo, Nivel_0, Descricao_0, Nivel_1, Descricao_1,
    Nivel_2, Descricao_2, Nivel_3, Descricao_3) %>%
    janitor::clean_names()

tradutor_alimentacao <- tradutor_alimentacao %>%
    rename(nivel_0_alimentacao = nivel_0,
           descricao_0_alimentacao = descricao_0,
           nivel_1_alimentacao = nivel_1,
           descricao_1_alimentacao = descricao_1,
           nivel_2_alimentacao = nivel_2,
           descricao_2_alimentacao = descricao_2,
           nivel_3_alimentacao = nivel_3,
           descricao_3_alimentacao = descricao_3)

### Filtrando p/ as despesas com alimentação
pof_despesa <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_despesa.rds") # nolint
pof_despesa <- pof_despesa %>%
    filter(nivel_3 == 1101)

#### Join com o Tradutor 
pof_alimentacao <- pof_despesa %>%
    mutate(codigo_5 = as.numeric(codigo_5)) %>%
    left_join(tradutor_alimentacao, by = c("codigo_5" = "codigo"))

### Pof Morador

pof_morador <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_morador.rds") # nolint

pof_morador <- pof_morador %>% 
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
         COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM),
         id_uc  = str_c(COD_UPA, NUM_DOM, NUM_UC),
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE))

pof_morador_ex1 <- pof_morador %>% 
  filter(V0306 == "1") %>% 
  mutate(PESO_FINAL = as.numeric(PESO_FINAL),
         total_uc = sum(PESO_FINAL))

### Testando p/ as despesas com Alimentação - nível_0_alimentacao == 0

pof_alimentacao1 <- pof_alimentacao %>% 
  filter(nivel_0_alimentacao == 0) %>%
  group_by(id_uc) %>% 
  mutate(despesa_total_alimentacao = sum(valor_anualizado, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(id_uc, .keep_all = TRUE)

pof_join_ex1 <- pof_morador_ex1 %>% 
  left_join(pof_alimentacao1, by = "id_uc")

### Teoricamente a media de gastos cm alimentacao por UC

pof_join_ex1 %>%
  group_by(id_uc) %>%
  mutate(despesa_mensal_alimentacao = despesa_total_alimentacao/12) %>%
  summarise(media_alimentacao = mean(despesa_mensal_alimentacao)) 

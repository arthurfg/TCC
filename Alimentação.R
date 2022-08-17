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
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE)) %>%
  group_by(id_uc)%>%
  mutate(n_uc = n()) %>%
  ungroup()

pof_morador_ex1 <- pof_morador %>% 
  filter(V0306 == "1") %>% 
  mutate(PESO_FINAL = as.numeric(PESO_FINAL),
         total_uc = sum(PESO_FINAL))

### Testando p/ as despesas com Alimentação - nível_0_alimentacao == 0

pof_alimentacao1 <- pof_alimentacao %>% 
  filter(nivel_2_alimentacao == 107) %>%
  group_by(id_uc) %>% 
  mutate(despesa_total_alimentacao = sum(valor_anualizado, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(id_uc, .keep_all = TRUE)

pof_join_ex1 <- pof_morador_ex1 %>% 
  left_join(pof_alimentacao1, by = "id_uc")

pof_join_ex2 <- pof_morador_ex1 %>% 
  left_join(pof_alimentacao2, by = "id_uc")

pof_join_ex1 %>%
  summarise(n = n_distinct(id_uc))

### Teoricamente a media de gastos cm alimentacao por UC

pof_join_ex1 %>%
  group_by(id_uc) %>%
  #mutate(despesa_mensal_alimentacao = despesa_total_alimentacao/12) %>%
  summarise(media_alimentacao = sum(despesa_total_alimentacao*PESO_FINAL.x)/sum(PESO_FINAL.x)/12) %>% summary()


pof_alimentacao1 <- pof_alimentacao %>%
  select(id_dom, id_uc, valor_anualizado, PESO_FINAL,
  descricao_0_alimentacao, descricao_1_alimentacao, descricao_2_alimentacao) %>%
  pivot_wider(names_from = descricao_2_alimentacao,
              values_from = valor_anualizado,
              values_fn = ~sum(.x, na.rm = TRUE),
              names_glue = "a_{descricao_2_alimentacao}" ) %>%
    janitor::clean_names()%>%
    group_by(id_uc) %>% 
    distinct(id_uc, .keep_all = TRUE)

pof_alimentacao2 <- pof_alimentacao %>%
  select(id_dom, id_uc, valor_anualizado, PESO_FINAL, descricao_1_alimentacao) %>%
  pivot_wider(names_from = descricao_1_alimentacao,
              values_from = valor_anualizado,
              values_fn = ~sum(.x, na.rm = TRUE),
              names_glue = "a_{descricao_1_alimentacao}" ) %>%
    janitor::clean_names()%>%
    group_by(id_uc) %>% 
    distinct(id_uc, .keep_all = TRUE)

glimpse(pof_join_ex2)

divide <- function(x) (x / 12)
per_capita_alimentacao <- pof_join_ex1 %>%
  select(-starts_with("V0")) %>% 
  group_by(id_uc) %>% 
  mutate_at(vars(starts_with("a_")), divide) %>%
  mutate(peso_final = as.numeric(peso_final)) %>%
  summarise(carnes_pescados = sum(a_carnes_visceras_e_pescados*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            cereais = sum(a_cereais_leguminosas_e_oleaginosas*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            farinhas = sum(a_farinhas_feculas_e_massas*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            tuberculos = sum(a_tuberculos_e_raizes*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            acucares = sum(a_acucares_e_derivados*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            legumes_verduras = sum(a_legumes_e_verduras*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            frutas = sum(a_frutas*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            aves_ovos = sum(a_aves_e_ovos*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            leites_derivados = sum(a_leites_e_derivados*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            panificados = sum(a_panificados*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            oleos_gorduras = sum(a_oleos_e_gorduras*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            bebidas_infusoes = sum(a_bebidas_e_infusoes*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            enlatados = sum(a_enlatados_e_conservas*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            sal_condimentos = sum(a_sal_e_condimentos*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            preparados = sum(a_alimentos_preparados*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            outros = sum(a_outros*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #almoco_jantar = sum(a_almoco_e_jantar*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #cafe_leite = sum(a_cafe_leite_cafe_leite_e_chocolate*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #saunduiches = sum(a_sanduiches_e_salgados*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #refrigerantes = sum(a_refrigerantes_e_outras_bebidas_nao_alcoolicas*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #lanches = sum(a_lanches*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #cerveja = sum(a_cervejas_chopes_e_outras_bebidas_alcoolicas*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #alimentacao_light = sum(a_alimentacao_light_e_diet*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            outras = sum(a_outras*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL))

per_capita_alimentacao2 <- pof_join_ex2 %>%
  select(-starts_with("V0")) %>% 
  group_by(id_uc) %>% 
  mutate_at(vars(starts_with("a_")), divide) %>%
  mutate(peso_final = as.numeric(peso_final)) %>%
  summarise(fora_de_casa = sum(a_alimentacao_fora_do_domicilio*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            dentro_de_casa = sum(a_alimentacao_no_domicilio*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL))

per_capita_alimentacao <- left_join(per_capita_alimentacao, per_capita_alimentacao2, by = "id_uc")


per_capita_alimentacao %>% write_rds("/Users/apple/Documents/TCC/dados_rds/per_capita_alimentacao_v2.rds", compress = "gz") ## nolint


###### Sem os pesos

per_capita_alimentacao <- pof_join_ex1 %>%
  select(-starts_with("V0")) %>% 
  group_by(id_uc) %>% 
  mutate_at(vars(starts_with("a_")), divide) %>%
  mutate(peso_final = as.numeric(peso_final)) %>%
  summarise(carnes_pescados = sum(a_carnes_visceras_e_pescados, na.rm = TRUE)/n_uc,
            cereais = sum(a_cereais_leguminosas_e_oleaginosas, na.rm = TRUE)/n_uc,
            farinhas = sum(a_farinhas_feculas_e_massas, na.rm = TRUE)/n_uc,
            tuberculos = sum(a_tuberculos_e_raizes, na.rm = TRUE)/n_uc,
            acucares = sum(a_acucares_e_derivados, na.rm = TRUE)/n_uc,
            legumes_verduras = sum(a_legumes_e_verduras, na.rm = TRUE)/n_uc,
            frutas = sum(a_frutas, na.rm = TRUE)/n_uc,
            aves_ovos = sum(a_aves_e_ovos, na.rm = TRUE)/n_uc,
            leites_derivados = sum(a_leites_e_derivados, na.rm = TRUE)/n_uc,
            panificados = sum(a_panificados, na.rm = TRUE)/n_uc,
            oleos_gorduras = sum(a_oleos_e_gorduras, na.rm = TRUE)/n_uc,
            bebidas_infusoes = sum(a_bebidas_e_infusoes, na.rm = TRUE)/n_uc,
            enlatados = sum(a_enlatados_e_conservas, na.rm = TRUE)/n_uc,
            sal_condimentos = sum(a_sal_e_condimentos, na.rm = TRUE)/n_uc,
            preparados = sum(a_alimentos_preparados, na.rm = TRUE)/n_uc,
            outros = sum(a_outros, na.rm = TRUE)/n_uc,
            #almoco_jantar = sum(a_almoco_e_jantar*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #cafe_leite = sum(a_cafe_leite_cafe_leite_e_chocolate*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #saunduiches = sum(a_sanduiches_e_salgados*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #refrigerantes = sum(a_refrigerantes_e_outras_bebidas_nao_alcoolicas*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #lanches = sum(a_lanches*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #cerveja = sum(a_cervejas_chopes_e_outras_bebidas_alcoolicas*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            #alimentacao_light = sum(a_alimentacao_light_e_diet*PESO_FINAL, na.rm = TRUE)/sum(PESO_FINAL),
            outras = sum(a_outras, na.rm = TRUE)/n_uc)


per_capita_alimentacao2 <- pof_join_ex2 %>%
  select(-starts_with("V0")) %>% 
  group_by(id_uc) %>% 
  mutate_at(vars(starts_with("a_")), divide) %>%
  mutate(peso_final = as.numeric(peso_final)) %>%
  summarise(fora_de_casa = sum(a_alimentacao_fora_do_domicilio, na.rm = TRUE)/n_uc,
            dentro_de_casa = sum(a_alimentacao_no_domicilio, na.rm = TRUE)/n_uc)

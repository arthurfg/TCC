library(tidyverse)
library(janitor)


pof_domicilio <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_domicilio_v1.rds") # nolint
pof_morador <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_morador.rds") # nolint
pof_rendimento_trabalho <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_rendimento_trabalho.rds") # nolint
pof_condicoes_vida <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_consumo_alimentar.rds") # nolint

#### Tradutores ####

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
         id_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
         COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE)) %>%
  select(id_dom, id_pes, id_uc, codigo_5, valor_anualizado,
         PESO_FINAL, V5304, V5314,
         nivel_0, descricao_0,
         nivel_1, descricao_1,
         nivel_2, descricao_2,
         nivel_3, descricao_3) %>%
  drop_na(descricao_0)


###### Outros Rendimentos ####
pof_outros_rendimentos <- read_rds("/Users/apple/Documents/curso_pof_drive/dados/dados_rds/pof_outros_rendimentos.rds") # nolint

pof_outros_rendimentos <- pof_outros_rendimentos %>%
  mutate(codigo_5 = str_sub(V9001, 1, -3))

pof_outros_rendimentos <- pof_outros_rendimentos %>% 
  left_join(tradutor_rendimento, by = c("codigo_5" = "codigo"))

pof_outros_rendimentos <- pof_outros_rendimentos %>%
    mutate(valor = V8500_DEFLA)

pof_outros_rendimentos <- pof_outros_rendimentos %>% 
  mutate(valor = as.numeric(valor),
         FATOR_ANUALIZACAO = as.numeric(FATOR_ANUALIZACAO)) %>% 
  mutate(valor_fator_anu = valor * FATOR_ANUALIZACAO)

pof_outros_rendimentos <- pof_outros_rendimentos %>% 
  mutate(V9011 = as.numeric(V9011)) %>% 
  mutate(valor_anualizado = valor_fator_anu * V9011)


pof_outros_rendimentos <- pof_outros_rendimentos %>%
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM),
         id_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
         COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE)) %>%
  select(id_dom, id_pes, id_uc, codigo_5, valor_anualizado,
         PESO_FINAL, COD_IMPUT_VALOR, QUADRO,
         nivel_0, descricao_0,
         nivel_1, descricao_1,
         nivel_2, descricao_2,
         nivel_3, descricao_3) %>%
  drop_na(descricao_0)

pof_outros_rendimentos %>%
  #filter(QUADRO == 54) %>%
  summarise(nas = sum(is.na(valor_anualizado)))

pof_outros_rendimentos <- pof_outros_rendimentos %>%
  mutate(bolsa_familia = if_else(nivel_3 == 124, 1, 0))


pof_outros_rendimentos %>%
  filter(nivel_3 == 124) %>%
  group_by(id_uc) %>%
  summarise(numero_pessoas = sum(bolsa_familia)) %>%
  pull(id_uc) -> uc_pbf

pof_outros_rendimentos %>%
  filter(nivel_3 == 124) %>%
  summarise(n = n_distinct(id_pes))



pof_join %>%
  group_by(id_dom) %>%
  summarise(numero_pessoas = n_distinct(id_pes.y)) 



View(head(pof_outros_rendimentos))
############

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

pof_morador <- pof_morador %>% 
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
         COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM),
         id_uc  = str_c(COD_UPA, NUM_DOM, NUM_UC),
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE),
         PESO_FINAL = as.numeric(PESO_FINAL),
         RENDA_TOTAL = as.numeric(RENDA_TOTAL),
         PC_RENDA_DISP = as.numeric(PC_RENDA_DISP),
         PC_RENDA_MONET = as.numeric(PC_RENDA_MONET),
         V0403 = as.numeric(V0403),
         id = 1)

pof_morador <- pof_morador %>%
  filter(V0306 != "18" & V0306 != "19") %>% 
  group_by(id_uc) %>% 
  mutate(n_uc = n()) %>%
  ungroup() %>% 
  mutate(RENDA_TOTAL_PC = RENDA_TOTAL / n_uc) %>%
  select(id_dom, RENDA_TOTAL_PC, TIPO_SITUACAO_REG, n_uc,PC_RENDA_DISP, PC_RENDA_MONET)

pof_domicilio %>%
  summarise(numero_domicilios = n_distinct(id_dom),
            numero_linhas = n())

pof_morador %>%
  summarise(numero_domicilios = n_distinct(id_dom),
            #numero_pessoas = n_distinct(id_pes),
            numero_linhas = n())


pof_morador %>%
  group_by(id_dom) %>%
  mutate(moradores = sum(n_distinct(id_pes)))
  summarise(moradores = n_distinct(id_pes))

###### Salvando em .rds ######
pof_morador %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_morador_v1.rds", compress = "gz") # nolint
pof_domicilio %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_domicilio_v1.rds", compress = "gz") # nolint
pof_rendimento_trabalho %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_rendimento_trabalho_v1.rds", compress = "gz") # nolint
cadastro %>% write_rds("/Users/apple/Documents/TCC/dados_rds/cadastro_v1.rds", compress = "gz") # nolint
indice %>% write_rds("/Users/apple/Documents/TCC/dados_rds/indice_v1.rds", compress = "gz") # nolint
tradutor %>% write_rds("/Users/apple/Documents/TCC/dados_rds/tradutor_v1.rds", compress = "gz") # nolint
pof_join %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_join_v1.rds", compress = "gz") # nolint
teste %>% write_rds("/Users/apple/Documents/TCC/dados_rds/teste_v1.rds", compress = "gz") # nolint
pof_outros_rendimentos %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_outros_rendimentos_v1.rds", compress = "gz") # nolint


##### Selecionando as variáveis ######
pof_domicilio <- pof_domicilio %>%
    select(V6199, NUM_DOM, id_dom, TIPO_SITUACAO_REG, V0205, V0207, V0209,
    V02111, V0212, V0215) ##V6199 - Insegurança Alimentar

pof_morador <- pof_morador %>%
    #filter(V0403 <=19) %>% ## V0403 - Idade
    #filter(V0306 != 01 & 17 & 18 & 19 ) %>% ## V0306 - Condição na UC - filho, neto e etc... # nolint
    select(id_dom, RENDA_TOTAL)


#####

pof_rendimento_trabalho2 <- pof_rendimento_trabalho %>%
    select(id_pes, id_uc, id_dom, V5304, V5314) %>%
    distinct(id_pes, .keep_all = TRUE)

pof_join <- pof_rendimento_trabalho2 %>%
    right_join(pof_morador, by = "id_pes")

dim(pof_morador)

distinct(pof_join)

teste %>%
    drop_na(id_uc) %>%
    summarise(n = n_distinct(id_pes.y))
summary(teste$pr_formal)
pof_rendimento_trabalho %>%
    #drop_na(id_uc) %>%
    summarise(n = n_distinct(id_uc))

teste %>%
    summarise(n = n_distinct(id_uc))


teste <- pof_join %>% head(300)


pof_rendimento_trabalho <- pof_rendimento_trabalho %>%
    drop_na(descricao_0)

teste %>%
    mutate(pr_formal = if_else(V0306 == 1 & V5304 == 1, 1, 0),
    pr_branco = if_else(V0306 == 1 & V0405 == 1, 1, 0),
    pr_masculino = if_else(V0306 == 1 & V0404 == 1, 1, 0),
    pr_idade = if_else(V0306 == 1 & V0404 == 1, 1, 0)) %>% View()

pof_join <- pof_join %>%
    filter(V0306 == 1) %>%
    mutate(pr_formal = if_else(V5304 == 1, 1, 0),
    pr_branco = if_else(V0405 == 1, 1, 0),
    pr_masculino = if_else(V0404 == 1, 1, 0),
    pr_idade = V0403,
    pr_anos_estudo = ANOS_ESTUDO,
    pr_horas_trabalhadas = V5314) %>%
    select(id_dom.y, id_pes, id_uc.y, pr_formal, pr_branco, pr_masculino,
    pr_idade, pr_anos_estudo, pr_horas_trabalhadas) %>%
    rename(id_uc = id_uc.y) %>%
    right_join(pof_morador, by = "id_uc")

## Para amanhã: Colocar as características do domicilio, mesmo procedimento de hoje. # nolint

## 3 de Agosto
pof_join <- read_rds("/Users/apple/Documents/TCC/dados_rds/pof_join_v1.rds")
pof_domicilio <- read_rds("/Users/apple/Documents/TCC/dados_rds/pof_domicilio_v1.rds")
teste <- read_rds("/Users/apple/Documents/TCC/dados_rds/teste_v1.rds")
pof_rendimento_trabalho <- read_rds("/Users/apple/Documents/TCC/dados_rds/pof_rendimento_trabalho_v1.rds")
pof_outros_rendimentos <- read_rds("/Users/apple/Documents/TCC/dados_rds/pof_outros_rendimentos_v1.rds") # nolint
pof_morador <- read_rds("/Users/apple/Documents/TCC/dados_rds/pof_morador_v1.rds")

summary(teste$pr_branco)

teste <- pof_domicilio %>% 
  select(V0205, V0207, V0209, V02111, V0212, V0215,
  V6199, V0213, id_dom) %>%
  mutate(comodos = V0205,
   agua_encanada = if_else(V0209 == 3, 0, 1),
   banheiro = if_else(V02111 >= 1, 1, 0),
   coleta_lixo = if_else(V0213 <= 2, 1, 0),
   esgotamento_sanitario = if_else(V0212 == 1 | V0212 == 2, 1, 0),
   energia_eletrica = if_else(V0215 == 1 | V0215 == 2, 1, 0),
   infra = ind$`Dim 1`) %>%
   left_join(pof_join, by = "id_dom")

teste %>%
  select(infra, PC_RENDA_DISP) %>%
  mutate(renda_pc = as.numeric(PC_RENDA_DISP),
  infra = infra) %>%
  filter(renda_pc > 0 & renda_pc < 600) -> teste2

cor.test(teste2$infra, teste2$renda_pc, method = "pearson")


teste2 <- teste %>%
  right_join(pof_outros_rendimentos, by = "id_dom")

glimpse(teste2)

teste2 %>%
  summarise(n = n_distinct(id_pes.y))

teste <- teste %>%
  mutate(bolsa_familia = if_else(id_uc %in% uc_pbf, 1, 0)) 

teste %>%
  filter(V0403 < 19, bolsa_familia == 1)

pof_morador <- pof_morador %>%
  mutate(id_dom = as.numeric(id_dom)) %>%
  distinct(id_dom, .keep_all = TRUE)
                             

teste <- teste %>%
  mutate(id_dom = as.numeric(id_dom)) %>%
  right_join(pof_morador, by = "id_dom")


####### PAREI AQUI #########
criancas_0_5 <- teste %>%
  filter(id_dom %in% dom_uc) %>%
  mutate(idade = as.numeric(V0403)) %>%
  filter(V0306 != 17 & 18 & 19) %>%
  filter(idade <= 5) 

criancas_5_10 <- teste %>%
  filter(id_dom %in% dom_uc) %>%
  mutate(idade = as.numeric(V0403)) %>%
  filter(V0306 != 17 & 18 & 19) %>%
  filter(idade >= 5 & idade <= 10)


criancas_10_19 <- teste %>%
  filter(id_dom %in% dom_uc) %>%
  mutate(idade = as.numeric(V0403)) %>%
  filter(V0306 != 17 & 18 & 19) %>%
  filter(idade >= 10 & idade <= 19)

summary(as.numeric(criancas_5_10$pr_idade))

criancas_10_19 %>%
  mutate(PC_RENDA_DISP = as.numeric(PC_RENDA_DISP)) %>%
  filter(PC_RENDA_DISP < 300) 

criancas_10_19 <- pof_morador %>%
  filter(id_dom %in% dom_uc) %>%
  distinct(id_dom, .keep_all =  TRUE) %>%
  right_join(criancas_10_19, by = "id_dom") 


criancas_0_5 <- pof_morador %>%
  filter(id_dom %in% dom_uc) %>%
  distinct(id_dom, .keep_all =  TRUE) %>%
  right_join(criancas_0_5, by = "id_dom") 

criancas_5_10 <- pof_morador %>%
  filter(id_dom %in% dom_uc) %>%
  distinct(id_dom, .keep_all =  TRUE) %>%
  right_join(criancas_5_10, by = "id_dom")



criancas_0_5 %>% write_rds("/Users/apple/Documents/TCC/dados_rds/criancas_0_5.rds", compress = "gz") # nolint
criancas_5_10 %>% write_rds("/Users/apple/Documents/TCC/dados_rds/criancas_5_10.rds", compress = "gz") # nolint
criancas_10_19 %>% write_rds("/Users/apple/Documents/TCC/dados_rds/criancas_10_19.rds", compress = "gz") # nolint


### Criando dummies de região
sudeste <- c(35, 33, 32, 31)
sul <- c(43, 42, 41)
centro_oeste <- c(50, 51, 52, 53)
norte <- c(12, 14, 11, 13, 15, 17, 16)
nordeste <- c(29, 22, 21, 28, 27, 26, 25, 24, 23 )

criancas_0_5 <- criancas_0_5 %>%
  mutate(UF = as.numeric(UF),
  sudeste = if_else(UF %in% sudeste, 1, 0),
  sul = if_else(UF %in% sul, 1, 0),
  centro_oeste = if_else(UF %in% centro_oeste, 1, 0),
  norte = if_else(UF %in% norte, 1, 0),
  nordeste = if_else(UF %in% nordeste, 1, 0))

criancas_5_10 <- criancas_5_10 %>%
  mutate(UF = as.numeric(UF),
  sudeste = if_else(UF %in% sudeste, 1, 0),
  sul = if_else(UF %in% sul, 1, 0),
  centro_oeste = if_else(UF %in% centro_oeste, 1, 0),
  norte = if_else(UF %in% norte, 1, 0),
  nordeste = if_else(UF %in% nordeste, 1, 0))

criancas_10_19 <- criancas_10_19 %>%
  mutate(UF = as.numeric(UF),
  sudeste = if_else(UF %in% sudeste, 1, 0),
  sul = if_else(UF %in% sul, 1, 0),
  centro_oeste = if_else(UF %in% centro_oeste, 1, 0),
  norte = if_else(UF %in% norte, 1, 0),
  nordeste = if_else(UF %in% nordeste, 1, 0))

criancas_0_5 <- criancas_0_5 %>%
  mutate(across(where(is.character), as.numeric))

criancas_5_10 <- criancas_5_10 %>%
  mutate(across(where(is.character), as.numeric))

criancas_10_19 <- criancas_10_19 %>%
  mutate(across(where(is.character), as.numeric))


summary(as.numeric(teste$V0403))

teste %>%
  group_by(id_dom) %>%
  summarise(qtds_ucs = sum(n_distinct(id_uc))) %>%
  filter(qtds_ucs <= 1) %>%
  pull(id_dom) -> dom_uc     ## domicilios com mais de uma UC

##### POF - Condições de Vida #####
leitores_vida <- readxl::read_excel("/Users/apple/Documents/curso_pof_drive/documentacao/dicionario_variaveis.xls",
                                           sheet = "Condições de Vida",
                                           skip = 2) %>% 
  janitor::clean_names() %>% 
  select(posicao_inicial, tamanho, codigo_da_variavel) %>% 
  filter(complete.cases(.)) %>% 
  mutate(posicao_inicial = as.numeric(posicao_inicial),
         tamanho = as.numeric(tamanho),
         posicao_final = posicao_inicial + tamanho - 1) %>% 
  select(posicao_inicial, tamanho, posicao_final, codigo_da_variavel)


colpos_vida <- fwf_positions(start = leitores_vida$posicao_inicial,
                                  end = leitores_vida$posicao_final,
                                  col_names = leitores_vida$codigo_da_variavel)

pof_condicoes_vida <- read_fwf(file = "/Users/apple/Documents/curso_pof_drive/dados/CONDICOES_VIDA.txt",
                          col_positions = colpos_vida,
                          col_types = cols(.default = col_character()))

pof_condicoes_vida %>% write_rds("/Users/apple/Documents/TCC/dados_rds/pof_condicoes_vida_v1.rds", compress = "gz") # nolint


glimpse(condicoes)

pof_condicoes_vida <- pof_condicoes_vida %>% 
  mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
         NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
         COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
         id_dom = str_c(COD_UPA, NUM_DOM),
         id_uc  = str_c(COD_UPA, NUM_DOM, NUM_UC),
         id_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE))


######### PCA ####


pof_condicoes_vida %>%
  select(V61042, V61051, V61053, V61054, V61064, V61066,
  V61068, V61069, V610610, V610611, id_dom) -> condicoes

condicoes2 <- pof_domicilio %>%
  select(V0202, V0203, V0204, id_dom)

condicoes <- left_join(condicoes, condicoes2, by = "id_dom")


teste2 %>% ggplot(aes(x=renda_pc, y=infra)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) 


plot(x = teste2$infra, y = teste2$renda_pc )

summary(teste2)
mean(ind$`Dim 1`)


res_mca <- pof_domicilio %>%
  select(V0202, V0203, V0204) %>%
  prcomp(scale = TRUE)

res_pca <- condicoes %>%
  distinct(id_dom, .keep_all = TRUE) %>%
  select(-id_dom) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  prcomp()

res_pca <- res_pca$x
res_pca <- as_tibble(res_pca)
res_pca


summary(res_pca)
plot(res_pca, type = "l")
summary(res_pca$x)

res_pca <- as_tibble(res_pca$x)
glimpse(res_pca)

res_mca <- condicoes %>%
  distinct(id_dom, .keep_all = TRUE) %>%
  select(-id_dom) %>%
  FactoMineR::MCA(graph = FALSE)

fviz_mca_var(res_mca, choice = "mca.cor", 
            repel = TRUE, # Avoid text overlapping (slow)
            ggtheme = theme_minimal())

fviz_screeplot(res_mca, addlabels = TRUE, ylim = c(0, 45))

fviz_contrib(res_mca, choice = "var", axes = 2, top = 15)

fviz_mca_var(res_mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
             )


ind <- get_mca_ind(res_mca)
ind
ind <- as_tibble(ind$coord)


library("FactoMineR")
library("factoextra")

data(poison)
ind <- as_tibble(ind$contrib)

poison.active <- poison[1:55, 5:15]


fviz_screeplot(res_mca, addlabels = TRUE, ylim = c(0, 45))
fviz_mca_var(res_mca, choice = "mca.cor", 
            repel = TRUE, # Avoid text overlapping (slow)
            ggtheme = theme_minimal())


ind <- get_mca_ind(res_mca)
ind

head(ind$coord)
# Quality of representation
head(ind$cos2)
# Contributions
head(ind$contrib)



###### Outros Rendimentos - PBF ######


#### Matching
library("MatchIt")
library("lmtest")
library("sandwich")
library("boot")

data("lalonde")

criancas_10_19 <- read_rds("/Users/apple/Documents/TCC/dados_rds/criancas_10_19.rds")
criancas_0_5 <- read_rds("/Users/apple/Documents/TCC/dados_rds/criancas_0_5.rds")

per_capita_alimentacao <- per_capita_alimentacao %>%
    mutate(id_uc = as.numeric(id_uc))

pc_teste <- pc_teste %>%
    mutate(id_uc = as.numeric(id_uc))


criancas_10_19 <- criancas_10_19 %>%
  mutate(id_uc = as.numeric(id_uc)) %>%
  left_join(per_capita_alimentacao, by = "id_uc") 



criancas_10_19 <- criancas_10_19 %>%
  mutate(id_uc = as.numeric(id_uc)) %>%
  left_join(pc_teste, by = "id_uc") 


criancas_0_5 <- criancas_0_5 %>%
  mutate(id_uc = as.numeric(id_uc)) %>%
  left_join(pc_teste, by = "id_uc") 

match_data <- criancas_0_5 %>%
  #select(id_dom, RENDA_TOTAL_PC, sudeste, sul, centro_oeste, norte, nordeste,
  #agua_encanada, banheiro, esgotamento_sanitario, energia_eletrica, n_uc,
  #comodos, pr_idade, pr_anos_estudo, pr_masculino, pr_branco,
  # pr_formal, pr_horas_trabalhadas, bolsa_familia, PESO_FINAL, V6199, idade ) %>%
   mutate(inseguranca_alimentar = if_else(V6199 != 1, 1, 0))%>%
   mutate(inseguranca_grave = if_else(V6199 == 4, 1, 0),
          inseguranca_moderada = if_else(V6199 == 3, 1, 0),
          inseguranca_leve = if_else(V6199 == 2, 1, 0)) %>%
   distinct(id_dom, .keep_all = TRUE) %>%
   mutate(pc_gasto = media_alimentacao/n_uc) %>%
   mutate(maior = if_else(RENDA_TOTAL_PC > pc_gasto, 1, 0),
          menor = if_else(pc_gasto > 0.20*RENDA_TOTAL_PC, 1, 0)) %>%
   filter(maior == 1, menor ==1 ) %>%
   filter(RENDA_TOTAL_PC < 400)

library("ggpubr")
ggscatter(match_data, x = "RENDA_TOTAL_PC", y = "pc_gasto", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "renda_pc", ylab = "despesa dentro de casa")


match_data <- criancas_0_5 %>%
  select(id_dom, RENDA_TOTAL_PC, sudeste, sul, centro_oeste, norte, nordeste,
  agua_encanada, banheiro, esgotamento_sanitario, energia_eletrica, n_uc,
  comodos, pr_idade, pr_anos_estudo, pr_masculino, pr_branco,
   pr_formal, pr_horas_trabalhadas, bolsa_familia, PESO_FINAL, V6199) %>%
   mutate(inseguranca_alimentar = if_else(V6199 != 1, 1, 0))%>%
   mutate(inseguranca_grave = if_else(V6199 == 4, 1, 0),
          inseguranca_moderada = if_else(V6199 == 3, 1, 0),
          inseguranca_leve = if_else(V6199 == 2, 1, 0)) %>%
   filter(RENDA_TOTAL_PC < 500)

match_data <- match_data %>%
  replace_na(list(pr_formal = 0, esgotamento_sanitario = 0, energia_eletrica = 0))

summary(match_data$inseguranca_alimentar)


m_out1 <- matchit(bolsa_familia ~ sudeste + sul + centro_oeste + norte +
                  nordeste + agua_encanada + banheiro + esgotamento_sanitario +
                  energia_eletrica + n_uc + comodos +
                  pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal,
                  data = match_data,
                  method = "cem", cutpoints = list(pr_anos_estudo = 5,
                  n_uc = 3, comodos = 3, pr_idade = 5),
                  s.weights = ~match_data$PESO_FINAL,
                  estimand = "ATE",
                  k2k = TRUE)


m_out1
summary(m_out1)

md <- match.data(m_out1, include.s.weights = TRUE)
log(criancas_0_5$RENDA_TOTAL_PC)

plot(m_out1, type = "density", interactive = FALSE,
which.xs = c("pr_idade", "pr_branco", "comodos"), subclass = 3)
#Linear model with covariates

fit3 <- lm(log(exp(pc_gasto)) ~ bolsa_familia + agua_encanada + banheiro + esgotamento_sanitario +
                  energia_eletrica + n_uc + comodos +
                  pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal + inseguranca_alimentar, data = md,
           weights = weights)

coeftest(fit3, vcov. = vcovCL)["bolsa_familia",,drop=FALSE]
fit1 <- lm(pc_gasto ~ bolsa_familia + pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal + inseguranca_alimentar, data = md, weights = weights)
summary(fit1)
summary(fit3)

#Generalized linear model without covariates
fit4 <- glm(inseguranca_leve ~ bolsa_familia, data = md, weights = weights, 
            family = binomial(link = "logit"))

coeftest(fit4, vcov. = vcovCL, cluster = ~subclass)

fit3 <- glm(inseguranca_moderada ~ bolsa_familia, data = md, weights = weights,
            family = quasibinomial(link = "log"))

summary(lm(inseguranca_alimentar ~ bolsa_familia, data = md, weights = weights))
coeftest(fit3, vcov. = vcovHC)
exp(coef(fit3))
coeftest(fit3, vcov. = vcovCL, cluster = ~subclass)


#Bootstrap confidence intervals
library(boot)

est_fun <- function(data, i) {
  #Subclassification function
  mS_boot <- matchit(bolsa_familia ~ sudeste + sul + centro_oeste + norte +
                  nordeste + agua_encanada + banheiro + esgotamento_sanitario +
                  energia_eletrica + n_uc + comodos +
                  pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal,
                  data = data[i,],
                  method = "cem", cutpoints = list(pr_anos_estudo = 5,
                  n_uc = 3, comodos = 3, pr_idade = 5),
                  s.weights = ~data[i,]$PESO_FINAL,
                  estimand = "ATE")
  md_boot <- match.data(mS_boot)
  
  #Fitting the model
  fit_boot <- glm(inseguranca_moderada ~ bolsa_familia + sudeste + sul + centro_oeste + norte +
                  nordeste + agua_encanada + banheiro + esgotamento_sanitario +
                  energia_eletrica + n_uc + comodos +
                  pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal, data = md_boot,
                  family = quasibinomial(link = "logit"))
  
  #Estimate potential outcomes for each unit
  

  ##
  
  md_boot$bolsa_familia <- 0
  P0 <- mean(predict(fit_boot, md_boot, type = "response"))
  Odds0 <- P0 / (1 - P0)
  
  md_boot$bolsa_familia <- 1
  P1 <- mean(predict(fit_boot, md_boot, type = "response"))
  Odds1 <- P1 / (1 - P1)

  #Return marginal odds ratio
  return(Odds1 / Odds0)
}

boot_est <- boot(match_data, est_fun, R = 199)
boot_est
boot.ci(boot.out = boot_est, type = "all")


match_data %>%
  summarise(nas = sum(is.na(esgotamento_sanitario)))


######### Mais um teste ########

#Block bootstrap confidence interval
# library(boot)

pair_ids <- levels(md$subclass)

est_fun <- function(pairs, i) {
  
  #Compute number of times each pair is present
  numreps <- table(pairs[i])
  
  #For each pair p, copy corresponding md row indices numreps[p] times
  ids <- unlist(lapply(pair_ids[pair_ids %in% names(numreps)],
                       function(p) rep(which(md$subclass == p), 
                                              numreps[p])))
  
  #Subset md with block bootstrapped ids
  md_boot <- md[ids,]
  
  #Fitting outcome the model
  fit_boot <- glm(inseguranca_grave ~ bolsa_familia + sudeste + sul + centro_oeste + norte +
                  nordeste + agua_encanada + banheiro + esgotamento_sanitario +
                  energia_eletrica + n_uc + comodos +
                  pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal, data = md_boot, 
                  family = binomial(link = "logit"),
                  weights = weights)
  
  #Estimate potential outcomes for each unit
  #Under control
  md_boot$bolsa_familia <- 0
  P0 <- weighted.mean(predict(fit_boot, md_boot, type = "response"),
                      w = md_boot$weights)
  Odds0 <- P0 / (1 - P0)
  
  #Under treatment
  md_boot$bolsa_familia <- 1
  P1 <- weighted.mean(predict(fit_boot, md_boot, type = "response"),
                      w = md_boot$weights)
  Odds1 <- P1 / (1 - P1)

  #Return marginal odds ratio
  return(Odds1 / Odds0)
}

boot_est <- boot(pair_ids, est_fun, R = 499)
boot_est

boot.ci(boot_est, type = "all")

md %>%
  group_by(bolsa_familia) %>%
  summarise(insegu = mean(RENDA_TOTAL_PC))

fit1 <- lm(RENDA_TOTAL_PC ~ bolsa_familia + sudeste + sul + centro_oeste + norte +
                  nordeste + agua_encanada + banheiro + esgotamento_sanitario +
                  energia_eletrica + n_uc + comodos +
                  pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal, data = md, weights = weights)

coeftest(fit1, vcov. = vcovHC)
coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)

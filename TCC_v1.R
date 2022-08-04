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
    select(UF, ESTRATO_POF, PESO, PESO_FINAL, V0403, V0306,
    INSTRUCAO, COMPOSICAO, PC_RENDA_DISP, PC_RENDA_MONET,
    PC_RENDA_NAO_MONET, id_dom, id_uc, id_pes,
    V0404, V0405, ANOS_ESTUDO)


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
    summarise(n = n_distinct(id_dom))


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

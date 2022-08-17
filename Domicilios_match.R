teste %>% write_rds("/Users/apple/Documents/TCC/dados_rds/teste_domicilios.rds", compress = "gz") # nolint
per_capita_alimentacao <- read_rds("/Users/apple/Documents/TCC/dados_rds/per_capita_alimentacao.rds")
per_capita_alimentacao <- read_rds("/Users/apple/Documents/TCC/dados_rds/per_capita_alimentacao_v2.rds")
teste <- read_rds("/Users/apple/Documents/TCC/dados_rds/teste_domicilios.rds")


teste <- teste %>%
  filter(id_dom %in% dom_uc) %>%
  distinct(id_dom, .keep_all = TRUE)

teste %>%
    filter(RENDA_TOTAL_PC < 300) %>%
    left_join(per_capita_alimentacao, by = "id_uc") -> match_data


sudeste <- c(35, 33, 32, 31)
sul <- c(43, 42, 41)
centro_oeste <- c(50, 51, 52, 53)
norte <- c(12, 14, 11, 13, 15, 17, 16)
nordeste <- c(29, 22, 21, 28, 27, 26, 25, 24, 23 )

match_data <- match_data %>%
  mutate(UF = as.numeric(UF),
  sudeste = if_else(UF %in% sudeste, 1, 0),
  sul = if_else(UF %in% sul, 1, 0),
  centro_oeste = if_else(UF %in% centro_oeste, 1, 0),
  norte = if_else(UF %in% norte, 1, 0),
  nordeste = if_else(UF %in% nordeste, 1, 0))


match_data <- match_data %>%
  mutate(across(where(is.character), as.numeric))

match_data %>%
  summarise(n = n_distinct(id_uc))

#### Matching
library("MatchIt")
library("lmtest")
library("sandwich")
library("boot")

match_data <- match_data %>%
   mutate(inseguranca_alimentar = if_else(V6199 != 1, 1, 0))%>%
   mutate(inseguranca_grave = if_else(V6199 == 4, 1, 0),
          inseguranca_moderada = if_else(V6199 == 3, 1, 0),
          inseguranca_leve = if_else(V6199 == 2, 1, 0)) 

match_data <- match_data %>%
  replace_na(list(pr_formal = 0, esgotamento_sanitario = 0, energia_eletrica = 0))



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
md %>%
  mutate(leites_derivados = log1p(leites_derivados)) ->md2

#Linear model with covariates
fit1 <- lm(log1p(leites_derivados) ~ bolsa_familia + pr_idade + I(pr_idade^2) , data = md, weights = weights)
coeftest(fit1, vcov. = vcovCL)["bolsa_familia",,drop=FALSE]

fit1 <- lm(log1p(leites_derivados) ~ bolsa_familia + comodos*n_uc + pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal + agua_encanada + banheiro + esgotamento_sanitario +
                  energia_eletrica  , data = md, weights = weights)
summary(fit1)
plot(fit1, which=2, col=c("red"))  # Q-Q Plot

100 * ((exp(fit1$coefficients["bolsa_familia"])) -1)

fit1$coefficients["bolsa_familia"]


library("ggpubr")
md %>%
    ggscatter( x = "RENDA_TOTAL_PC", y = "carnes_pescados", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "renda_pc", ylab = "despesa dentro de casa")



md %>%
    mutate(bolsa_familia = as_factor(bolsa_familia)) %>%
    filter(carnes_pescados < 300 & carnes_pescados > 10, RENDA_TOTAL_PC < 250) %>%
    ggplot(aes(x = RENDA_TOTAL_PC, y = carnes_pescados, color = bolsa_familia)) +
    geom_point() +
    scale_x_log10() + scale_y_log10()


md %>%
    filter(bolsa_familia == 0) %>%
    summarise(media = mean(leites_derivados))

fit4 <- glm(inseguranca_alimentar ~ bolsa_familia, data = md, weights = weights, 
            family = binomial(link = "logit"))

summary(fit4)
coeftest(fit4, vcov. = vcovCL, cluster = ~subclass)
exp(coef(fit4)) #OR

a <- names(md)

a <- a[47:65]

ln <- function(x) (log1p(x))
md %>% 
  mutate(across(a, ~ln(.x))) -> md2

regs <- expand.grid(variavel = a) %>%
  as_tibble() %>%
  mutate(variavel = as.character(variavel)) %>%
  mutate(regressao = map(.x = variavel, ~lm(as.formula(paste(.x, "~", "bolsa_familia + comodos*n_uc + pr_idade + pr_anos_estudo + pr_masculino +
                  pr_branco + pr_formal + agua_encanada + banheiro + esgotamento_sanitario + energia_eletrica")), data = md2, weights = weights) ))


regs
library(broom)
library(gtsummary)
install.packages("broom")

regs$regressao[11]

a[11]

ln <- function(x) (log1p(x))
md %>% 
  mutate(across(a, ~ln(.x))) 


tbl_regression(fit1) %>%
  bold_p(t = 0.1) %>%
  as_gt() %>%
  gt::gtsave(filename = "teste.rtf") # use extensions .html .tex .ltx .rtf

regs

library(stargazer)
stargazer(regs$regressao[1], title = "Results", align = TRUE, type = "text") 

gt_1 <- tbl_regression(regs$regressao[[1]], include = bolsa_familia,
          label = list(bolsa_familia ~ "Treatment (adjusted)")) %>%
  add_significance_stars(thresholds = c(0.001, 0.01, 0.05, 0.1)) %>%
  bold_labels() %>%
  add_global_p()

gt_2 <- tbl_regression(regs$regressao[[2]], include = bolsa_familia) %>%
  add_significance_stars(thresholds = c(0.001, 0.01, 0.05, 0.1))

gt_3 <- tbl_regression(regs$regressao[[3]]) %>%
  add_significance_stars(thresholds = c(0.001, 0.01, 0.05, 0.1))
gt_4 <- tbl_regression(regs$regressao[[4]]) %>%
  add_significance_stars(thresholds = c(0.001, 0.01, 0.05, 0.1))
gt_5 <- tbl_regression(regs$regressao[[5]]) %>%
  add_significance_stars(thresholds = c(0.001, 0.01, 0.05, 0.1))
gt_6 <- tbl_regression(regs$regressao[[6]]) %>%
  add_significance_stars(thresholds = c(0.001, 0.01, 0.05, 0.1))


theme_gtsummary_compact()
#> Setting theme `Compact`
tbl_merge(
  list(gt_1, gt_2, gt_3, gt_4, gt_5, gt_6),
  tab_spanner = c("Carnes e Pescados", "Cereais", "Farinhas", "Tubérculos", "Açucares", "Legumes e Verduras")
)

tbl_stack(list(gt_1, gt_2), group_header = c("**Carnes e Pescados**", "Cereais"))


regs %>% write_rds("/Users/apple/Documents/TCC/dados_rds/regressoes.rds")

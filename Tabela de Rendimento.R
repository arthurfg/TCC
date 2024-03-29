#                            POF 2017-2018
  
#  PROGRAMA PARA GERA??O DAS ESTIMATIVAS PONTUAIS DA TABELA DE RENDIMENTO,
#  EXCETO AS LINHAS DE RENDIMENTO N?O MONET?RIO E VARIA??O PATRIMONIAL
#                       N?VEL GEOGR?FICO - BRASIL 

# ? preciso executar antes o arquivo "Leitura dos Microdados - R.R"
# que se encontra no arquivo compactado "Programas_de_Leitura.zip"
# Este passo ? necess?rio para gerar os arquivos com a extens?o .rds
# correspondentes aos arquivos com extens?o .txt dos microdados da POF

# "....." indica a pasta/diret?rio de trabalho no HD local separados por "/"
# onde se encontram os arquivos .txt descompactados do arquivo Dados_aaaammdd.zip
# Exemplo: setwd("c:/POF2018/Dados_aaaammdd/")

setwd(".....") # Caminho onde se encontram as bases de dados


#  Leitura do REGISTRO -  - ALUGUEL ESTIMADO 

rendimento_trabalho <- readRDS("RENDIMENTO_TRABALHO.rds")


#   Anualiza??o e expans?o dos valores de rendimentos utilizados para a obten??o 
#   dos resultados (vari?vel V8500_DEFLA).

# a) Para anualizar, utilizamos o quesito "fator_anualizacao". No caso espec?fico
#    deste registro, cujas informa??es se referem a valores mensais, utilizamos
#    tamb?m o quesito V9011 (n?mero de meses).
#    Os valores s?o anualizados para depois se obter uma m?dia mensal.

# b) Para expandir, utilizamos o quesito "peso_final".

# c) Posteriormente, o resultado ? dividido por 12 para obter a estimativa mensal.
  
rend_trabalho <- 
  transform( 
    subset(rendimento_trabalho,
           !is.na(V8500_DEFLA)
           ) ,
    valor_mensal=(V8500_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )[ , c( "V9001" , "valor_mensal" ) ] 
  
  
rm(rendimento_trabalho)


# Leitura do REGISTRO - OUTROS RENDIMENTOS

outros_rendimentos <- readRDS("OUTROS_RENDIMENTOS.rds")


#   Anualiza??o e expans?o dos valores de dedu??es utilizados para a obten??o
#   dos resultados (vari?vel V850_defla).

# a) Para anualizar, utilizamos o quesito "fator_anualizacao". No caso espec?fico 
#    do quadro 54, cujas informa??es se referem a valores mensais, utilizamos tamb?m
#    o quesito V9011 (n?mero de meses).
#    Os valores s?o anualizados para depois se obter uma m?dia mensal.

# b) Para expandir, utilizamos o quesito "peso_final".

# c) Posteriormente, o resultado ? dividido por 12 para obter a estimativa mensal. 

outros_rend <-
  transform( outros_rendimentos,
             valor_mensal = ifelse( QUADRO==54,
                                      (V8500_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                      (V8500_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 
                                    ) 
             ) [ , c( "V9001" , "valor_mensal" ) ]
rm(outros_rendimentos)


# [1] Jun??o dos registros, que englobam os itens componentes da tabela de despesa geral. 

# [2] Transforma??o do c?digo do item (vari?vel V9001) em 5 n?meros, para ficar no mesmo
#     padr?o dos c?digos que constam nos arquivos de tradutores das tabelas. Esses c?digos
#     s?o simplificados em 5 n?meros, pois os 2 ?ltimos n?meros caracterizam sin?nimos.
#     Todos os resultados da pesquisa s?o trabalhados com os c?digos considerando 
#     os 5 primeiros n?meros.


junta <- 
  rbind( rend_trabalho ,
         outros_rend ) # [1]

junta <- 
  transform( junta ,
             codigo = trunc(V9001/100) 
             )[ , 2:3 ] # [2]

rm( rend_trabalho ,
    outros_rend
)


# Leitura do REGISTRO - MORADOR, necess?rio para o c?lculo do n?mero de UC's expandido.
# Vale ressaltar que este ? o ?nico registro dos microdados que engloba todas as UC's

# Extraindo todas as UC's do arquivo de morador

morador_uc <- 
  unique( 
    readRDS( 
      "MORADOR.rds" 
    ) [ ,
        c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
           "PESO_FINAL"
        ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
        ] ) # Apenas um registro por UC

# Calculando o n?mero de UC's expandido 
# A cada domic?lio ? associado um peso_final e este ? tamb?m associado a cada uma de suas unidades de consumo 
# Portanto, o total de unidades de consumo (familias) expandido, ? o resultado da soma dos pesos_finais a elas associados

soma_familia <- sum( morador_uc$PESO_FINAL )


# Leitura do arquivo de tradutor da tabela de rendimento. 
# Este tradutor organiza os c?digos de produtos pelos diferetes
# grupos da tabela de rendimento.

# Descomente e execute o comando seguinte apenas se o pacote "readxl" n?o estiver ainda instalado:
# install.packages("readxl")

# "....." indica a pasta/diret?rio de trabalho no HD local separados por "/"
# onde se encontram os arquivos .xls dos tradutores das tabelas
# Exemplo: setwd("c:/POF2018/Tradutores_aaaammdd/Tradutores das Tabelas/")

setwd(".....") # ..... ? o caminho para a pasta "/Tradutores_aaaammdd/Tradutores das Tabelas/"

tradutor_rendimento <-
  readxl::read_excel("Tradutor_Rendimento.xls") 


# Juntando a base de dados com o tradutor da tabela de rendimento por c?digo.

# Descomenta e execute o comando seguinte apenas se o pacote "sqldf" n?o estiver ainda instalado:
# install.packages("sqldf")

junta_tradutor <-
  sqldf::sqldf("SELECT a.*,
               b.nivel_1,
               b.nivel_2,
               b.nivel_3
               from junta as a 
               left join tradutor_rendimento as b
               on a.codigo = b.codigo"
               )


merge1 <- junta_tradutor
rm( junta, junta_tradutor , tradutor_rendimento)

# Somando os valores mensais de cada grupo de c?digos, segundo cada n?vel, conforme consta no tradutor

soma_final_2 <- aggregate(valor_mensal~Nivel_2,data=merge1,sum)
names(soma_final_2) <- c("nivel", "soma")

soma_final_3 <- aggregate(valor_mensal~Nivel_3,data=merge1,sum)
names(soma_final_3) <- c("nivel", "soma")


# [1] Empilhando as somas obtidas no passo anterior 
# [2] Cria??o da vari?vel a=1 que ser? utilizada posteriomente para 
#     juntar com arquivo de n?mero de unidades de consumo (familias) expandido

soma_final <- rbind( soma_final_2 ,
                     soma_final_3 
                     ) # [1]
rm(soma_final_2, soma_final_3)

soma_final <-
  transform( soma_final ,
             a = 1) # [2]

# Calculando a despesa m?dia mensal de cada grupo de c?digos, segundo cada n?vel, conforme consta no tradutor

merge2 <- data.frame( soma_final , soma_familia=soma_familia )
merge2 <- 
  transform( merge2 ,
             media_mensal = round( soma / soma_familia , 2 ) )

# Leitura do arquivo de ?ndice que determina a posi??o que cada linha deve ficar na tabela final
# O arquivo de ?ndice ? apenas um arquivo auxiliar, criado para associar os resultados gerados com a ordem de apresentacao
# da tabela de resultados

# "....." indica a pasta/diret?rio de trabalho no HD local separados por "/"
# onde se encontram os arquivos .xls dos ?ndices das tabelas
# Exemplo: setwd("c:/POF2018/Memoria_de_Calculo_aaaammdd/Mem?ria de C?lculo/")

setwd(".....") # ..... ? o caminho onde se encontra a pasta "/Memoria_de_Calculo_aaaammdd/Mem?ria de C?lculo/"

indice_rendimento <-
  readxl::read_excel("indice_Rendimento.xls")

# Juntando o arquivo das despesas m?dias mensais de cada grupo de c?digos com o arquivo de ?ndice,
# para organizar os itens da tabela

merge3 <- merge(merge2,indice_rendimento, by.x="nivel", by.y="NIVEL")
merge3 <- 
  merge3[ order( merge3$Indice ) , c(6,1,7,5) ] 


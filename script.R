########[AD-UFPE-2019]#######
########Antonio Fernandes
########Lista 4


##QUESTAO 2

##Abrindo a base de dados de matriculas do censo escolar de 2016

library(ffbase) ##Carregando o pacote ffbase

getwd() ##Verificando o repositório

setwd("~/Dados/Dados_Encontro/dados_encontro_1_ufpe") ##Configurando base onde se encontra os dados

matriculas <- read.csv.ffdf(file = "MATRICULA_NORDESTE.csv", sep = "|", fill = TRUE, colClasses = NA, header = TRUE, first.rows = 1000, next.rows = 50000) ##Abrindo a base de matriculas

##selecionando apenas PE

names(matriculas) ##identificando os nomes do banco matriculas

matriculas_df <- as.data.frame(matriculas) ##transformando em data.frame

matriculas_pe <- subset(matriculas_df, matriculas_df$CO_UF == '26') ##seleconando apenas Pernambuco

rm(matriculas, matriculas_df) ##removendo as outras bases referentes as matriculas

##Selecionando idade especifica dos alunos

matriculas_pe_c <- matriculas_pe[ which (matriculas_pe$NU_IDADE >= 1 & matriculas_pe$NU_IDADE <= 25),] # selecionando os alunos com idade maior ou igual a 1 ano e inferior ou igual a 25 anos, 
#com base na idade calculada pelo ano de nascimento do aluno

rm(matriculas_pe) ## removendo a base anterior

##Abrindo banco de docentes

docentes <- read.csv.ffdf(file = "DOCENTES_NORDESTE.csv", sep = "|", fill = TRUE, colClasses = NA, header = TRUE, first.rows = 1000, next.rows = 50000) ##Abrindo a base de docentes

docentes <- as.data.frame(docentes) #transformando docentes em dataframe

##selecionando apenas docentes de Pernambuco

docentes_pe <- subset(docentes, docentes$CO_UF == '26') #selecionando apenas os docentes de Pernambuco

rm(docentes) #removendo banco docentes

##selecionando idade especifica dos docentes

docentes_pe_c <- docentes_pe[ which (docentes_pe$NU_IDADE >= 18 & docentes_pe$NU_IDADE <= 70),] # selecionando os docentes com idade maior ou igual a 18 anos e inferior ou igual a 70 anos, 
#com base na idade calculada pelo ano de nascimento do docente

rm(docentes_pe) ##removendo base docentes_pe

#eliminando duplicidades no banco de docentes

install.packages("dplyr") ##instalando pacote dplyr
library("dplyr") ##abrindo o pacote dplyr

uni_docente <- docentes_pe_c[!duplicated(docentes_pe_c$CO_PESSOA_FISICA), ] ##por meio do código do docente, percebe-se que ha docentes repetidos. Por isso, utilizei este comando para
##remover as linhas com o código de docente duplicado.

uni_docente$N <- 1 ##criando uma varíavel com o valor 1, que ira permitir que eu some essa variavel por codigo do municipio, obtendo assim o total de docentes por cidade do estado

doc_cidade <- uni_docente %>%
  select(N, CO_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(N = sum(N))   ##Com esse comando, eu somei os valores da variavel que criei anteriormente por municipio. Com isso eu tenho a quantidade de docentes por municipio


##eliminando duplicidades no banco de alunos

uni_daluno <- matriculas_pe_c[!duplicated(matriculas_pe_c$ID_MATRICULA), ] ##por meio do ID de matricula,irei verificar se existem matrículas repetidas. Por isso, utilizei este comando para
##remover as linhas com o código de matricula duplicado.e possivel verificar que nao ha duplicidade

uni_daluno$N <- 1 ##criando uma varíavel com o valor 1, que ira permitir que eu some essa variavel por codigo do municipio, obtendo assim o total de alunos por cidade do estado

alun_cidade <- uni_daluno %>%
  select(N, CO_MUNICIPIO) %>%
  group_by(CO_MUNICIPIO) %>%
  summarise(N = sum(N))   ##Com esse comando, eu somei os valores da variavel que criei anteriormente por municipio. Com isso eu tenho a quantidade de alunoss por municipio


  ##renomeando as variaveis dos dois bancos criados

alun_cidade$N_Alunos <- alun_cidade$N
alun_cidade$N <- NULL

doc_cidade$N_docentes <- doc_cidade$N
doc_cidade$N <- NULL

    ##Mergindo os dois bancos por meio do codigo do municipio

banco_mun <- merge(alun_cidade, doc_cidade)

    ##Abrindo banco do atlas brasil (PNUD)

IDH <- read_excel("AtlasBrasil_Consulta.xlsx")

    ##Criando coluna com mesmo nome do banco

IDH$CO_MUNICIPIO <- IDH$Código
IDH$Código <- NULL

    ##Mergindo as duas bases

banco <- merge(banco_mun, IDH)


##Criando variavel com o número de alunos por docente
banco$alu_doc <- banco_mun$N_Alunos/banco_mun$N_docentes
banco$alu_doc <- round(banco$alu_doc, 2) ##arredondando valor para duas casas decimais


#####Estatisticas descritivas

mean(banco$alu_doc)
##A média de alunos por docentes nos 185 municipios do estado é de 21,94 alunos para cada
##docente.

fivenum(banco$alu_doc)
##O comando fivenum apresenta um sumário da variável. O menor valor de alunos para cada docente
##no estado e de 13,71, o primeiro quartil tem um valor de 19,79 alunos por docente, a mediana da 
##distribuicao e de 22,03, o terceiro quartil tem valor de 24,22 e o maior valor de alunos
##para cada docente e de 30,39. Esses valores podem ser apresentados em um boxplot

boxplot(banco$alu_doc, outline = TRUE)
##por meio do boxplot, e possivel observar que nao ha valores extremos na distribuicao (outliers)

###Municipio com maior numero de alunos por docente

arrange(banco, desc(alu_doc)) ##Por meio desse comando é possível ordenar o banco por ordem
##descendente da quantidade de alunos por docente. Assim, verifica-se que o municipio que
##apresenta o maior valor de alunos por docente é Moreno, com 30,39 alunos por professor e
##com um IDH de 0,652 (considerado um desenvolvimeno medio). O municipio possui 12916 alunos
##e 425 docentes. De acordo com o IBGE, Moreno tem uma populacao total de 62263 pessoas.


##Correlacao

cor.test(banco$alu_doc, banco$`IDHM 2010`)
##O valor obitido da correlacao de pearson entre a variavel com a quantidade de alunos por docente
##e o IDH do municipio apresentou um valor quase igual a 0 (0,03), mostrando uma associacao 
##bastante fraca entre as variaveis. Alem disso, o p-valor obtido foi de 0,641 nao sendo possivel
##rejeitar a hipotese nula de que nao ha associacao entre as variaveis (pvalor>0,05)

##Salvando banco em .rdata

save(banco, file = "banco.RData")

##Grafico de dispersao

library("ggplot2")
library("ggrepel")

banco$idh <- banco$`IDHM 2010`##Ajustando nome da variavel IDH (criando nova variavel)

ggplot(banco, aes(idh, alu_doc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = 0.5) +
  theme_minimal()  ##plotando o grafico de dispersao 

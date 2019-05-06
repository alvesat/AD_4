########[AD-UFPE-2019]#######
########Antonio Fernandes
########Lista 4


##QUESTAO 2

##Abrindo a base de dados do censo escolar de 2016

library(ffbase) ##Carregando o pacote ffbase

getwd() ##Verificando o repositório

setwd("~/Dados/Dados_Encontro/dados_encontro_1_ufpe") ##Configurando base onde se encontra os dados

matriculas <- read.csv.ffdf(file = "MATRICULA_NORDESTE.csv", sep = "|", fill = TRUE, colClasses = NA, header = TRUE, first.rows = 1000, next.rows = 50000) ##Abrindo a base de matriculas

names(matriculas) ##identificando os nomes do banco matriculas

matriculas_df <- as.data.frame(matriculas) ##transformando em data.frame

matriculas_pe <- subset(matriculas_df, matriculas_df$CO_UF == '26') ##seleconando apenas Pernambuco







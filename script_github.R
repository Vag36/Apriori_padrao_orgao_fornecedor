# Selecionando o diretório de trabalho
setwd("C:\\diretorio")

# Pacotes necessários para a análise
library(dplyr)
library(arules)
library(arulesViz)
library(readxl)

# Carregando os dados
base <- read_excel('dataset.xlsx')

# Manipulando os dados
base2 <- subset(base, EXERCICIO >= '2015')
pag_lista <- split(base2$CPF_CNPJ_CREDOR, f=factor(base2$CODIGO_JURISDICIONADO))
transacoes <- as(pag_lista, "transactions")

# O comando abaixo irá aplicar a função "apriori" (algortimo de regra de associção do pacote "arules").
regras <- apriori(transacoes, parameter = list(support = 0.025, confidence = 0.7))
regras2 <- subset(regras, lift > 1.0)
regras2 <- sort(regras2, by = "confidence")

# Removendo as regras duplicadas
gi <- generatingItemsets(regras2)
d <- which(duplicated(gi))

# Gravando o objeto Regras2 sem as regras redundantes
regras2 <- regras2[-d]

# Graficos das regras de associacao geradas:
plot(regras2, method = 'graph', engine = 'htmlwidget')
plot(regras2, method = 'scatter', engine = 'htmlwidget')

# Salvar resultado em csv
write(regras2, file = 'C:/Users/vagodoy/Desktop/RA_TI/versao02/RAs.csv', sep = ';', col.names = NA)
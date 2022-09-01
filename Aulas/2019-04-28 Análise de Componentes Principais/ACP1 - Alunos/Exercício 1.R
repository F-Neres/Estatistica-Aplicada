
install.packages('foreign')
install.packages('psych')
install.packages('stats')
install.packages('dplyr')
install.packages('corrplot')
install.packages('mvnormtest')
install.packages('GPArotation')

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

# Entrada dos dados e extração das componentes principais com a matriz de correlação 
dados <- read.csv('C:/Users/logonlb/Desktop/ACP1 - Alunos/dados-poluicao.csv', sep = ';', dec = ',')
glimpse(dados)

# Teste de esfericidade de Bartllet (nega-se H0, então os dados são correlacionados)
bartlett.test(dados)

# Ajuste do modelo - escolha do número de componentes
ajuste1.cov <- princomp(dados, cor=FALSE)
ajuste1.cor <- princomp(dados, cor=TRUE)
# Impressão da variância explicada (4 por desvio-padrão, 3 por variância acumulada)
summary(ajuste1.cov) #(1 componente, não utilizar SD/kaiser)
summary(ajuste1.cor) #(4 componentes)
# Scree Plot (4 componentes)
plot(ajuste1.cov, type="lines") #(2 componentes)
plot(ajuste1.cor, type="lines") #(4 componentes)

# Ajuste do modelo sem correlação e número de componentes definido
ajuste2.cov <- principal(dados, covar = TRUE, nfactors = 2)

# Ajuste do modelo com covariância e número de componentes definido
ajuste2.cor <- principal(dados, covar = FALSE, nfactors = 4)

# Interpretação (cargas)
ajuste2.cov
ajuste2.cor

# Comunalidades
ajuste2.cov$communality #(As comunalidades não obedecem percentual)
ajuste2.cor$communality
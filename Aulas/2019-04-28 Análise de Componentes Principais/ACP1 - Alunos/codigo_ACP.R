###############################################################################################
#                         An?lise de Componentes Principais
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("D:/UNICID/An?lise de Componentes Principais/ACP1 - Alunos")

# Entrada dos dados e extra??o das componentes principais com a matriz de correla??o 
dados <- read.csv('dados-evaporador-industrial.csv', sep = ';', dec = ',')
glimpse(dados)

# Teste de esfericidade de Bartllet
bartlett.test(dados)

# Ajuste do modelo - escolha do n?mero de componentes
ajuste1 <- princomp(dados, cor=TRUE)
# Impress?o da vari?ncia explicada
summary(ajuste1) 
# Scree Plot
plot(ajuste1, type="lines") 

# Ajuste do modelo com n?mero de componentes definido
ajuste2 <- principal(dados, covar = FALSE, nfactors = 3)

# Interpreta??o (correla??es)
ajuste2

# Comunalidades
ajuste2$communality

# Gr?ficos
library(ggfortify)
autoplot(prcomp(scale(dados)))
autoplot(prcomp(scale(dados)), data = dados, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)







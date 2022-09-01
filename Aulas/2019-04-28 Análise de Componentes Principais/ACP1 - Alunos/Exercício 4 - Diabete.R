###############################################################################################
#                         Análise de Componentes Principais - Exercício 4
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("D:/UNICID/Análise de Componentes Principais/ACP1 - Alunos")

# Entrada dos dados e extração das componentes principais com a matriz de correlação 
dados <- read.csv('dados-mc.csv', sep = ';', dec = ',')

diabetes[,1:8] <- na.omit(diabetes[,1:8])
glimpse(diabetes[,1:8])

##   A)Análise dos componentes principais.

# Teste de esfericidade de Bartllet
bartlett.test(diabetes[,1:8])
#Significante há correlação entre as variáveis.

# Ajuste do modelo - escolha do número de componentes; correlação = verdadeiro (ao invés de covariância)
ajuste1 <- princomp(diabetes[,1:8], cor=TRUE)
# Impressão da variância explicada
summary(ajuste1) 
# Scree Plot
plot(ajuste1, type="lines")
#Critério de Kaiser (autovalor acima de 1): 3 fatores
#Variância agregada (cumulative proportion > 0,7): 4 fatores


# Ajuste do modelo com número de componentes definido
ajuste2 <- principal(diabetes[,1:8], covar = FALSE, nfactors = 3)

# Interpretação (correlações)
ajuste2
summary(ajuste2)

#RC2: Gravidez e Idade
#RC1: Pressão sanguínea, espessura da pele e IMC
#RC3: Glicose, insulina e Diabete na família


# Comunalidades
ajuste2$communality



## Regressão logística utilizando os escores.

ajuste2$scores

diabetes$comp1 <- ajuste2$scores[,1]
diabetes$comp2 <- ajuste2$scores[,2]
diabetes$comp3 <- ajuste2$scores[,3]
# *isso altera os dados originais*

tabela<- diabetes%>%
  group_by(Outcome) %>%
  summarise(media_c1 = mean(comp1, na.rm = TRUE),
            media_c2 = mean(comp2, na.rm = TRUE),
            media_c3 = mean(comp3, na.rm = TRUE))
tabela

#Regressões

modelo.logit<- glm(Outcome ~ comp1+comp2+comp3, data = diabetes,family=binomial(link="logit"))
summary(modelo.logit)
#Log.chance de diabetes = -0.76021 +0.74484*0.74484 +0.31860*comp2 +0.88539*0.88539
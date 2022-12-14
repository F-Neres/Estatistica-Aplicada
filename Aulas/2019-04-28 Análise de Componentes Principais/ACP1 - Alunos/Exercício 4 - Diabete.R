###############################################################################################
#                         An?lise de Componentes Principais - Exerc?cio 4
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
dados <- read.csv('dados-mc.csv', sep = ';', dec = ',')

diabetes[,1:8] <- na.omit(diabetes[,1:8])
glimpse(diabetes[,1:8])

##   A)An?lise dos componentes principais.

# Teste de esfericidade de Bartllet
bartlett.test(diabetes[,1:8])
#Significante h? correla??o entre as vari?veis.

# Ajuste do modelo - escolha do n?mero de componentes; correla??o = verdadeiro (ao inv?s de covari?ncia)
ajuste1 <- princomp(diabetes[,1:8], cor=TRUE)
# Impress?o da vari?ncia explicada
summary(ajuste1) 
# Scree Plot
plot(ajuste1, type="lines")
#Crit?rio de Kaiser (autovalor acima de 1): 3 fatores
#Vari?ncia agregada (cumulative proportion > 0,7): 4 fatores


# Ajuste do modelo com n?mero de componentes definido
ajuste2 <- principal(diabetes[,1:8], covar = FALSE, nfactors = 3)

# Interpreta??o (correla??es)
ajuste2
summary(ajuste2)

#RC2: Gravidez e Idade
#RC1: Press?o sangu?nea, espessura da pele e IMC
#RC3: Glicose, insulina e Diabete na fam?lia


# Comunalidades
ajuste2$communality



## Regress?o log?stica utilizando os escores.

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

#Regress?es

modelo.logit<- glm(Outcome ~ comp1+comp2+comp3, data = diabetes,family=binomial(link="logit"))
summary(modelo.logit)
#Log.chance de diabetes = -0.76021 +0.74484*0.74484 +0.31860*comp2 +0.88539*0.88539
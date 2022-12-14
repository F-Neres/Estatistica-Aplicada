###############################################################################################
#                         An?lise de Componentes Principais - Exerc?cio 3
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

dados_mc[,4:24] <- na.omit(dados_mc[,4:24])
glimpse(dados_mc[,4:24])

##   A)An?lise dos componentes principais.

# Teste de esfericidade de Bartllet
bartlett.test(dados_mc[,4:24])
#Significante h? correla??o entre as vari?veis.

# Ajuste do modelo - escolha do n?mero de componentes; correla??o = verdadeiro (ao inv?s de covari?ncia)
ajuste1 <- princomp(dados_mc[,4:24], cor=TRUE)
# Impress?o da vari?ncia explicada
summary(ajuste1) 
# Scree Plot
plot(ajuste1, type="lines")
#Crit?rio de Kaiser (autovalor acima de 1): 4 fatores
#Vari?ncia agregada (cumulative proportion > 0,7): 2 fatores


# Ajuste do modelo com n?mero de componentes definido
ajuste2 <- principal(dados_mc[,4:24], covar = FALSE, nfactors = 4)

# Interpreta??o (correla??es)
ajuste2
summary(ajuste2)

# Comunalidades
ajuste2$communality

# Gr?ficos
install.packages('ggfortify')
library(ggfortify)
autoplot(prcomp(scale(dados_mc[,4:24])))
autoplot(prcomp(scale(dados_mc[,4:24])), data = dados, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


## Regress?o utilizando os escores.

ajuste2$scores

dados_mc$comp1 <- ajuste2$scores[,1]
dados_mc$comp2 <- ajuste2$scores[,2]
dados_mc$comp3 <- ajuste2$scores[,3]
dados_mc$comp4 <- ajuste2$scores[,4]
# *isso altera os dados originais*

tabela<- dados_mc%>%
  group_by(Category) %>%
  summarise(media_c1 = mean(comp1, na.rm = TRUE),
            media_c2 = mean(comp2, na.rm = TRUE),
            media_c3 = mean(comp3, na.rm = TRUE),
            media_c4 = mean(comp4, na.rm = TRUE))
tabela

#Regress?es

modelo2 <- lm(comp2 ~ Category, data = dados_mc)
summary(modelo2)
#"caf? da manh?" e "peixe e frango" n?o foram significativos; os coeficientes destes podem ser considerados zero e suas ocorr?ncias consideradas igual ? de "carne vermelha", que ? o intercepto.


modelo2 <- lm(comp1 ~ Category, data = dados_mc)
summary(modelo1)
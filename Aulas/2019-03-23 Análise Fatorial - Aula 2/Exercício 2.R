###############################################################################################
#                                   An?lise Fatorial - Exerc?cio 2
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("c:/Users/logonlb/Desktop/2019-03-23 An?lise Fatorial - Aula 2")

dados = read.csv('c:/Users/logonlb/Desktop/2019-03-23 An?lise Fatorial - Aula 2/dados_animais.csv', sep = ';', dec = ',')
dados = na.omit(dados)
glimpse(dados)

# Crit?rio para o uso
nrow(dados) #n=150; mais de 50 observa??es
nrow(dados)/ncol(dados) #5.357143 casos para cada vari?vel
corrplot(cor(dados), order = 'hclust', tl.col = 'black', tl.cex = 0.75)
KMO(fatores.1) #80% da variabilidade vem de fator comum - ?timo
bartlett.test(dados) #hipot?se rejeitada (p-value < 2.2e-16), as vari?veis s?o correlacionadas

#Adequado



########## Sem Rota??o
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # vari?ncias (autovalores)
loadings(fit_acp) # cargas fatoriais
plot(fit_acp,type="lines", main = 'Scree-Plot') # scree plot: 2 fatores




########## Componentes principais - Rota??o Varimax

fit_acp_var <- principal(na.omit(dados), nfactors=7, rotate="varimax") #nfactors determina o n?mero de fatores
fit_acp_var # print results
write.table(fit_acp_var$loadings, 'cargas_ex2.csv', sep = ';', dec = ',')

#RC3 explica AR1, 6 9 12 15 19 20 25 27
#RC4 explica...
#RC2 ...

# comunalidades
fit_acp_var$communality

# vari?ncia n?o explicada
fit_acp_var$uniquenesses

# escores dos fatores
fit_acp_var$scores



########## Eixos principais - Rota??o Varimax

fit_ep_var <- principal(na.omit(dados), nfactors=7, rotate="pa") #nfactors determina o n?mero de fatores
fit_ep_var # print results
write.table(fit_ep_var$loadings, 'cargas_ex2.csv', sep = ';', dec = ',')

#RC3 explica AR1, 6 9 12 15 19 20 25 27
#RC4 explica...
#RC2 ...

# comunalidades
fit_ep_var$communality

# vari?ncia n?o explicada
fit_ep_var$uniquenesses

# escores dos fatores
fit_acp_var$scores




########## M?xima Verossimilhan?a - Rota??o Varimax

fit_ep_var <- principal(na.omit(dados), nfactors=7, rotate="varimax") #nfactors determina o n?mero de fatores
fit_ep_var # print results
write.table(fit_ep_var$loadings, 'cargas_ex2.csv', sep = ';', dec = ',')

#RC3 explica AR1, 6 9 12 15 19 20 25 27
#RC4 explica...
#RC2 ...

# comunalidades
fit_ep_var$communality

# vari?ncia n?o explicada
fit_ep_var$uniquenesses

# escores dos fatores
fit_acp_var$scores









########## Rota??o Oblimin
# Extra??o de fatores por componentes principais - rota??o oblimin
fit <- principal(Thurstone.33, nfactors=2, rotate="oblimin")

# Extra??o de fatores por eixos principais - rota??o oblimin
pa2 <- fa(Thurstone.33, 2,rotate="oblimin", fm="pa")

# Extra??o de fatores por m?xima verossimilhan?a - rota??o oblimin
mle2 <- fa(Thurstone.33, 2,rotate="oblimin", fm="mle")

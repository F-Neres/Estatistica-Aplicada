###############################################################################################
#                                   Análise Fatorial - Exercício 2
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("c:/Users/logonlb/Desktop/2019-03-23 Análise Fatorial - Aula 2")

dados = read.csv('c:/Users/logonlb/Desktop/2019-03-23 Análise Fatorial - Aula 2/dados_animais.csv', sep = ';', dec = ',')
dados = na.omit(dados)
glimpse(dados)

# Critério para o uso
nrow(dados) #n=150; mais de 50 observações
nrow(dados)/ncol(dados) #5.357143 casos para cada variável
corrplot(cor(dados), order = 'hclust', tl.col = 'black', tl.cex = 0.75)
KMO(fatores.1) #80% da variabilidade vem de fator comum - ótimo
bartlett.test(dados) #hipotése rejeitada (p-value < 2.2e-16), as variáveis são correlacionadas

#Adequado



########## Sem Rotação
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # variâncias (autovalores)
loadings(fit_acp) # cargas fatoriais
plot(fit_acp,type="lines", main = 'Scree-Plot') # scree plot: 2 fatores




########## Componentes principais - Rotação Varimax

fit_acp_var <- principal(na.omit(dados), nfactors=7, rotate="varimax") #nfactors determina o número de fatores
fit_acp_var # print results
write.table(fit_acp_var$loadings, 'cargas_ex2.csv', sep = ';', dec = ',')

#RC3 explica AR1, 6 9 12 15 19 20 25 27
#RC4 explica...
#RC2 ...

# comunalidades
fit_acp_var$communality

# variância não explicada
fit_acp_var$uniquenesses

# escores dos fatores
fit_acp_var$scores



########## Eixos principais - Rotação Varimax

fit_ep_var <- principal(na.omit(dados), nfactors=7, rotate="pa") #nfactors determina o número de fatores
fit_ep_var # print results
write.table(fit_ep_var$loadings, 'cargas_ex2.csv', sep = ';', dec = ',')

#RC3 explica AR1, 6 9 12 15 19 20 25 27
#RC4 explica...
#RC2 ...

# comunalidades
fit_ep_var$communality

# variância não explicada
fit_ep_var$uniquenesses

# escores dos fatores
fit_acp_var$scores




########## Máxima Verossimilhança - Rotação Varimax

fit_ep_var <- principal(na.omit(dados), nfactors=7, rotate="varimax") #nfactors determina o número de fatores
fit_ep_var # print results
write.table(fit_ep_var$loadings, 'cargas_ex2.csv', sep = ';', dec = ',')

#RC3 explica AR1, 6 9 12 15 19 20 25 27
#RC4 explica...
#RC2 ...

# comunalidades
fit_ep_var$communality

# variância não explicada
fit_ep_var$uniquenesses

# escores dos fatores
fit_acp_var$scores









########## Rotação Oblimin
# Extração de fatores por componentes principais - rotação oblimin
fit <- principal(Thurstone.33, nfactors=2, rotate="oblimin")

# Extração de fatores por eixos principais - rotação oblimin
pa2 <- fa(Thurstone.33, 2,rotate="oblimin", fm="pa")

# Extração de fatores por máxima verossimilhança - rotação oblimin
mle2 <- fa(Thurstone.33, 2,rotate="oblimin", fm="mle")

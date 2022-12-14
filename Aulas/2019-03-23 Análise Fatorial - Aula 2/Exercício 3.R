###############################################################################################
#                                   An?lise Fatorial - Exerc?cio 3
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("c:/Users/logonlb/Desktop/2019-03-23 An?lise Fatorial - Aula 2")

dados.pre = read.csv('c:/Users/logonlb/Desktop/2019-03-23 An?lise Fatorial - Aula 2/dados_mc.csv', sep = ';', dec = '.')
dados.pre = na.omit(dados.pre)
dados = dados.pre[,4:24]
glimpse(dados)

# a)Verifique se um modelo de an?lise fatorial ? adequado. 
# Crit?rio para o uso
nrow(dados) #n=260; mais de 50 observa??es
nrow(dados)/ncol(dados) #12.38095 casos para cada vari?vel
corrplot(cor(dados), order = 'hclust', tl.col = 'black', tl.cex = 0.75)
KMO(fatores.1) #90% da variabilidade vem de fator comum - ?timo
bartlett.test(dados) #hipot?se rejeitada (p-value < 2.2e-16), as vari?veis s?o correlacionadas



# b) Por meio dos crit?rios de Kaiser, vari?ncia acumulada e Scree Plot, escolha uma n?mero de fatores para o modelo. 
########## Sem Rota??o
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # vari?ncias (autovalores)
#Quatro fatores pelo crit?rio de Kaiser: Standard deviation acima de 1.
#Dois fatores j? atingem 0.705 de vari?ncia acumulada.
loadings(fit_acp) # cargas fatoriais.
plot(fit_acp,type="lines", main = 'Scree-Plot') # scree plot: 4 fatores.



# c) Utilizando a extra??o por Componentes Principais, verifique qual o melhor m?todo de rota??o para a an?lise (varimax ou oblimin).
fit_acp_obl <- principal(dados, nfactors=4, rotate="oblimin")
fit_acp_obl # print results
#With component correlations of
#Todas as correla??es estiveram entre -0.3 e +0.3. ? aconselh?vel utilizar varimax


# d) Com o m?todo de rota??o definido, interprete os fatores.
########## Componentes principais - Rota??o Varimax

fit_acp_var <- principal(na.omit(dados), nfactors=4, rotate="varimax") #nfactors determina o n?mero de fatores
fit_acp_var # print results
write.table(fit_acp_var$loadings, 'cargas_ex3.csv', sep = ';', dec = ',')

#RC1 explica Calories, Calories.from.Fat, Total.Fat, Total.Fat....Daily.Value., Saturated.Fat, Saturated.Fat....Daily.Value., Cholesterol, Cholesterol....Daily.Value., Sodium, Sodium....Daily.Value., Dietary.Fiber, Dietary.Fiber....Daily.Value., Protein, Iron....Daily.Value.
#RC2 explica Trans.Fat, Carbohydrates, Carbohydrates....Daily.Value., Sugars e Calcium....Daily.Value..
#RC3 explica Vitamin.A....Daily.Value.
#RC4 explica Vitamin.C....Daily.Value.




write.table(fit_acp_obl$loadings, 'cargas_ex2.csv', sep = ';', dec = ',')

#RC3 explica AR1, 6 9 12 15 19 20 25 27
#RC4 explica...
#RC2 ...

# comunalidades
fit_acp_obl$communality

# vari?ncia n?o explicada
fit_acp_obl$uniquenesses

# escores dos fatores
fit_acp_obl$scores

#Justifique.
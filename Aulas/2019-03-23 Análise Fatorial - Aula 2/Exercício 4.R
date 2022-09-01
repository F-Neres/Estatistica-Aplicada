###############################################################################################
#                                   Análise Fatorial - Exercício 4
###############################################################################################


install.packages('foreign')
library(foreign)

install.packages('psych')
library(psych)

install.packages('stats')
library(stats)

install.packages('dplyr')
library(dplyr)

install.packages('corrplot')
library(corrplot)

install.packages('mvnormtest')
library(mvnormtest)

install.packages('GPArotation')
library(GPArotation)

setwd("c:/Users/logonlb/Desktop/2019-03-23 Análise Fatorial - Aula 2")

dados.pre = read.csv('c:/Users/logonlb/Desktop/2019-03-23 Análise Fatorial - Aula 2/Wine_data.csv', sep = ';', dec = ',')
dados = na.omit(dados.pre)
glimpse(dados)

#a) Verifique se a análise fatorial é adequada para o estudo.
nrow(dados) #n=1599; mais de 50 observações
nrow(dados)/ncol(dados) #133.25 casos para cada variável
corrplot(cor(dados), order = 'hclust', tl.col = 'black', tl.cex = 0.75)
KMO(dados) #47% da variabilidade vem de fator comum - abaixo do limite
bartlett.test(dados) #hipotése rejeitada (p-value < 2.2e-16), as variáveis são correlacionadas

#b) Defina um número de fatores para o modelo.
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # variâncias (autovalores)
#4 fatores pelo critério de Kaiser: Standard deviation acima de 1.
#4 fatores já atingem 0.705 de variância acumulada.
loadings(fit_acp) # cargas fatoriais.
plot(fit_acp,type="lines", main = 'Scree-Plot') # scree plot: 6 fatores.

#4 fatores


#c) Utilizando o método de Componentes Principais, avalie qual tipo de rotação é mais indicado aos dados (oblimin ou varimax).
fit_acp_obl <- principal(dados, nfactors=4, rotate="oblimin")
fit_acp_obl # print results
#With component correlations of
#Todas as correlações estiveram entre -0.3 e +0.3. É aconselhável utilizar varimax
fit_acp_var <- principal(na.omit(dados), nfactors=4, rotate="varimax") #Modelo varimax com componentes principais


#d) Indique a variável mais e a menos explicada pelos fatores em estudo. 
fit_acp_obl$communality
#Mais explicada: fixed.acidity
#Menos explicada: residual.sugar


#e) Interprete os fatores obtidos.
fit_acp_var # print results
write.table(fit_acp_var$loadings, 'cargas_ex4.csv', sep = ';', dec = ',')

##RC1 explica:
#fixed.acidity
#citric.acid
#density
#pH

##RC2 explica:
#volatile.acidity
#alcohol
#quality

##RC3 explica:
#residual.sugar
#free.sulfur.dioxide
#total.sulfur.dioxide

##RC4 explica:
#chlorides
#sulphates


#f) Compare os resultados obtidos com as extrações por eixos principais e máxima verossimilhança.

####Eixos Principais
fit_aep_var <- fa(dados, 4,rotate="varimax", fm="pa")
fit_aep_var # print results
write.table(fit_aep_var$loadings, 'cargas_ex4ep.csv', sep = ';', dec = ',')

##PA1 explica:
#fixed.acidity
#citric.acid
#density
#pH

##PA2 explica:
#volatile.acidity
#alcohol
#quality

##PA3 explica:
#residual.sugar
#free.sulfur.dioxide
#total.sulfur.dioxide


##PA4 explica:
#chlorides
#sulphates


####Máxima Verossimilhança
fit_amv_var <- fa(dados, 4,rotate="varimax", fm="mle")
fit_amv_var
write.table(fit_amv_var$loadings, 'cargas_ex4mv.csv', sep = ';', dec = ',')

##ML4 explica:
#fixed.acidity
#volatile.acidity
#citric.acid
#pH
#sulphates

##ML1
#chlorides
#alcohol
#quality

##ML3
#free.sulfur.dioxide
#total.sulfur.dioxide

##ML2
#residual.sugar
#density

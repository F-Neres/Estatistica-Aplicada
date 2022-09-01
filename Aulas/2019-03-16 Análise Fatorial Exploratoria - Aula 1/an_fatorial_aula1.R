###############################################################################################
#                           Análise Fatorial Exploratória
###############################################################################################

install.packages("foreign")
install.packages("psych")
install.packages("stats")
install.packages("dplyr")
install.packages("corrplot")
install.packages("mvnormtest")
install.packages("GPArotation")

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("D:/UNICID/Análise Fatorial/1-AFE/Dados")

dados = read.csv('c:/Users/logonlb/Desktop/2019-03-16 Análise Fatorial Exploratoria - Aula 1/dados_personalidade_32.csv', sep = ';', dec = ',')
glimpse(dados)

# Critérios para o uso
nrow(dados) #mais de 50 observações
nrow(dados)/ncol(dados) #10 casos para cada variável
corrplot(cor(dados), order = 'hclust', tl.col = 'black', tl.cex = 0.75)
KMO(dados) #ótimo
bartlett.test(dados) #hipotése rejeitada, as variáveis são correlacionadas

#### Método de extração: Análise de Componentes Principais (não rotacionado) ###
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # variâncias (autovalores)
loadings(fit_acp) # cargas fatoriais 
plot(fit_acp,type="lines", main = 'Scree-Plot') # scree plot

################################################################################


############# Método de extração: eixos principais - sem rotação ###############
fit_ep <- fa(dados, 2, rotate="none", fm = "pa")
fit_ep
################################################################################


################## Componentes Principais com Rotação Varimax ##################
fit_acp_var <- principal(na.omit(dados), nfactors=6, rotate="varimax") #nfactors determina o número de fatores
fit_acp_var # print results

# comunalidades
fit_acp_var$communality

# variância não explicada
fit_acp_var$uniquenesses

# escores dos fatores
fit_acp_var$scores
################################################################################


###### Método de extração: eixos principais - com rotação Varimax ##############
fit_ep <- fa(dados, 6, rotate="varimax", fm = "pa")
fit_ep

# autovalores
fit_ep$e.values

# cargas
fit_ep$loadings

# comunalidades
fit_ep$communality

# variância não explicada
fit_ep$uniquenesses

# escores dos fatores
fit_ep$scores
################################################################################


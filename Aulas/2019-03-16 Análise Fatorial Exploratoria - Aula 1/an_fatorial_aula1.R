###############################################################################################
#                           An?lise Fatorial Explorat?ria
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

setwd("D:/UNICID/An?lise Fatorial/1-AFE/Dados")

dados = read.csv('c:/Users/logonlb/Desktop/2019-03-16 An?lise Fatorial Exploratoria - Aula 1/dados_personalidade_32.csv', sep = ';', dec = ',')
glimpse(dados)

# Crit?rios para o uso
nrow(dados) #mais de 50 observa??es
nrow(dados)/ncol(dados) #10 casos para cada vari?vel
corrplot(cor(dados), order = 'hclust', tl.col = 'black', tl.cex = 0.75)
KMO(dados) #?timo
bartlett.test(dados) #hipot?se rejeitada, as vari?veis s?o correlacionadas

#### M?todo de extra??o: An?lise de Componentes Principais (n?o rotacionado) ###
fit_acp <- princomp(na.omit(dados), cor=TRUE)
summary(fit_acp) # vari?ncias (autovalores)
loadings(fit_acp) # cargas fatoriais 
plot(fit_acp,type="lines", main = 'Scree-Plot') # scree plot

################################################################################


############# M?todo de extra??o: eixos principais - sem rota??o ###############
fit_ep <- fa(dados, 2, rotate="none", fm = "pa")
fit_ep
################################################################################


################## Componentes Principais com Rota??o Varimax ##################
fit_acp_var <- principal(na.omit(dados), nfactors=6, rotate="varimax") #nfactors determina o n?mero de fatores
fit_acp_var # print results

# comunalidades
fit_acp_var$communality

# vari?ncia n?o explicada
fit_acp_var$uniquenesses

# escores dos fatores
fit_acp_var$scores
################################################################################


###### M?todo de extra??o: eixos principais - com rota??o Varimax ##############
fit_ep <- fa(dados, 6, rotate="varimax", fm = "pa")
fit_ep

# autovalores
fit_ep$e.values

# cargas
fit_ep$loadings

# comunalidades
fit_ep$communality

# vari?ncia n?o explicada
fit_ep$uniquenesses

# escores dos fatores
fit_ep$scores
################################################################################


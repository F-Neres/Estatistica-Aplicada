###############################################################################################
#                                   Análise Fatorial
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("D:/UNICID/Análise Fatorial/1-AFE/Dados")

####################### Exemplo habilidades cognitivas ##############################
exemplo_fatores <- as.matrix(Thurstone.33)

# Critério para o uso
KMO(exemplo_fatores) #ótimo

########## Sem Rotação
# Extração de fatores por componentes principais - sem rotação
fit <- principal(Thurstone.33, nfactors=2, rotate="none")

# Extração de fatores por eixos principais - sem rotação
pa2 <- fa(Thurstone.33, 2, rotate="none", fm = "pa")

# Extração de fatores por máxima verossimilhança - sem rotação
mle2 <- fa(Thurstone.33, 2, rotate="none", fm = "mle")

########## Rotação Varimax
# Extração de fatores por componentes principais - rotação varimax
fit <- principal(Thurstone.33, nfactors=2, rotate="varimax")

# Extração de fatores por eixos principais - rotação varimax
pa2 <- fa(Thurstone.33, 2,rotate="varimax", fm="pa")

# Extração de fatores por máxima verossimilhança - rotação varimax
mle2 <- fa(Thurstone.33, 2,rotate="varimax", fm="mle")

########## Rotação Oblimin
# Extração de fatores por componentes principais - rotação oblimin
fit <- principal(Thurstone.33, nfactors=2, rotate="oblimin")

# Extração de fatores por eixos principais - rotação oblimin
pa2 <- fa(Thurstone.33, 2,rotate="oblimin", fm="pa")

# Extração de fatores por máxima verossimilhança - rotação oblimin
mle2 <- fa(Thurstone.33, 2,rotate="oblimin", fm="mle")

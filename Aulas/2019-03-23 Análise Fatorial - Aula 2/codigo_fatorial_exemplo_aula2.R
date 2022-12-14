###############################################################################################
#                                   An?lise Fatorial
###############################################################################################

library(foreign)
library(psych)
library(stats)
library(dplyr)
library(corrplot)
library(mvnormtest)
library(GPArotation)

setwd("D:/UNICID/An?lise Fatorial/1-AFE/Dados")

####################### Exemplo habilidades cognitivas ##############################
exemplo_fatores <- as.matrix(Thurstone.33)

# Crit?rio para o uso
KMO(exemplo_fatores) #?timo

########## Sem Rota??o
# Extra??o de fatores por componentes principais - sem rota??o
fit <- principal(Thurstone.33, nfactors=2, rotate="none")

# Extra??o de fatores por eixos principais - sem rota??o
pa2 <- fa(Thurstone.33, 2, rotate="none", fm = "pa")

# Extra??o de fatores por m?xima verossimilhan?a - sem rota??o
mle2 <- fa(Thurstone.33, 2, rotate="none", fm = "mle")

########## Rota??o Varimax
# Extra??o de fatores por componentes principais - rota??o varimax
fit <- principal(Thurstone.33, nfactors=2, rotate="varimax")

# Extra??o de fatores por eixos principais - rota??o varimax
pa2 <- fa(Thurstone.33, 2,rotate="varimax", fm="pa")

# Extra??o de fatores por m?xima verossimilhan?a - rota??o varimax
mle2 <- fa(Thurstone.33, 2,rotate="varimax", fm="mle")

########## Rota??o Oblimin
# Extra??o de fatores por componentes principais - rota??o oblimin
fit <- principal(Thurstone.33, nfactors=2, rotate="oblimin")

# Extra??o de fatores por eixos principais - rota??o oblimin
pa2 <- fa(Thurstone.33, 2,rotate="oblimin", fm="pa")

# Extra??o de fatores por m?xima verossimilhan?a - rota??o oblimin
mle2 <- fa(Thurstone.33, 2,rotate="oblimin", fm="mle")

##############################################################################################
#
#                                     Análise Discriminante
#                                       Exercício - Cobras
##############################################################################################

# Pacotes para análise
library(foreign)
library(MASS)
library(dplyr)
library(biotools)

# Leitura dos dados
dados_cobras <- read.csv('dados_cobras.csv', sep = ';', dec = ',')
glimpse(dados_cobras)

############################################ Item A ###########################################
plot(x = dados_cobras$Cauda, y = dados_cobras$Focinho, xlab = 'Comprimento da cauda',
     ylab = 'Comprimento do focinho', col = as.integer(dados_cobras$Genero), pch = 20)
legend(x = 120, y = 650, legend = levels(dados_cobras$Genero),
       col = c(1, 2), pch = 20, bty = 'n')

############################################ Item B ###########################################
# Modelo discriminante
discrim_1 <- lda(Genero ~ Idade + Cauda + Focinho, data = dados_cobras)
discrim_1

# Classificando um novo objeto
predict(discrim_1, newdata = data.frame(Idade = 3, Cauda = 140, Focinho = 500))

############################################ Item C ###########################################
# Modelo discriminante
discrim_2 <- qda(Genero ~ Idade + Cauda + Focinho, data = dados_cobras)
discrim_2

# Classificando um novo objeto
predict(discrim_2, newdata = data.frame(Idade = 3, Cauda = 140, Focinho = 500))

############################################ Item D ###########################################

# Verificando acurácia do modelo - Linear
# Percentual de classificações corretas para cada grupo
discrim_1 <- lda(Genero ~ Idade + Cauda + Focinho, data = dados_cobras, na.action = 'na.omit', CV = TRUE)
matriz_class <- table(dados_cobras$Genero, discrim_1$class)
matriz_class
diag(prop.table(matriz_class, 1))
# Percentual total de classificações corretas
sum(diag(prop.table(matriz_class)))

# Verificando acurácia do modelo - Quadrático
# Percentual de classificações corretas para cada grupo
discrim_2 <- qda(Genero ~ Idade + Cauda + Focinho, data = dados_cobras, na.action = 'na.omit', CV = TRUE)
matriz_class_2 <- table(dados_cobras$Genero, discrim_2$class)
matriz_class_2
diag(prop.table(matriz_class_2, 1))
# Percentual total de classificações corretas
sum(diag(prop.table(matriz_class_2)))







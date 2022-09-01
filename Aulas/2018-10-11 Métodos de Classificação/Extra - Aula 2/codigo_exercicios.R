##############################################################################################
#
#                               Análise Discriminante - Exercícios
#
##############################################################################################

# Pacotes para análise
library(foreign)
library(MASS)
library(dplyr)
library(biotools)
library(DiscriMiner)

################################### Exercício 3 ##############################

# Leitura dos dados
dados <- read.csv('F:/UNICID/Análise Discriminante/Dados/dados_insetos.csv', sep = ';', dec = ',')

############ Verificando Suposições

# Verificando tamanho dos grupos
table(dados$Raça)

# Matriz de correlações (suposição de não multicolinearidade)
cor(dados[,2:3])

# Verificando suposição de normalidade
mshapiro.test(t(dados[,-1]))

# Verificando suposição de variâncias iguais
boxM(data = dados[, -1], grouping = dados$Raça)

########### Seleção das variáveis
# Medidas de qualidade
discPower(variables = dados[,2:3], group = dados$Raça)

######## Ajuste das funções discriminantes

# Modelo discriminante
discrim_1 <- desDA(variables = dados[, 2:3], 
                   group = dados$Raça, covar = "within")

# Coeficientes das funções discriminantes
discrim_1$discrivar

# Autovalores das funções discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpretação
discrim_1$discor

############### Avaliação do ajuste
# Testes da significância das funções discriminantes
# Classificação real
Y_real <- dados$Raça

escores <- discrim_1$scores
summary(aov(escores ~ Y_real), test="Wilks")

################################### Exercício 4 ##############################

# Leitura dos dados
dados <- read.csv('F:/UNICID/Análise Discriminante/Dados/dados_cereais.csv', sep = ';', dec = ',') %>%
  select(Fabricante, Calorias, Proteina, Gordura, Sodio, Fibra, Carboidrato, Acucar, Potassio)
glimpse(dados)

############ Verificando Suposições

dados[,-1] <- scale(dados[,-1])

# Verificando tamanho dos grupos
table(dados$Fabricante)

# Matriz de correlações (suposição de não multicolinearidade)
cor(dados[,2:9])

# Verificando suposição de normalidade
mshapiro.test(t(dados[,-1]))

# Verificando suposição de variâncias iguais
boxM(data = dados[, -1], grouping = dados$Fabricante)

########### Seleção das variáveis
# Medidas de qualidade
discPower(variables = dados[,2:9], group = dados$Fabricante)

######## Ajuste das funções discriminantes

# Modelo discriminante
discrim_1 <- desDA(variables = dados[, 2:9], 
                   group = dados$Fabricante, covar = "within")

# Coeficientes das funções discriminantes
discrim_1$discrivar

# Autovalores das funções discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpretação
discrim_1$discor

############### Avaliação do ajuste
# Testes da significância das funções discriminantes
# Classificação real
Y_real <- dados$Fabricante

escores <- discrim_1$scores
summary(manova(escores ~ Y_real), test="Wilks")
summary(manova(escores ~ Y_real),test="Hotelling-Lawley")
summary.aov(manova(escores ~ Y_real),test="Hotelling-Lawley")

############## Avaliando as marcas - item c
# Centróides
dados$DF1 <- discrim_1$scores[,1]
dados$DF2 <- discrim_1$scores[,2]
dados %>% group_by(Fabricante) %>% summarise(C1 = mean(DF1), C2 = mean(DF2))

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes(x = DF1, y = DF2, colour = Fabricante)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()

# Classificação linear
fit <- linDA(variables = dados[, 2:9], group = dados$Fabricante)
fit2 <- linDA(variables = dados[, 2:9], group = dados$Fabricante, validation = 'crossval')
treinamento <- sample(seq_len(nrow(dados)), size = 33) #75% dos dados
teste <- setdiff(seq_len(nrow(dados)), treinamento)
fit3 <- linDA(variables = dados[, 2:9], group = dados$Fabricante,
              validation = 'learntest', learn = treinamento,
              test = teste)
fit$error_rate
fit2$error_rate
fit3$error_rate

# Classificação quadrática - GRUPOS MUITO PEQUENOS
fit <- quaDA(variables = dados[, 2:9], group = dados$Fabricante)
fit2 <- quaDA(variables = dados[, 2:9], group = dados$Fabricante, validation = 'crossval')
treinamento <- sample(seq_len(nrow(dados)), size = 33) #75% dos dados
teste <- setdiff(seq_len(nrow(dados)), treinamento)
fit3 <- quaDA(variables = dados[, 2:9], group = dados$Fabricante,
              validation = 'learntest', learn = treinamento,
              test = teste)
fit$error_rate
fit2$error_rate
fit3$error_rate

################################### Exercício 7 ##############################

# Leitura dos dados
dados <- read.csv('F:/UNICID/Análise Discriminante/Dados/dados_alunos.csv', sep = ';', dec = ',') %>%
  select(Grupo, Nota.técnica, Histórico.escolar)

dados %>% 
  group_by(Grupo) %>% 
  summarise(media_NT = mean(Nota.técnica, na.rm = TRUE), 
            media_HE = mean(Histórico.escolar, na.rm = TRUE),
            desv_NT = sd(Nota.técnica, na.rm = TRUE),
            desv_HE = sd(Histórico.escolar, na.rm = TRUE))

dados_1 <- dados %>% filter(Grupo ==1)
cov(dados_1[,-1])
dados_2 <- dados %>% filter(Grupo ==2)
cov(dados_2[,-1])
dados_3 <- dados %>% filter(Grupo ==3)
cov(dados_3[,-1])

############ Verificando Suposições

# Verificando tamanho dos grupos
table(dados$Grupo)

# Matriz de correlações (suposição de não multicolinearidade)
cor(dados[,2:3])

# Verificando suposição de variâncias iguais
boxM(data = dados[, -1], grouping = dados$Grupo)

# Verificando suposição de normalidade
mshapiro.test(t(dados[,-1]))

########### Seleção das variáveis
# Medidas de qualidade
discPower(variables = dados[,2:3], group = dados$Grupo)
dados %>% group_by(Grupo) %>% summarise(media_nota = mean(Nota.técnica, na.rm = TRUE),
                                        media_historica = mean(Histórico.escolar, na.rm = TRUE))

######## Ajuste das funções discriminantes

# Modelo discriminante
discrim_1 <- desDA(variables = dados[, 2:3], 
                   group = dados$Grupo, covar = "within")

# Coeficientes das funções discriminantes
discrim_1$discrivar

# Autovalores das funções discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpretação
discrim_1$discor

############### Avaliação do ajuste
# Testes da significância das funções discriminantes
# Classificação real
Y_real <- dados$Grupo

escores <- discrim_1$scores
summary(manova(escores ~ Y_real), test="Wilks")

# Centróides
dados$DF1 <- discrim_1$scores[,1]
dados$DF2 <- discrim_1$scores[,2]
dados %>% group_by(Grupo) %>% summarise(C1 = mean(DF1), C2 = mean(DF2))

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes(x = DF1, y = DF2, colour = factor(Grupo))) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()

############### Classificação - Funções de classificação

fit <- linDA(variables = dados[, 2:3], 
             group = dados$Grupo)

# Funções de classificação
fit$functions

# Matriz de confusão
fit$confusion

# Taxa de erro
fit$error_rate

# Classificação prevista
Y_previsto <- fit$classification

# Classificando um novo objeto
mean(dados$Nota.técnica, na.rm = TRUE)
mean(dados$Histórico.escolar, na.rm = TRUE)
sd(dados$Nota.técnica, na.rm = TRUE)
sd(dados$Histórico.escolar, na.rm = TRUE)

classify(fit, newdata = data.frame(Nota.técnica = 15, Histórico.escolar = 8))$pred_class

fit2 <- quaDA(variables = dados[, 2:3], group = dados$Grupo, validation = 'crossval')
treinamento <- sample(seq_len(nrow(dados)), size = 47) #75% dos dados
teste <- setdiff(seq_len(nrow(dados)), treinamento)
fit3 <- quaDA(variables = dados[, 2:3], group = dados$Grupo,
              validation = 'learntest', learn = treinamento,
              test = teste)
fit2$error_rate
fit3$error_rate

fit2 <- linDA(variables = dados[, 2:3], group = dados$Grupo, validation = 'crossval')
treinamento <- sample(seq_len(nrow(dados)), size = 47) #75% dos dados
teste <- setdiff(seq_len(nrow(dados)), treinamento)
fit3 <- linDA(variables = dados[, 2:3], group = dados$Grupo,
              validation = 'learntest', learn = treinamento,
              test = teste)
fit2$error_rate
fit3$error_rate

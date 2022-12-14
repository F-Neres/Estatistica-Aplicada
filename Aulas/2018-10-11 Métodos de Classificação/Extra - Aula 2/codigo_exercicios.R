##############################################################################################
#
#                               An?lise Discriminante - Exerc?cios
#
##############################################################################################

# Pacotes para an?lise
library(foreign)
library(MASS)
library(dplyr)
library(biotools)
library(DiscriMiner)

################################### Exerc?cio 3 ##############################

# Leitura dos dados
dados <- read.csv('F:/UNICID/An?lise Discriminante/Dados/dados_insetos.csv', sep = ';', dec = ',')

############ Verificando Suposi??es

# Verificando tamanho dos grupos
table(dados$Ra?a)

# Matriz de correla??es (suposi??o de n?o multicolinearidade)
cor(dados[,2:3])

# Verificando suposi??o de normalidade
mshapiro.test(t(dados[,-1]))

# Verificando suposi??o de vari?ncias iguais
boxM(data = dados[, -1], grouping = dados$Ra?a)

########### Sele??o das vari?veis
# Medidas de qualidade
discPower(variables = dados[,2:3], group = dados$Ra?a)

######## Ajuste das fun??es discriminantes

# Modelo discriminante
discrim_1 <- desDA(variables = dados[, 2:3], 
                   group = dados$Ra?a, covar = "within")

# Coeficientes das fun??es discriminantes
discrim_1$discrivar

# Autovalores das fun??es discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpreta??o
discrim_1$discor

############### Avalia??o do ajuste
# Testes da signific?ncia das fun??es discriminantes
# Classifica??o real
Y_real <- dados$Ra?a

escores <- discrim_1$scores
summary(aov(escores ~ Y_real), test="Wilks")

################################### Exerc?cio 4 ##############################

# Leitura dos dados
dados <- read.csv('F:/UNICID/An?lise Discriminante/Dados/dados_cereais.csv', sep = ';', dec = ',') %>%
  select(Fabricante, Calorias, Proteina, Gordura, Sodio, Fibra, Carboidrato, Acucar, Potassio)
glimpse(dados)

############ Verificando Suposi??es

dados[,-1] <- scale(dados[,-1])

# Verificando tamanho dos grupos
table(dados$Fabricante)

# Matriz de correla??es (suposi??o de n?o multicolinearidade)
cor(dados[,2:9])

# Verificando suposi??o de normalidade
mshapiro.test(t(dados[,-1]))

# Verificando suposi??o de vari?ncias iguais
boxM(data = dados[, -1], grouping = dados$Fabricante)

########### Sele??o das vari?veis
# Medidas de qualidade
discPower(variables = dados[,2:9], group = dados$Fabricante)

######## Ajuste das fun??es discriminantes

# Modelo discriminante
discrim_1 <- desDA(variables = dados[, 2:9], 
                   group = dados$Fabricante, covar = "within")

# Coeficientes das fun??es discriminantes
discrim_1$discrivar

# Autovalores das fun??es discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpreta??o
discrim_1$discor

############### Avalia??o do ajuste
# Testes da signific?ncia das fun??es discriminantes
# Classifica??o real
Y_real <- dados$Fabricante

escores <- discrim_1$scores
summary(manova(escores ~ Y_real), test="Wilks")
summary(manova(escores ~ Y_real),test="Hotelling-Lawley")
summary.aov(manova(escores ~ Y_real),test="Hotelling-Lawley")

############## Avaliando as marcas - item c
# Centr?ides
dados$DF1 <- discrim_1$scores[,1]
dados$DF2 <- discrim_1$scores[,2]
dados %>% group_by(Fabricante) %>% summarise(C1 = mean(DF1), C2 = mean(DF2))

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes(x = DF1, y = DF2, colour = Fabricante)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()

# Classifica??o linear
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

# Classifica??o quadr?tica - GRUPOS MUITO PEQUENOS
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

################################### Exerc?cio 7 ##############################

# Leitura dos dados
dados <- read.csv('F:/UNICID/An?lise Discriminante/Dados/dados_alunos.csv', sep = ';', dec = ',') %>%
  select(Grupo, Nota.t?cnica, Hist?rico.escolar)

dados %>% 
  group_by(Grupo) %>% 
  summarise(media_NT = mean(Nota.t?cnica, na.rm = TRUE), 
            media_HE = mean(Hist?rico.escolar, na.rm = TRUE),
            desv_NT = sd(Nota.t?cnica, na.rm = TRUE),
            desv_HE = sd(Hist?rico.escolar, na.rm = TRUE))

dados_1 <- dados %>% filter(Grupo ==1)
cov(dados_1[,-1])
dados_2 <- dados %>% filter(Grupo ==2)
cov(dados_2[,-1])
dados_3 <- dados %>% filter(Grupo ==3)
cov(dados_3[,-1])

############ Verificando Suposi??es

# Verificando tamanho dos grupos
table(dados$Grupo)

# Matriz de correla??es (suposi??o de n?o multicolinearidade)
cor(dados[,2:3])

# Verificando suposi??o de vari?ncias iguais
boxM(data = dados[, -1], grouping = dados$Grupo)

# Verificando suposi??o de normalidade
mshapiro.test(t(dados[,-1]))

########### Sele??o das vari?veis
# Medidas de qualidade
discPower(variables = dados[,2:3], group = dados$Grupo)
dados %>% group_by(Grupo) %>% summarise(media_nota = mean(Nota.t?cnica, na.rm = TRUE),
                                        media_historica = mean(Hist?rico.escolar, na.rm = TRUE))

######## Ajuste das fun??es discriminantes

# Modelo discriminante
discrim_1 <- desDA(variables = dados[, 2:3], 
                   group = dados$Grupo, covar = "within")

# Coeficientes das fun??es discriminantes
discrim_1$discrivar

# Autovalores das fun??es discriminantes e variabilidade explicada
discrim_1$values

# Matriz de fatores para interpreta??o
discrim_1$discor

############### Avalia??o do ajuste
# Testes da signific?ncia das fun??es discriminantes
# Classifica??o real
Y_real <- dados$Grupo

escores <- discrim_1$scores
summary(manova(escores ~ Y_real), test="Wilks")

# Centr?ides
dados$DF1 <- discrim_1$scores[,1]
dados$DF2 <- discrim_1$scores[,2]
dados %>% group_by(Grupo) %>% summarise(C1 = mean(DF1), C2 = mean(DF2))

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes(x = DF1, y = DF2, colour = factor(Grupo))) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()

############### Classifica??o - Fun??es de classifica??o

fit <- linDA(variables = dados[, 2:3], 
             group = dados$Grupo)

# Fun??es de classifica??o
fit$functions

# Matriz de confus?o
fit$confusion

# Taxa de erro
fit$error_rate

# Classifica??o prevista
Y_previsto <- fit$classification

# Classificando um novo objeto
mean(dados$Nota.t?cnica, na.rm = TRUE)
mean(dados$Hist?rico.escolar, na.rm = TRUE)
sd(dados$Nota.t?cnica, na.rm = TRUE)
sd(dados$Hist?rico.escolar, na.rm = TRUE)

classify(fit, newdata = data.frame(Nota.t?cnica = 15, Hist?rico.escolar = 8))$pred_class

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

##############################################################################################
#
#                                     Análise Discriminante
#
##############################################################################################

# Pacotes para análise

install.packages("foreign")
install.packages("MASS")
install.packages("dplyr")
install.packages("biotools")
install.packages("DiscriMiner")

library(foreign)
library(MASS)
library(dplyr)
library(biotools)
library(DiscriMiner)
library(mvnormtest)
library(ggplot2)

# Leitura dos dados
dados <- read.csv('F:/UNICID/Análise Discriminante/Dados/dados_exemplo.csv', sep = ';', dec = ',') %>%
  select(outdoor, social, conservative, job)

################################### Análise Discriminante Linear ##############################
###################################      Método de Fisher        ##############################

############ Verificando Suposições

# Verificando tamanho dos grupos
table(dados$job)

# Matriz de correlações (suposição de não multicolinearidade)
cor(dados[,1:3])

# Verificando suposição de normalidade
mshapiro.test(t(dados[,-4]))

# Verificando suposição de variâncias iguais
boxM(data = dados[, -4], grouping = dados$job)

########### Seleção das variáveis
# Medidas de qualidade
discPower(variables = dados[,1:3], group = dados$job)

######## Ajuste das funções discriminantes

# Modelo discriminante linear
discrim_1 <- desDA(variables = dados[, c("outdoor", "social", "conservative")], 
                   group = dados$job)

# Coeficientes das funções discriminantes
discrim_1$discrivar

# Autovalores das funções discriminantes e variabilidade explicada
discrim_1$values
  
# Matriz de fatores para interpretação
discrim_1$discor

# Centróides
dados$DF1 <- discrim_1$scores[,1]
dados$DF2 <- discrim_1$scores[,2]
dados %>% group_by(job) %>% summarise(C1 = mean(DF1), C2 = mean(DF2))

# Scatterplot
library(ggplot2)
ggplot(data = dados, aes(x = DF1, y = DF2, colour = job)) +
  geom_hline(yintercept = 0, colour="gray70") +
  geom_vline(xintercept = 0, colour="gray70") +
  geom_point()

############### Avaliação do ajuste
# Testes da significância das funções discriminantes
# Classificação real
Y_real <- dados$job

escores <- discrim_1$scores
summary(manova(escores ~ Y_real), test="Wilks")
summary(manova(escores ~ Y_real),test="Hotelling-Lawley")
summary.aov(manova(escores ~ Y_real),test="Hotelling-Lawley")

############### Classificação - Funções de classificação
# Ajuste total
fit <- linDA(variables = dados[, c("outdoor", "social", "conservative")], 
             group = dados$job)

# Ajuste com cross-validaton
fit2 <- linDA(variables = dados[, c("outdoor", "social", "conservative")], 
             group = dados$job, validation = 'crossval')

# Ajuste com amostras treinamento/teste
treinamento <- sample(seq_len(nrow(dados)), size = 183) #75% dos dados
teste <- setdiff(seq_len(nrow(dados)), treinamento)
fit3 <- linDA(variables = dados[, c("outdoor", "social", "conservative")], 
              group = dados$job, validation = 'learntest', learn = treinamento,
              test = teste)

# Funções de classificação
fit$functions

# Matriz de confusão
fit$confusion

# Taxa de erro
fit$error_rate
fit2$error_rate
fit3$error_rate

# Classificação prevista
Y_previsto <- fit$classification

# Classificando um novo objeto
classify(fit, newdata = data.frame(outdoor = 15, social = 10, conservative = 2))$pred_class

# Modelo discriminante quadrático
discrim_quad <- quaDA(variables = dados[, c("outdoor", "social", "conservative")], 
                      group = dados$job)
discrim_quad2 <- quaDA(variables = dados[, c("outdoor", "social", "conservative")], 
                      group = dados$job, validation = 'crossval')
discrim_quad3 <- quaDA(variables = dados[, c("outdoor", "social", "conservative")], 
                      group = dados$job, validation = 'learntest',  learn = treinamento,
                      test = teste)
discrim_quad$error_rate
discrim_quad2$error_rate
discrim_quad3$error_rate






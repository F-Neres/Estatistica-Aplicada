##############################################################################################
#
#                                  An?lise Discriminante - Aula 2
#
##############################################################################################

# Pacotes para an?lise
library(dplyr)

set.seed(0)

# Leitura dos dados
dados <- read.csv('D:/UNICID/An?lise Discriminante/Dados/dados_doenca.csv', sep = ';', dec = ',') %>%
  select(DOENCA = LO3, ANGEST, AH, IMP, TRIGS, COLS, IDADE1, SEXO)
dados <- na.omit(dados)

# Data splitting
# Observa??es de treinamento
tr <- sample.int(1205, 843, replace= F)
dados_treino <- dados[tr,]
dados_teste <- dados[-tr,]

################################### Regress?o Linear ######################################
ajuste_reg_lin <- lm(DOENCA ~ .,
                     data = dados_treino)
prob_posteriori <- predict.lm(ajuste_reg_lin, newdata = dados_teste[, -1])
classificacoes <- ifelse(prob_posteriori > 0.5, 1, 0) 
tab_confusao <- table(classificacoes, dados_teste$DOENCA)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################## Regress?o Log?stica ######################################
ajuste_reg_log <- glm(DOENCA ~ ., 
                      family = binomial(link = 'logit'), data = dados_treino)

# Propor??o de 1's na amostra de treino
p <- mean(dados_treino$DOENCA)

# Predi??es da probabilidade a posteriori
log_chances <- predict.glm(ajuste_reg_log, newdata = dados_teste[, -1])
prob_posteriori <- exp(log_chances)/(1+exp(log_chances))
classificacoes <- ifelse(prob_posteriori > p, 1, 0) 
tab_confusao <- table(classificacoes, dados_teste$DOENCA)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### An?lise Discriminante Linear ##############################
library(DiscriMiner)
fit <- linDA(variables = dados_treino[, -1], 
             group = dados_treino$DOENCA)

# Classificando um novo objeto
classificacoes <- classify(fit, newdata = dados_teste[, -1])$pred_class
tab_confusao <- table(classificacoes, dados_teste$DOENCA)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################ An?lise Discriminante Quadr?tica ##############################
fit <- quaDA(variables = dados_treino[, -1], 
             group = dados_treino$DOENCA)

# Classificando um novo objeto
classificacoes <- classify(fit, newdata = dados_teste[, -1])$pred_class
tab_confusao <- table(classificacoes, dados_teste$DOENCA)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### ?rvore de classifica??o ##############################
library(rpart)

# Ajustar a ?rvore:
fit <- rpart(DOENCA ~ .,
             method="class", data = dados_treino)

# poda:
melhorCp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

# cp ? uma medida de complexidade da ?rvore, essencialmente
# proporcional ao n?mero de folhas presentes. Este c?digo
# escolhe o melhor cp via valida??o cruzada.
pfit <- prune(fit, cp = melhorCp)

# plotar ?rvore podada
pdf('G:/UNICID/An?lise Discriminante/Aulas/grafico_teste_arvore.pdf', width = 9, height = 9)
plot(pfit)
text(pfit,use.n=FALSE,all=FALSE,cex=1.5)
dev.off()

# Classifica??o
classificacao_arvore = predict(pfit, dados_teste[,-1], type="class")
tab_confusao <- table(classificacao_arvore, dados_teste$DOENCA)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### KNN ##############################
ajuste = knn(train = dados_treino[,-1], test = dados_teste[,-1], 
             cl = dados_treino$DOENCA, k = 3)
tab_confusao <- table(ajuste, dados_teste$DOENCA)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

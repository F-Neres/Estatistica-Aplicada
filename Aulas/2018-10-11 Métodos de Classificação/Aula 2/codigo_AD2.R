##############################################################################################
#
#                                  An?lise Discriminante - Aula 2
#
##############################################################################################

##################################### PROBLEMA ####################################
# No arquivo dados_empresa.csv encontram-se os dados referentes a 21 empresas 
# coletados dois anos antes de falirem (grupo 0) e a 25 empresas que n?o faliram 
# (grupo 1). As vari?veis observadas foram:
#  .	Fluxo de caixa com total de d?bitos
#  .	Rendimento da empresa/total do patrim?nio
#  .	Patrim?nio atual/total de d?bito
#  .	Patrim?nio atual/rendimento das vendas
###################################################################################

# Pacotes para an?lise
library(dplyr)
library(biotools)
library(DiscriMiner)

# Leitura dos dados
dados_treino <- read.csv('C:/Users/logonlb/Desktop/Material Aula 2 An?lise Discriminante/dados_empresa.csv', sep = ';', dec = ',') %>%
  select(Grupo, Fluxo = Fluxo.de.caixa..total.de.debitos, 
         Rendimento = Rendimento.da.empresa..total.de.patrim?nio,
         Patrimonio_debito = Patrim?nio.atual..total.de.debito,
         Patrimonio_vendas = Patrim?nio.atual..rendimento.das.vendas)

dados_treino$Grupo <- dados_treino$Grupo - 1 #Cuidado para n?o executar duas vezes

dados_teste <- data.frame(Grupo = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                          Fluxo = c(-0.560, -0.185, -0.070, 0.065, 0.370, -0.330, 0.140, 
                                     0.190, 0.425, 0.580),
                          Rendimento = c(-0.410, -0.185, -0.060, 0.020, 0.110, -0.090, 
                                         0.040, 0.060, 0.085, 0.140),
                          Patrimonio_debito = c(0.330, 1.165, 1.370, 1.535, 2.150, 0.460, 
                                                2.030, 2.350, 2.965, 5.060),
                          Patrimonio_vendas = c(0.160, 0.265, 0.400, 0.630, 0.950, 0.130, 
                                                0.315, 0.450, 0.545, 0.690))

################################### Regress?o Linear ######################################
ajuste_reg_lin <- lm(Grupo ~ Fluxo + Rendimento + Patrimonio_debito + Patrimonio_vendas,
                     data = dados_treino)
prob_posteriori <- predict.lm(ajuste_reg_lin, newdata = dados_teste[, -1])
prob_posteriori
classificacoes <- ifelse(prob_posteriori > 0.5, 1, 0) #Separa os que s?o acima e abaixo de meio
classificacoes
tab_confusao <- table(classificacoes, dados_teste$Grupo)
tab_confusao
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################## Regress?o Log?stica ######################################
ajuste_reg_log <- glm(Grupo ~ Fluxo + Rendimento + Patrimonio_debito + Patrimonio_vendas, 
                      family = binomial(link = 'logit'), data = dados_treino)
ajuste_reg_log

# Propor??o de 1's na amostra de treino
p <- mean(dados_treino$Grupo) #Regra de classifica??o proporcional
p

# Predi??es da probabilidade a posteriori
log_chances <- predict.glm(ajuste_reg_log, newdata = dados_teste[, -1])
prob_posteriori <- exp(log_chances)/(1+exp(log_chances))
prob_posteriori
classificacoes <- ifelse(prob_posteriori > p, 1, 0) 
classificacoes
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### An?lise Discriminante Linear ##############################
fit <- linDA(variables = dados_treino[, -1], 
             group = dados_treino$Grupo)

# Classificando um novo objeto
classificacoes <- classify(fit, newdata = dados_teste[, -1])$pred_class
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################ An?lise Discriminante Quadr?tica ##############################
fit <- quaDA(variables = dados_treino[, -1], 
             group = dados_treino$Grupo)

# Classificando um novo objeto
classificacoes <- classify(fit, newdata = dados_teste[, -1])$pred_class
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### ?rvore de classifica??o ##############################
set.seed(0)
library(rpart)

# Ajustar a ?rvore:
fit <- rpart(Grupo ~ Fluxo + Rendimento + Patrimonio_debito + Patrimonio_vendas,
             method="class", data = dados_treino)
fit

# poda:
melhorCp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
melhorCp

# cp ? uma medida de complexidade da ?rvore, essencialmente
# proporcional ao n?mero de folhas presentes. Este c?digo
# escolhe o melhor cp via valida??o cruzada.
pfit <- prune(fit, cp = melhorCp)
pfit

# plotar ?rvore podada
pdf('grafico_teste_arvore.pdf', width = 9, height = 9) #Endere?o do gr?fico
plot(pfit)
text(pfit,use.n=FALSE,all=FALSE,cex=1.5)
dev.off()

# Classifica??o
classificacoes <- predict(pfit, dados_teste[,-1], type = 'class')
tab_confusao <- table(classificacoes, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos

################################### KNN ##############################
install.packages("class")
library(class)
ajuste = knn(train = dados_treino[,-1], test = dados_teste[,-1], 
             cl = dados_treino$Grupo, k = 3)
tab_confusao <- table(ajuste, dados_teste$Grupo)
taxa_acertos <- (tab_confusao[1,1] + tab_confusao[2,2])/sum(tab_confusao)
taxa_acertos
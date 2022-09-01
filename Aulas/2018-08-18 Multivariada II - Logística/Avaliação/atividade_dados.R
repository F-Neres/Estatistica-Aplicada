#title: Avaliação - Inferência Multivariada II
#author: Felipe N. S. Bezerra

# Exercício 1 - Dados
rm(list=ls())
library(readxl)
Dados_ativ_final201802 <- read_excel("D:/Documentos/Estatística/Aulas/2018-08-18 Multivariada II - Logística/Avaliação/Dados_ativ_final201802.xlsx")
View(Dados_ativ_final201802)


#Definindo variáveis como fatores ou numéricas.
  
Dados.ativ <- as.data.frame(Dados_ativ_final201802)
Dados.ativ[,"pagamento"] <- as.factor(Dados.ativ[,"pagamento"])
Dados.ativ[,"estadocivil"] <- as.factor(Dados.ativ[,"estadocivil"])
Dados.ativ[,"sexo"] <- as.factor(Dados.ativ[,"sexo"])
Dados.ativ[,"idade"] <- as.numeric(Dados.ativ[,"idade"])
summary(Dados.ativ)


#Separando observações como treinamento (60% das observações) e validação (40% das observações).
#A coluna 'id' será removida do conjunto de dados de treinamento para não ser considerada uma variável explicativa no modelo mais adiante.
#Nota: foi utilizado o comando 'set.seed' para fins de posterior averiguação dos resultados.
set.seed(8)
ablation <- sort(sample(nrow(Dados.ativ), nrow(Dados.ativ)*.6))
Dados.treino <- Dados.ativ[ablation,]
Dados.treino <- subset(Dados.treino, select = -id)
Dados.valid <- Dados.ativ[-ablation,]
summary(Dados.treino)
summary(Dados.valid)


#Regressão logistica dos dados de treinamento com todas as variáveis.
Mod.compl <- glm(pagamento ~., family=binomial, data=Dados.treino)
summary(Mod.compl)


#Processo de stepwise para seleção do modelo com maior poder explicativo (segundo critério de Akaike) e verificação de superdispersão.
Mod.stepwise <- step (Mod.compl, direction="both")
summary(Mod.stepwise)
deviance(Mod.stepwise)/df.residual(Mod.stepwise)


#Razão de chances (odds ratio).
exp(cbind (OR=coef (Mod.stepwise), confint (Mod.stepwise)))


#Previsão para a base de validação e matriz de confusão.
Predit.mod.stepwise <- predict(Mod.stepwise, Dados.valid, type="response")
Predit.mod.stepwise <- ifelse(Predit.mod.stepwise >= 0.8,1,0)
MC <- table(Predit.mod.stepwise, Dados.valid$pagamento)
show(MC)


#Taxa de Acerto
Acur = sum(diag(MC))/sum(MC)
Acur

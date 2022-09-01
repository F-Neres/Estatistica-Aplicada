#title: Avaliação - Inferência Multivariada II
#author: Felipe N. S. Bezerra

# Exercício 1 - Dados
rm(list=ls())
library(readxl)
credito_ativ_final201802 <- read_excel("D:/Documentos/Estatística/Aulas/2018-08-18 Multivariada II - Logística/Avaliação/credito_ativ_final201802.xlsx")
View(credito_ativ_final201802)


#Definindo variáveis como fatores ou numéricas.
credito.ativ <- as.data.frame(credito_ativ_final201802)
credito.ativ[,"default"] <- as.factor(credito.ativ[,"default"])
credito.ativ[,"idade"] <- as.numeric(credito.ativ[,"idade"])
credito.ativ[,"educação"] <- as.numeric(credito.ativ[,"educação"])
credito.ativ[,"t_emprego"] <- as.numeric(credito.ativ[,"t_emprego"])
credito.ativ[,"outras_dív"] <- as.numeric(credito.ativ[,"outras_dív"])
summary(credito.ativ)


#Separando observações como treinamento (60% das observações) e validação (40% das observações).
#Nota: foi utilizado o comando 'set.seed' para fins de posterior averiguação dos resultados.
set.seed(8)
ablation <- sort(sample(nrow(credito.ativ), nrow(credito.ativ)*.6))
credito.treino <- credito.ativ[ablation,]
credito.valid <- credito.ativ[-ablation,]
summary(credito.treino)
summary(credito.valid)


#Regressão logistica dos dados de treinamento com todas as variáveis.
Mod.compl <- glm(default ~., family=binomial, data=credito.treino)
summary(Mod.compl)


#Processo de stepwise para seleção do modelo com maior poder explicativo (segundo critério de Akaike) e verificação de superdispersão.
Mod.stepwise <- step (Mod.compl, direction="both")
summary(Mod.stepwise)
deviance(Mod.stepwise)/df.residual(Mod.stepwise)


#Razão de chances (odds ratio).
exp(cbind (OR=coef (Mod.stepwise), confint (Mod.stepwise)))


#Predisão para a base de validação e matriz de confusão.
Predit.mod.stepwise <- predict(Mod.stepwise, credito.valid, type="response")
Predit.mod.stepwise <- ifelse(Predit.mod.stepwise >= 0.8,1,0)
MC <- table(Predit.mod.stepwise, credito.valid$default)
show(MC)


#Taxa de Acerto
Acur = sum(diag(MC))/sum(MC)
Acur


#Predição para um indivíduo de 40 anos, nível de educação 3, há 3 anos no emprego atual, há 5 anos no endereço atual e mais de $30.000,00 em outras dívidas.
pred.2b <- data.frame(idade = 40,
                      educação = 3,
                      t_emprego = 3,
                      t_endereço = 5,
                      outras_dív = 30)
predict.glm(Mod.stepwise,
            pred.2b,
            type="response")


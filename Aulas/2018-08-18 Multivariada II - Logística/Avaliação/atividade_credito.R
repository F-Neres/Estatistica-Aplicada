#title: Avalia??o - Infer?ncia Multivariada II
#author: Felipe N. S. Bezerra

# Exerc?cio 1 - Dados
rm(list=ls())
library(readxl)
credito_ativ_final201802 <- read_excel("D:/Documentos/Estat?stica/Aulas/2018-08-18 Multivariada II - Log?stica/Avalia??o/credito_ativ_final201802.xlsx")
View(credito_ativ_final201802)


#Definindo vari?veis como fatores ou num?ricas.
credito.ativ <- as.data.frame(credito_ativ_final201802)
credito.ativ[,"default"] <- as.factor(credito.ativ[,"default"])
credito.ativ[,"idade"] <- as.numeric(credito.ativ[,"idade"])
credito.ativ[,"educa??o"] <- as.numeric(credito.ativ[,"educa??o"])
credito.ativ[,"t_emprego"] <- as.numeric(credito.ativ[,"t_emprego"])
credito.ativ[,"outras_d?v"] <- as.numeric(credito.ativ[,"outras_d?v"])
summary(credito.ativ)


#Separando observa??es como treinamento (60% das observa??es) e valida??o (40% das observa??es).
#Nota: foi utilizado o comando 'set.seed' para fins de posterior averigua??o dos resultados.
set.seed(8)
ablation <- sort(sample(nrow(credito.ativ), nrow(credito.ativ)*.6))
credito.treino <- credito.ativ[ablation,]
credito.valid <- credito.ativ[-ablation,]
summary(credito.treino)
summary(credito.valid)


#Regress?o logistica dos dados de treinamento com todas as vari?veis.
Mod.compl <- glm(default ~., family=binomial, data=credito.treino)
summary(Mod.compl)


#Processo de stepwise para sele??o do modelo com maior poder explicativo (segundo crit?rio de Akaike) e verifica??o de superdispers?o.
Mod.stepwise <- step (Mod.compl, direction="both")
summary(Mod.stepwise)
deviance(Mod.stepwise)/df.residual(Mod.stepwise)


#Raz?o de chances (odds ratio).
exp(cbind (OR=coef (Mod.stepwise), confint (Mod.stepwise)))


#Predis?o para a base de valida??o e matriz de confus?o.
Predit.mod.stepwise <- predict(Mod.stepwise, credito.valid, type="response")
Predit.mod.stepwise <- ifelse(Predit.mod.stepwise >= 0.8,1,0)
MC <- table(Predit.mod.stepwise, credito.valid$default)
show(MC)


#Taxa de Acerto
Acur = sum(diag(MC))/sum(MC)
Acur


#Predi??o para um indiv?duo de 40 anos, n?vel de educa??o 3, h? 3 anos no emprego atual, h? 5 anos no endere?o atual e mais de $30.000,00 em outras d?vidas.
pred.2b <- data.frame(idade = 40,
                      educa??o = 3,
                      t_emprego = 3,
                      t_endere?o = 5,
                      outras_d?v = 30)
predict.glm(Mod.stepwise,
            pred.2b,
            type="response")


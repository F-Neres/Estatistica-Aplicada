#title: Avalia??o - Infer?ncia Multivariada II
#author: Felipe N. S. Bezerra

# Exerc?cio 1 - Dados
rm(list=ls())
library(readxl)
Dados_ativ_final201802 <- read_excel("D:/Documentos/Estat?stica/Aulas/2018-08-18 Multivariada II - Log?stica/Avalia??o/Dados_ativ_final201802.xlsx")
View(Dados_ativ_final201802)


#Definindo vari?veis como fatores ou num?ricas.
  
Dados.ativ <- as.data.frame(Dados_ativ_final201802)
Dados.ativ[,"pagamento"] <- as.factor(Dados.ativ[,"pagamento"])
Dados.ativ[,"estadocivil"] <- as.factor(Dados.ativ[,"estadocivil"])
Dados.ativ[,"sexo"] <- as.factor(Dados.ativ[,"sexo"])
Dados.ativ[,"idade"] <- as.numeric(Dados.ativ[,"idade"])
summary(Dados.ativ)


#Separando observa??es como treinamento (60% das observa??es) e valida??o (40% das observa??es).
#A coluna 'id' ser? removida do conjunto de dados de treinamento para n?o ser considerada uma vari?vel explicativa no modelo mais adiante.
#Nota: foi utilizado o comando 'set.seed' para fins de posterior averigua??o dos resultados.
set.seed(8)
ablation <- sort(sample(nrow(Dados.ativ), nrow(Dados.ativ)*.6))
Dados.treino <- Dados.ativ[ablation,]
Dados.treino <- subset(Dados.treino, select = -id)
Dados.valid <- Dados.ativ[-ablation,]
summary(Dados.treino)
summary(Dados.valid)


#Regress?o logistica dos dados de treinamento com todas as vari?veis.
Mod.compl <- glm(pagamento ~., family=binomial, data=Dados.treino)
summary(Mod.compl)


#Processo de stepwise para sele??o do modelo com maior poder explicativo (segundo crit?rio de Akaike) e verifica??o de superdispers?o.
Mod.stepwise <- step (Mod.compl, direction="both")
summary(Mod.stepwise)
deviance(Mod.stepwise)/df.residual(Mod.stepwise)


#Raz?o de chances (odds ratio).
exp(cbind (OR=coef (Mod.stepwise), confint (Mod.stepwise)))


#Previs?o para a base de valida??o e matriz de confus?o.
Predit.mod.stepwise <- predict(Mod.stepwise, Dados.valid, type="response")
Predit.mod.stepwise <- ifelse(Predit.mod.stepwise >= 0.8,1,0)
MC <- table(Predit.mod.stepwise, Dados.valid$pagamento)
show(MC)


#Taxa de Acerto
Acur = sum(diag(MC))/sum(MC)
Acur

###############################################################################################
#                                Análise de Regressão Logística
###############################################################################################

# Pacotes necessários
library(dplyr)
library(pROC)

############################## Exemplo 1 #########################################

# Leitura dos dados
idade <- c(21,20,25,26,22,35,36,40,42,46,59,50,60,72,85,59,29,45,39,45,20,25,36,58,95,52,80,85,62,72)
renda <- c(1,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1)
saude <- c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

dados <- data.frame(idade = idade, renda = renda, saude = saude)

glimpse(dados)

# Ajuste do modelo - estimativas pontuais dos parâmetros
modelo1 <- glm(saude ~ idade + renda, 
               family = binomial(link = 'logit'))
summary(modelo1)

# Medidas de associação para interpretação
OR1 <- exp(modelo1$coefficients)
OR1

# Intervalos de 95% de confiança para as razões de chances
ICbeta1 <- confint.default(modelo1,level = 0.95)
ICOR1 <- exp(ICbeta1)
ICOR1

# Avaliando qualidade do ajuste - deviance
p_valor <- pchisq(q = modelo1$deviance, df = modelo1$df.residual, lower.tail = FALSE)
p_valor

# Predição
novos_dados <- data.frame(idade = c(20,20, 50, 50), renda = c(0, 1, 0, 1))
predict(modelo1, newdata = novos_dados, type = 'response')

# Valores ajustados pelo modelo para as probabilidade de boa saúde
modelo1$fitted.values

# Curva ROC
curva_roc <- roc(response = saude, predictor = modelo1$fitted.values)
plot.roc(curva_roc)

# Classificação
class <- ifelse(modelo1$fitted.values >= 0.90, 1, 0)
table(class, saude)





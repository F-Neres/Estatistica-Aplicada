              #German Credit
#Limpar o Workspace
rm(list=ls())

dados.df <- as.data.frame(GermanCredit)

#Apresenta as variáveis do DataFrame
names(dados.df)
#Apresenta a estrutura do Dataframe
str(dados.df)

#Transforma em fatores as variáveis categóricas e "dummies" 
dados.df[,"CHK_ACCT"] <-as.factor(dados.df[,"CHK_ACCT"])
dados.df[,"HISTORY"] <-as.factor(dados.df[,"HISTORY"])
dados.df[,"NEW_CAR"] <-as.factor(dados.df[,"NEW_CAR"])
dados.df[,"USED_CAR"] <-as.factor(dados.df[,"USED_CAR"])
dados.df[,"FURNITURE"] <-as.factor(dados.df[,"FURNITURE"])
dados.df[,"RADIO/TV"] <-as.factor(dados.df[,"RADIO/TV"])
dados.df[,"EDUCATION"] <-as.factor(dados.df[,"EDUCATION"])
dados.df[,"RETRAINING"] <-as.factor(dados.df[,"RETRAINING"])
dados.df[,"SAV_ACCT"] <-as.factor(dados.df[,"SAV_ACCT"])
dados.df[,"EMPLOYMENT"] <-as.factor(dados.df[,"EMPLOYMENT"])
dados.df[,"MALE_DIV"] <-as.factor(dados.df[,"MALE_DIV"])
dados.df[,"MALE_SINGLE"] <-as.factor(dados.df[,"MALE_SINGLE"])
dados.df[,"CO-APPLICANT"] <-as.factor(dados.df[,"CO-APPLICANT"])
dados.df[,"GUARANTOR"] <-as.factor(dados.df[,"GUARANTOR"])
dados.df[,"REAL_ESTATE"] <-as.factor(dados.df[,"REAL_ESTATE"])
dados.df[,"OTHER_INSTALL"] <-as.factor(dados.df[,"OTHER_INSTALL"])
dados.df[,"RENT"] <-as.factor(dados.df[,"RENT"])
dados.df[,"OWN_RES"] <-as.factor(dados.df[,"OWN_RES"])
dados.df[,"NUM_CREDITS"] <-as.factor(dados.df[,"NUM_CREDITS"])
dados.df[,"JOB"] <-as.factor(dados.df[,"JOB"])
dados.df[,"TELEPHONE"] <-as.factor(dados.df[,"TELEPHONE"])
dados.df[,"FOREIGN"] <- as.factor(dados.df[,"FOREIGN"])

#Variável dependente
dados.df[,"RESPONSE"]     <-as.factor(dados.df[,"RESPONSE"])

#Transforma em numeric
dados.df[,"AMOUNT"]       <-as.numeric(dados.df[,"AMOUNT"])
dados.df[,"INSTALL_RATE"] <-as.numeric(dados.df[,"INSTALL_RATE"])
dados.df[,"AGE"]          <-as.numeric(dados.df[,"AGE"])
dados.df[,"DURATION"]     <-as.numeric(dados.df[,"DURATION"])

#Índices obtidos após a aleatorização
ordena <- sort(sample(nrow(dados.df), nrow(dados.df)*.6))

#Dados para o treinamento
treinamento<-dados.df[ordena,]

#Dados para a validação
validacao<-dados.df[-ordena,]

#Regressão Logística, de 'RESPONSE' em função de todas as outras variáveis
modelo.completo <- glm(RESPONSE ~ . ,family=binomial,data=treinamento)
summary(modelo.completo)

#Abordagem Stepwise para seleção de variáveis
stepwise <- step(modelo.completo,direction="both")

#Modelo com as variáveis indicadas pelo Stepwise [???]
stepwise <- glm(RESPONSE ~  JOB+NUM_CREDITS+EMPLOYMENT+RETRAINING+NEW_CAR+TELEPHONE+MALE_DIV+ FURNITURE+RENT+REAL_ESTATE+EDUCATION+FOREIGN, family=binomial,data=treinamento)

#Resume os resultados do modelo
summary(stepwise)

#Calcula a razão de chances
exp(cbind(OR = coef(stepwise), confint(stepwise)))

#Faz a previsão para a base de validação (probabilidade)
predito<-predict(stepwise,validacao,type="response")

#Escolhe quem vai ser "1" e quem vai ser "0"
predito<-ifelse(predito>=0.8,1,0)

#Compara os resultados
MC<-table(predito,validacao$RESPONSE)
show(MC) #Matriz de confusão

#Logo, a nossa taxa de acerto (acurácia) nesse modelo é dada por:
ACC = sum(diag(MC))/sum(MC)
ACC

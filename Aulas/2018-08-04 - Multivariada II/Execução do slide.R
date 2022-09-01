idade=c(21,20,25,26,22,35,36,40,42,46,59,50,60,72,85,59,29,45,39,45,20,25,36,58,95,52,80,85,62,72)
renda=c(1,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1)
saude=c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

#Modelo de regressão logística: glm
modelo1=glm(saude ~ idade+renda , family = binomial (link="logit"))
modelo1
summary(modelo1)

#Chances
OR1=exp(modelo1$coefficients);OR1

#Demonstra o intervalo de confiança; os coeficientes devem estar dentro deste intervalo
ICbeta1=confint.default(modelo1,level=0.95);ICbeta1
#Intervalo de confiança para as chances
ICOR1=exp(ICbeta1);ICOR1

#Agrupando os dados
round((cbind(OR1,ICOR1)),3)

#      Exercício 2

gaviao<- read.csv("C:/Users/logonlb/Documents/gaviao.csv", header = TRUE, sep = ";", dec=",")

gaviao.logi<-glm(resp~alt,data=gaviao,family=binomial)
summary(gaviao.logi)

#Para árvores com 14 metros
exp(-3.67961+0.19013*14)/(1+exp(-3.67961+0.19013*14))

plot(gaviao$alt,gaviao$resp,xlab="Altura das árvores (vezes 10)",  ylab="Presença de gavião(0 e 1) e prob encontro de gavião  (curva)")
curve((exp(-3.67961+0.19013*x))/(1+(exp(-3.67961+ 0.19013*x))),add=TRUE)

#     Exercício 3

#BD_aulaclass

install.packages("tabplot") # instala a biblioteca  
library(tabplot) # carrega a biblioteca
tableplot(BD_aulaclass) # visualização das variáveis 
tableplot(BD_aulaclass, sortCol = "IDADE", decreasing = F) # ordenado por idade do menor pro maior 

BD_aulaclass$ADIMPLENTE<-as.factor(BD_aulaclass$ADIMPLENTE)
BD_aulaclass$ADIMPLENTE = relevel(BD_aulaclass$ADIMPLENTE, ref = "N")

RLSIMPLES = glm(ADIMPLENTE ~ ESTC + NDEP + RENDA + TIPOR + VBEM + NPARC + VPARC + TEL + IDADE + RESMS + ENTRADA, family="binomial" , data = BD_aulaclass)
predRL = predict(RLSIMPLES, newdata = BD_aulaclass, type = "response")

#
BD_aulaclass$ADIMPLENTE_PREDRL = predRL > 0.45 # faz a limiarização (diferença nos níveis)
MC = table(BD_aulaclass$ADIMPLENTE, BD_aulaclass$ADIMPLENTE_PREDRL, deparse.level = 2) # montar a matriz de confusão  
show(MC) # mostra os resultados

ACC = sum(diag(MC))/sum(MC) # calcula a acurácia
show(ACC) # mostra a acurácia

#       Exercício 4

summary(chd)

install.packages("ggplot2")
library(ggplot2)

ggplot(chd, aes(x=age, y=chd)) + geom_point() + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

m1 = glm (chd~age, family = binomial(link="logit"), data = chd)
summary(m1)

install.packages("mfx")
require(mfx)
logitor(chd~age, data = chd)
exp(cbind(OR=coef(m1), confint(m1)))
media = data.frame(age=mean(chd$age))
media
media$pred.prob = predict(m1, newdata=media, type="response")
media

#       Exercício 5 [Extra]
#Dados de "titanic3"
titanic3$pclass<-as.factor(titanic3$pclass)

Modelo_Titanic=glm(survived ~ pclass+sex+age+sibsp+parch , family ="binomial"(link="logit") , data = titanic3)
Modelo_Titanic
summary(Modelo_Titanic)

predTitanic = predict(Modelo_Titanic, newdata = titanic3, type = "response")


#Chances
ORTitanic=exp(Modelo_Titanic$coefficients);ORTitanic

#Demonstra o intervalo de confiança; os coeficientes devem estar dentro deste intervalo
ICTitanic=confint.default(Modelo_Titanic,level=0.95);ICTitanic
#Intervalo de confiança para as chances
ICORTitanic=exp(ICTitanic);ICORTitanic

#Agrupando os dados
round((cbind(ORTitanic,ICORTitanic)),3)

    # Removendo "parch"

Modelo_TitanicAj=glm(survived ~ pclass+sex+age+sibsp , family ="binomial"(link="logit") , data = titanic3)
Modelo_TitanicAj
summary(Modelo_TitanicAj)

predTitanicAj = predict(Modelo_TitanicAj, newdata = titanic3, type = "response")

#Chances
ORTitanicAj=exp(Modelo_TitanicAj$coefficients);ORTitanicAj

#Demonstra o intervalo de confiança; os coeficientes devem estar dentro deste intervalo
ICTitanicAj=confint.default(Modelo_TitanicAj,level=0.95);ICTitanicAj
#Intervalo de confiança para as chances
ICORTitanicAj=exp(ICTitanicAj);ICORTitanicAj

#Agrupando os dados
round((cbind(ORTitanicAj,ICORTitanicAj)),3)

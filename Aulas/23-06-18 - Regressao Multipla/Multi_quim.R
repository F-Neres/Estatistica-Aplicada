setwd(choose.dir()) #escolher diretório
quimica<-read.csv2("quimica.csv") #abre os dados do arquivo

str(quimica)
summary(quimica)

attach(quimica)

par(mfrow=c(1,2))

plot(Rendimento~Temperatura)
plot(Rendimento~Tempo)

mod_quim<-lm(Rendimento~Temperatura*Tempo)
mod_quim

par(mfrow=c(2,2))

plot(mod_quim)
summary(mod_quim)

#Utilizando apenas as dispersões
Tempo1<-Tempo-mean(Tempo) #média igual a zero
Temperatura1<-Temperatura-mean(Temperatura) #média igual a zero

ajuste<-lm(Rendimento~Tempo1+I(Tempo1^2) +Temperatura1+I(Temperatura1^2) +Tempo1*Temperatura1)
  ajuste.t<-lm(Rendimento ~ I(Tempo1^2) +I(Temperatura1^2) + Tempo1*Temperatura1)
ajuste
#Rendimento ~ 61.23 -0.66*Tempo1 -0.07*I(Tempo1^2) +0.11*Temperatura1 -0.04*I(Temperatura1^2) +0,02*(Tempo1:Temperatura1)

summary(ajuste)
#Todos os coeficientes foram significativos

par(mfrow=c(1,1))
plot(ajuste,which=1)
plot(ajuste,which=2)
shapiro.test(residuals(ajuste))
plot(ajuste,which=3)
plot(ajuste,which=4)


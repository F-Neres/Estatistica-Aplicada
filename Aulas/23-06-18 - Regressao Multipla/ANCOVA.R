      #Análise de Covariância

equire(MASS)
#Exige o pacote de dados "MASS"
data(cats)
#Abre os dados "cats" (do pacote "MASS")
head(cats,8)
tail(cats,11)
summary(cats)

plot(Hwt~Bwt,
     col=factor(cats$Sex),
     data=cats)
plot(Hwt~Sex,cats)
plot(Bwt~Sex,cats)

#Nomenclatura da divisão de grupos por "Sex"
cats$Sex=="M"
macho<-cats[cats$Sex=="M",]
macho
cats$Sex=="F"
femea<-cats[cats$Sex=="F",]
femea

mod_macho<-lm(Hwt~Bwt,macho) #ou
  mod_macho.t<-lm(Hwt~Bwt,cats$Sex=="M")
summary(mod_macho)
  summary(mod_macho.t)
  #
mod_femea<-lm(Hwt~Bwt,femea) #ou
  mod_femea.t<-lm(Hwt~Bwt,cats$Sex=="F")
summary(mod_femea)
  summary(mod_femea.t)

mod_sexo<-lm(Hwt ~ Bwt*Sex , cats)
  mod_sexo<-lm(Hwt ~ Bwt +Sex +Bwt:Sex,cats)
summary(mod_sexo)

par(mfrow=c(2,2))
plot(mod_sexo)
par(mfrow=c(1,1))

install.packages("ggplot2")
require(ggplot2)
ggplot(cats,aes(x=Bwt,y=Hwt, color=Sex, group=Sex))+
  geom_point()+
  geom_smooth(se=F, method="lm")

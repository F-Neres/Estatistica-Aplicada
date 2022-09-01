sementes<-read.csv('http://renatabrandt.github.io/EBC2015/data/sementes.csv')
head(sementes,3)
mod_sem=lm(Sementes ~ Chuva*Fertilizante, data = sementes)

summary(mod_sem)

install.packages("ggplot2")
require(ggplot2)
ggplot(sementes,aes(x=Chuva, y=Sementes, color=Fertilizante, group=Fertilizante))+
  geom_point()+ theme_bw()+
  geom_smooth(se=F, method="lm")

anova(mod_sem)

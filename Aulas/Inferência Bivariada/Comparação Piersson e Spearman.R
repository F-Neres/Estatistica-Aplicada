x<-1:100
y<- -1*exp(x)
plot(x~y)
cor.test(x=x,y=y,method="pearson")
cor.test(x=x,y=y,method="spearman")

y1<- -1*exp(c(x[2:100],1))
plot(y1~x)
cor.test(x=x,y=y1,method="pearson")
cor.test(x=x,y=y1,method="spearman")
cor(x=x,y=y1)^2 #R²

modelo <- lm(y1~x)
plot(y1~x)
abline(coef(modelo),col="red")
summary(modelo) #sumário de um modelo linear
cor(x=x,y=y1)^2

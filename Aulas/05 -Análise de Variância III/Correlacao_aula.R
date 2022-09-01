## carregamento de pacotes
if(!require(dplyr)){install.packages("dplyr")}          # unite, filter, select, %>%
if(!require(tidyr)){install.packages("tidyr")}          # gather, spread
if(!require(ggplot2)){install.packages("ggplot2")}      # graficos

## Correlacao linear simples
data("anscombe")
plot(y1~x1, anscombe)
abline(lm(y1~x1, anscombe), col="red")

summary(lm(y1~x1, anscombe))

## Construir uma função bonita para plotar
## Fonte: Freakonomics (http://freakonometrics.hypotheses.org/9593)
plotar_curvas<-function(n=2, m=8, X, Y){
  df<-data.frame(X,Y)
  vX<-seq(min(X)-1,max(X)+1,length=n)
  vY<-seq(min(Y)-1,max(Y)+1,length=n)
  mat<-persp(vX,vY,matrix(0,n,n), zlim=c(0,.5),
             theta=-30,ticktype ="detailed", box = FALSE)
  reggig<-glm(Y~X,data=df,family=gaussian(link="identity"))
  x<-seq(min(X),max(X),length=501)
  C<-trans3d(x,predict(reggig,
                       newdata=data.frame(X=x),
                       type="response"),rep(0,length(x)),mat)
  lines(C,lwd=2)
  sdgig<-sqrt(summary(reggig)$dispersion)
  x<-seq(min(X),max(X),length=501)
  y1<-qnorm(.95,
            predict(reggig,newdata=data.frame(X=x),
                    type="response"), sdgig)
  C<-trans3d(x,y1,rep(0,length(x)),mat)
  lines(C,lty=2)
  y2<-qnorm(.05, 
            predict(reggig,newdata=data.frame(X=x),type="response"), 
            sdgig)
  C<-trans3d(x,y2,rep(0,length(x)),mat)
  lines(C,lty=2)
  C<-trans3d(c(x,rev(x)),c(y1,rev(y2)),rep(0,2*length(x)),mat)
  polygon(C,border=NA,col="yellow")
  C<-trans3d(X,Y,rep(0,length(X)),mat)
  points(C,pch=19,col="red")
  vX<-seq(min(X),max(X),length=m)
  mgig<-predict(reggig,newdata=data.frame(X=vX))
  sdgig<-sqrt(summary(reggig)$dispersion)
  for(j in m:1){
    stp<-251
    x<-rep(vX[j],stp)
    y<-seq(min(min(Y)-15,
               qnorm(.05,predict(reggig,
                                 newdata=data.frame(X=vX[j]),
                                 type="response"), sdgig)),
           max(Y)+15,length=stp)
    z0<-rep(0,stp)
    z<-dnorm(y, mgig[j], sdgig)
    C<-trans3d(c(x,x),c(y,rev(y)),c(z,z0),mat)
    polygon(C,border=NA,col="light blue",density=40)
    C<-trans3d(x,y,z0,mat)
    lines(C,lty=2)
    C<-trans3d(x,y,z,mat)
    lines(C,col="blue")}
}

plotar_curvas(n=8, m=6, X = anscombe$x1, Y = anscombe$y1)
ggplot(anscombe, aes(y=y1,x=x1))+
  geom_point(color="red")+
  geom_smooth(method='lm',
              fill='yellow',se=0.95)+theme_bw()

dev.off()
par(mfrow=c(2,2))
plot(y1~x1, anscombe)
abline(lm(y1~x1, anscombe), col="red")
plot(y2~x2, anscombe)
abline(lm(y2~x2, anscombe), col="red")
plot(y3~x3, anscombe)
abline(lm(y3~x3, anscombe), col="red")
plot(y4~x4, anscombe)
abline(lm(y4~x4, anscombe), col="red")
par(mfrow(c=1,1))

# Hipotese nula Beta = 0
summary(lm(y1~x1, anscombe))

# Correlacao

plot(Sepal.Length~Petal.Length, iris[iris$Species=="setosa",])
mod1 <- lm(Sepal.Length~Petal.Length, iris[iris$Species=="setosa",])
mod2 <- lm(Petal.Length~Sepal.Length, iris[iris$Species=="setosa",])
abline(mod1, col="red")
mabline(mod2, col="blue")

cor(anscombe$x1,anscombe$y1, method="pearson")
cor.test(anscombe$x1,anscombe$y1, method="pearson")
summary(lm(y1~x1, anscombe))

## Explorar a correlação entre varias variáveis

if(!require(ggpubr)){install.packages("ggpubr")}
# Gráfico de pontos colorido por grupos ("Species")
data("iris")
sp <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width",
                color = "Species", palette = "jco",
                size = 3, alpha = 0.6)+
  border()                                         
# Gráfico de densidade para os eixos x(em cima) e y (lado direito)
xplot <- ggdensity(iris, "Sepal.Length", fill = "Species",
                   palette = "jco")
yplot <- ggdensity(iris, "Sepal.Width", fill = "Species", 
                   palette = "jco")+rotate()

# limpando os temas
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()
# Arranjando os gráficos
ggarrange(xplot, NULL, sp, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)
# Fonte: http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

## Nao parametrica Spearman
data("USArrests")
plot(UrbanPop~Rape, USArrests)
cor.test(x=USArrests$UrbanPop, y=USArrests$Rape, method="spearman")

plot(UrbanPop~Murder, USArrests)
cor.test(x=USArrests$UrbanPop, y=USArrests$Murder, method="spearman")

## Correlacao bisserial
data("mtcars")
cor.test(x=mtcars$am, y=mtcars$mpg)

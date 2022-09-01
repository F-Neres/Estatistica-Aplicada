pacotes<-c("rgl","graphics","lattice","scatterplot3d")
install.packages(pacotes)
require(datasets)
str(trees)
head(trees)
attach(trees)

#Definindo uma função para desenhar retas de regressão:

flines<- function(x,y){
  points(x,y)
  abline(lm(y~x), col="red")
}

#definindo uma função para plotar as correlações:

fcor<- function(x,y){
  par(usr=c(0,1,0,1))
  txt<- as.character(round(cor(x,y),2))
  text(0.5, 0.5, txt, cex=3)
}

pairs(trees, lower.panel=flines, upper.panel = fcor)
#Matriz de gráficos de regressões, dois a dois.

library(scatterplot3d)
attach(trees)
graph<- scatterplot3d(Volume ~Girth+Height, pch=16, angle=60)
fit<- lm(Volume~Girth +Height)
graph$plane3d(fit, col="blue")

library(lattice)
cloud(Volume ~Girth*Height, data=trees,
      scales=list(arrows=FALSE))

library(rgl)
plot3d(trees)


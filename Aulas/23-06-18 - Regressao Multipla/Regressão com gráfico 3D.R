pacotes<-c("rgl","graphics","lattice","scatterplot3d")
install.packages(pacotes)
require(datasets)
str(trees)
head(trees)
attach(trees)

#Definindo uma fun??o para desenhar retas de regress?o:

flines<- function(x,y){
  points(x,y)
  abline(lm(y~x), col="red")
}

#definindo uma fun??o para plotar as correla??es:

fcor<- function(x,y){
  par(usr=c(0,1,0,1))
  txt<- as.character(round(cor(x,y),2))
  text(0.5, 0.5, txt, cex=3)
}

pairs(trees, lower.panel=flines, upper.panel = fcor)
#Matriz de gr?ficos de regress?es, dois a dois.

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


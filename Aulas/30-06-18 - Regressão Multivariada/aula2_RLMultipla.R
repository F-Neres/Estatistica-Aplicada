data("trees")
#definindo uma função para desenhar retas de regressão:
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
attach(trees)
fit <- lm(Volume ~ Girth + Height)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
# Visualizando
if(!require(scatterplot3d)){install.packages("scatterplot3d")}
graph<- scatterplot3d(Volume ~ Girth + Height,
                      pch=16, #tipo de ponto
                      angle=50) #ângulo de observação
# Adiciona ao gráfico um plano de regressao
graph$plane3d(fit, col="blue")
# Adiciona Outliers ao gráfico.São três números por ser uma coordenada 3D
graph$points3d(c(18,20,17), # Girth
               c(64,64,65), # Height
               c(19,18,19), # Volume
               col="red", 
               pch=16)
graph<- scatterplot3d(Volume ~ Girth + Height,
                      pch=16, 
                      angle=50)
graph$points3d(c(18,20,17), # Girth
               c(64,64,65), # Height
               c(19,18,19), # Volume
               col="red", 
               pch=16)

#Adicionando outliers
g_outliers <- c(Girth,18,20,17)
h_outliers <- c(Height,64,64,65)
v_outliers <- c(Volume,19,18,19)
fit2<-lm(v_outliers ~ g_outliers+h_outliers) #Modelo com outliers
# Planos das regressões
graph$plane3d(fit, col="blue") #plano sem outliers
graph$plane3d(fit2, col="red") #plano com outliers
summary(fit2)

############### Detectar Outliers na Regressao Multipla ######################
### Detectar Outliers na Regressao Multipla
trees_o <- cbind(g_outliers, h_outliers, v_outliers) #cria um dataframe com colunas
  #data.frame faria o mesmo
  #rbind criaria com linhas
alfa <- 0.05         # probabilidade de erro tipo I para detectar outlier 
p_val <- 1-alfa      # 1 - alfa 
p <- ncol(trees_o)   # número de parâmetros (VI + VD)
n <- nrow(trees_o)   # n observações

## Distancia de Cook
plot(fit2, which=5)   #Resíduo vs Alavancagem
plot(fit2, which=4)   #Valor absoluto da distância de Cook
critico <- 4/n                       # definir
abline(a=critico, b=0, col="red", lty=2)
cook <- as.vector(cooks.distance(fit2)) #Sem "como.vetor", mostra o número da observação
which(cook > critico)   #Quais observações possuem valor de cook maior que o valor crítico

# Distancia Mahalonobis; frequência vs distância de cook
d <- mahalanobis(trees_o,             #  dados (p colunas)
                 colMeans(trees_o),   # medias de cada parametro (centro de D)
                 cov(trees_o))        # matriz de covariancias (p x p)
plot(density(d, bw = 0.5),
     main=paste0("Distancias ao quadrado de Mahalanobis, n=",n,", p=",p+1)) 
rug(d)

gl <- p-1                             # graus de liberdade neste caso = 2
critico <-qchisq(p = p_val, df=gl)    # crítico equivale ao qui² onde p-valor é 0,05
which(d > critico)   # observações cujas distâncias de cook são diferentes de zero

# Dffits - Desvio do Ajuste
ajuste <- as.vector(dffits(fit2))
critico <- 2 * (sqrt(p+1))/(n) # amostras maiores (>50)
critico <- 1                     # amostras pequenas (<50)
which(abs(ajuste) > critico)

# Dfbeta - desvio da inclinacao
d_beta <- as.vector(dfbeta(fit2))
critico1 <- 2/sqrt(n) # amostras grandes
which(abs(d_beta) > critico1) # amostras pequenas (<50)
critico2 <- 1         # amostras pequenas; ou 4/n
which(abs(d_beta) > critico2) # amostras pequenas (<50)

# Residuos estudentizados; como o "Scale-Location", mas com a distribuição t.
require(MASS)
res_stu<-as.vector(studres(fit2))
plot(res_stu^2)   # O eixo Y equivale a t-student
critico <- qt(p=p_val, df=n-1)
abline(a=critico^2,b=0,col="red")
which(abs(res_stu) > critico)


######################### Multicolinearidade ###########################
pairs(trees, lower.panel=flines, upper.panel = fcor)
round(cor(trees),2)

rho_parcial <- function(Y=Y, X1=X1, X2=X2){
  rho_Y_X1 <- cor(Y, X1)
  rho_Y_X2 <- cor(Y, X2)
  rho_X1X2 <- cor(X1, X2)
  rho_parcial_X1 <- (rho_Y_X1-(rho_Y_X2*rho_X1X2))/sqrt(1-rho_X1X2^2)
  return(rho_parcial_X1)  
}

a <- rho_parcial(Y=Volume, X1=Girth, X2=Height)^2
b <- rho_parcial(Y=Volume, X1=Height, X2=Girth)^2
c <- cor(Volume, Height)^2 - b
c <- cor(Volume, Girth)^2 - a
d <- 1 -a -b -c
a+b+c+d

######################### VIF e Tolerancia #############################
install.packages("car")
require(car)
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars)
plot(fit)
vif(fit)
sqrt(vif(fit))>2
ceresPlots(fit)  #Sugere ajuste polinomial (em lilás)

require(datasets) 
data(swiss)
pairs(swiss, lower.panel=flines, upper.panel = fcor)
fit <- lm(Fertility ~ ., data = swiss)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
vif(fit)
sqrt(vif(fit))>2

fit2 <- lm(Fertility ~ Agriculture+Education+Catholic+Infant.Mortality, 
          data = swiss)

summary(fit2)
plot(fit2)

anova(fit, fit2)
  #quanto menor o valor, melhor o modelo
AIC(fit, fit2) #critério de informação de Akayke; um valor para cada modelo.
BIC(fit, fit2) #critério de informação bayesiana

fit3<-lm(Fertility~Education, swiss)
summary(fit3)
plot(fit3)

AIC(fit,fit2,fit3)
BIC(fit,fit2,fit3)
anova(fit,fit2,fit3)

############################# Variável Dummy ############################
egrandis <- read.csv("http://ecologia.ib.usp.br/bie5782/lib/exe/fetch.php?media=dados:egrandis.csv",
                     sep = ";")
summary(egrandis)
mod <- lm( ht ~ dap + regiao, data=egrandis )
levels(egrandis$regiao)
summary(mod)
model.matrix(mod)[1,]
amostra <-sample(1:nrow(egrandis), size=10)
model.matrix(mod)[amostra,]

if(!require(ggplot2)){install.packages("ggplot2")}
ggplot(egrandis, aes(x=dap, y=ht, color=regiao, group=regiao))+
  geom_point()+geom_smooth(method = "lm", se=F, color="black")+
  facet_grid(regiao~.)+theme_classic()
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))
plot(mod,which=4)
abline(a=4/nrow(egrandis),b=0,col="red")
mod2 <- lm( ht ~ dap*regiao, data=egrandis )
summary(mod2)
model.matrix(mod2)[amostra,]
AIC(mod, mod2)
anova(mod, mod2)
mod3<- lm( ht ~ dap, data=egrandis )
mod4<- lm( ht ~ regiao, data=egrandis )
AIC(mod, mod2, mod3, mod4)
anova(mod, mod2, mod3, mod4)

novo <- data.frame(dap=5, regiao="Botucatu")
predict(mod2, novo, interval="confidence") 
predict(mod2, novo, interval="prediction")
# ver: https://stats.stackexchange.com/questions/16493/difference-between-confidence-intervals-and-prediction-intervals

#############
# setwd("C:/Users/User/Documents/Disciplinas/Pós estatistica/07 - Regressão Múltipla")
# Exercicio 1
Trabalho<-read.csv("Trabalho.csv")
plot(lm(TMP~Experiencia+Turno, Trabalho))
Trabalho <- Trabalho[-c(205,206),]
plot(lm(TMP~Experiencia+Turno, Trabalho))


# Exercicio 2
Vendas <-read.csv("Vendas.csv")
Vendas<-Vendas[-c(31,32,33),]
mod1<-lm(Vendas ~ Anos.de.experiencia+Score.no.Teste+interacao, Vendas)
mod2<-lm(Vendas ~ Anos.de.experiencia*Score.no.Teste, Vendas)
summary(mod2)


data("trees")
mod<-lm(Volume ~ Girth+Height, trees)
vif(mod)

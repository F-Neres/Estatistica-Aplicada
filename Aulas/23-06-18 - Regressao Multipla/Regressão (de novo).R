require(MASS)
#Exige o pacote de dados "MASS"

data(cats)
#Abre os dados "cats" (do pacote "MASS")
head(cats,8)
tail(cats,11)

summary(cats)

attach(cats)
#Desanexa os vetores (colunas) do banco de dados.

plot(Hwt~Bwt,cats)
title(main="Massa do Coração (g) vs. Massa Corpórea (Kg)\nde Gatos Domésticos")
abline(lm(Hwt~Bwt,cats),col="red")

lm(Hwt~Bwt,cats)
#Hwt = -0.3567 + 4.0341*Bwt + erro
#Hwt equivale a Y_hat

    mod<-lm(Hwt~Bwt,cats)
mod
#Atribui ao objeto denominado "mod" o modelo linear.

summary(lm(Hwt~Bwt))
#Resíduos devem ter média igual a zero e distribuição normal.
#Coeficientes
  #"Estimate" é o coeficiente em si
  #"Pr(>|t|)" é o p-valor do teste t sobre a H0 de que o coeficiente é igual a zero.
    #Se significativo, então não é igual a zero.
#

abline(a=-0.3567, b=4.0341, lty=4)

      #Resíduos nos gráficos
plot(mod,which=1)
  #Resíduos e Y
  #Média zero e variância constante (homocedasticidade)
plot(mod,which=2)
  #Resíduos e os quantis.
  #O ideal é que os dados se agrupem ao longo da sua linearidade, indicando normalidade dos resíduos.
residuos<-residuals(lm(Hwt~Bwt,cats))
  #Estrai os resíduos do modelo como um novo vetor denominado "reasíduos".
shapiro.test(residuals(lm(Hwt~Bwt,cats)))
  #Teste de normalidade dos dados "resíduos".
plot(mod,which=3)
  #Como o primeiro gráfico, mas o eixo Y é elevado ao quadrado.
  #Verifica-se há homocedasticidade.
plot(mod,which=4)
  #Distância de Cook

par(mfrow=c(2,2)) #Regra para gráficos serem apresentados como 2x2
plot(lm(Hwt~Bwt,cats))
  #O quarto gráfico mostra resíduos contra alavancagem, e uma margem de linhas críticas considerando a distãncia de cook.
par(mfrow=c(1,1))
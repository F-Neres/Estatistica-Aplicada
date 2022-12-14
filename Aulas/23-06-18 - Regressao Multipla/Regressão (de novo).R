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
title(main="Massa do Cora??o (g) vs. Massa Corp?rea (Kg)\nde Gatos Dom?sticos")
abline(lm(Hwt~Bwt,cats),col="red")

lm(Hwt~Bwt,cats)
#Hwt = -0.3567 + 4.0341*Bwt + erro
#Hwt equivale a Y_hat

    mod<-lm(Hwt~Bwt,cats)
mod
#Atribui ao objeto denominado "mod" o modelo linear.

summary(lm(Hwt~Bwt))
#Res?duos devem ter m?dia igual a zero e distribui??o normal.
#Coeficientes
  #"Estimate" ? o coeficiente em si
  #"Pr(>|t|)" ? o p-valor do teste t sobre a H0 de que o coeficiente ? igual a zero.
    #Se significativo, ent?o n?o ? igual a zero.
#

abline(a=-0.3567, b=4.0341, lty=4)

      #Res?duos nos gr?ficos
plot(mod,which=1)
  #Res?duos e Y
  #M?dia zero e vari?ncia constante (homocedasticidade)
plot(mod,which=2)
  #Res?duos e os quantis.
  #O ideal ? que os dados se agrupem ao longo da sua linearidade, indicando normalidade dos res?duos.
residuos<-residuals(lm(Hwt~Bwt,cats))
  #Estrai os res?duos do modelo como um novo vetor denominado "reas?duos".
shapiro.test(residuals(lm(Hwt~Bwt,cats)))
  #Teste de normalidade dos dados "res?duos".
plot(mod,which=3)
  #Como o primeiro gr?fico, mas o eixo Y ? elevado ao quadrado.
  #Verifica-se h? homocedasticidade.
plot(mod,which=4)
  #Dist?ncia de Cook

par(mfrow=c(2,2)) #Regra para gr?ficos serem apresentados como 2x2
plot(lm(Hwt~Bwt,cats))
  #O quarto gr?fico mostra res?duos contra alavancagem, e uma margem de linhas cr?ticas considerando a dist?ncia de cook.
par(mfrow=c(1,1))
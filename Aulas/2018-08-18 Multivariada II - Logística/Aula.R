data("infert")
head(infert)
summary(infert)
#Como a m?dia e a mediana da idade concentram-se em torno de 31 anos, pode-se supor previamente que haja uma concentra??o em torno desses valores.

#Verifica??o da normalidade
qqnorm(infert$age)
qqline(infert$age)
shapiro.test(infert$age)
ks.test(infert$age) #Teste de normalidade smirnov-kolmogorov

install.packages("nortest") #pacote para o teste de normalidade Anderson-Darlin
require(nortest)
ad.test(infert$age)

glm(formula = case ~ parity, family = binomial, data = infert)
glm(formula = case ~ parity+age, family = binomial, data = infert)
glm(formula = case ~ parity+age+education, family = binomial, data = infert)
#A vari?vel dependente ? obrigatoriamente bin?ria.
#Coeficientes: A cada n?vel ou unidade da vari?vel, a chance do evento Y ocorrer ? multiplicada pelo coeficiente.
#A fun??o preditora n?o resulta em Y, mas na probabilidade de Y
   #P[Y] = B0 + B1*X1 + B2*X2 ... + Bn*Xn
#Intercepto:
#Critetio de Akaike (AIC): O poder explicativo do modelo. Faz-se o teste para v?rios modelos
#N?o h? R?, ent?o utiliza-se apenas o AIC.

################################################################################

install.packages("AER")
require(AER)
data("Affairs")
head(Affairs)
summary(Affairs)
str(Affairs, strict.width = "wrap")

#Transformando a vari?vel discreta em bin?ria:
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels = c(0, 1), labels = c("No", "Yes"))
table(Affairs$ynaffairs)

fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, data = Affairs, family = binomial())
summary(fit.full)

##Fazer o Z a partir dos slides

fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, data = Affairs, family = binomial())
summary(fit.reduced)

anova(fit.reduced, fit.full, test = "Chisq")

coef(fit.reduced)
exp(coef(fit.reduced))
#A interpreta??o ? a partir de 1; valores inferiores s?o uma redu??o da chance.

#Executando a predi??o com o modelo obtido
#Mantendo todas as vari?veis independentes como a m?dia, exceto 'rating', que ter-se-? seu pr?prio escopo
testdata <- data.frame(rating=c(1,2,3,4,5),
                       age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata
testdata$prob <- predict(fit.reduced, newdata = testdata, type = "response")
testdata

###

testdata.exer1 <- data.frame(rating=mean(Affairs$rating),
                            age=mean(Affairs$age),
                            yearsmarried=mean(Affairs$yearsmarried),
                            religiousness=c(1,2,3,4,5))
testdata.exer1
testdata.exer1$prob <- predict(fit.reduced, newdata = testdata, type = "response")
testdata.exer1

#Adicionando 'occupation' ao modelo reduzido
fit.reduced.occup <- glm(ynaffair ~ age + yearsmarried + religiousness + rating + occupation, data = Affairs, family = binomial())
summary(fit.reduced.occup)

anova(fit.reduced.occup, fit.reduced, test = "Chisq")

coef(fit.reduced.occup)
exp(coef(fit.reduced.occup))

testdata.exer2 <- data.frame(rating=mean(Affairs$rating),
                             age=mean(Affairs$age),
                             yearsmarried=mean(Affairs$yearsmarried),
                             religiousness=c(1,2,3,4,5),
                             occupation=mean(Affairs$occupation))
testdata.exer2$prob <- predict(fit.reduced, newdata = testdata, type = "response")
testdata.exer2





###


fit.relig <- glm(ynaffair ~  religiousness, data = Affairs, family = binomial())
summary(fit.relig)
fit.occup <- glm(ynaffair ~  occupation, data = Affairs, family = binomial())
summary(fit.relig.occup)
fit.relig.occup <- glm(ynaffair ~  religiousness + occupation, data = Affairs, family = binomial())
summary(fit.relig.occup)
################################################################################

    #Modelo de Overdispersion
#Averigua-se a dispers?o dos res?duos em rela??o ? m?dia.

################################################################################

    #Utilizando o banco de dados 'titanic3'
install.packages("ggplot2")
require(ggplot2)
theme_set(theme_bw())
install.packages("GGally")
library(GGally)
install.packages("pscl")
library(pscl)
install.packages("broom")
library(broom)
install.packages("tidyverse")
require(tidyverse)
#Vari?veis categ?ricas n?o ser?o aceitas pelo comando 'ggpairs'
#Os pr?ximos 5 comandos removem as vari?veis indesejadas.
titanic3 <- select(titanic3, -name)
titanic3 <- select(titanic3, -ticket)
titanic3 <- select(titanic3, -cabin)
titanic3 <- select(titanic3, -boat)
titanic3 <- select(titanic3, -home.dest)
ggpairs(titanic3)
titanic3 %>%
  ggplot(aes(x = sex, fill = survived)) + geom_bar(position = "dodge")
ggplot(titanic3, aes(x = survived, y = fare)) +
  #geom_violin(aes(fill = survived), alpha = .4) +
  geom_boxplot(aes(fill = survived), alpha = .4) +
  #geom_count() +
  geom_jitter(width = .1, alpha = .3) + coord_flip() + scale_y_log10()

titanic_t <- titanic3 %>%
  filter(fare > 0) %>%
  mutate(logFare = log(fare), # cria ou modifica colunas
         survived = as.factor(survived)) 
# glm que usaremos abaixo lida melhor com factor que character

bm <- glm(survived ~ logFare, data = titanic_t, family = "binomial")
tidy(bm, conf.int = TRUE)

tidy(bm, conf.int = TRUE, exponentiate = TRUE)                                                    

bm %>% augment(type.predict = "response")  %>% mutate(survivedNum = ifelse(survived == "1", 1, 0)) %>% ggplot(aes(x = logFare)) + geom_count(aes(y = survivedNum), alpha = 0.5) + geom_line(aes(y = .fitted))                                                     
#Correla??o probabil?stica entre 'survived' e 'fare'
#'estimate' ? a chance dada a unidade da vari?vel independente.

bm <- glm(survived ~ pclass,data = titanic_t,family = "binomial")
tidy(bm, conf.int = TRUE)
glance(bm)
# Os m?todos glance sempre retornam um quadro de dados de uma linha (exceto em NULL, que retorna um quadro de dados vazio)
#Comparando ambas regress?es.

summary(bm)

bm %>% augment(type.predict = "response")  %>% mutate(survivedNum = ifelse(survived == "1", 1, 0)) %>% ggplot(aes(x = pclass)) + geom_count(aes(y = survivedNum), alpha = 0.5) + geom_point(aes(y = .fitted), color = "blue")
#Gr?fico que represen?a frequ?ncia.
#O c?rculo azul baliza o n?mero de sobreviventes e n?o-sobreviventes.

##Multivariada#
bm <- glm(survived ~ pclass + sex + age + sex*age,data = titanic_t,family = "binomial")
tidy(bm, conf.int = TRUE)
tidy(bm, conf.int = TRUE, exponentiate = TRUE) 
glance(bm)


bm %>% augment(type.predict = "response")

predictions <- predict(bm, type = "response") > .5
titanic_t = titanic_t %>% mutate(true_survivals = survived == 1)
titanic_t

erro <- sum((predictions != titanic_t$true_survivals)) / NROW(predictions)
erro

data("infert")
head(infert)
summary(infert)
#Como a média e a mediana da idade concentram-se em torno de 31 anos, pode-se supor previamente que haja uma concentração em torno desses valores.

#Verificação da normalidade
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
#A variável dependente é obrigatoriamente binária.
#Coeficientes: A cada nível ou unidade da variável, a chance do evento Y ocorrer é multiplicada pelo coeficiente.
#A função preditora não resulta em Y, mas na probabilidade de Y
   #P[Y] = B0 + B1*X1 + B2*X2 ... + Bn*Xn
#Intercepto:
#Critetio de Akaike (AIC): O poder explicativo do modelo. Faz-se o teste para vários modelos
#Não há R², então utiliza-se apenas o AIC.

################################################################################

install.packages("AER")
require(AER)
data("Affairs")
head(Affairs)
summary(Affairs)
str(Affairs, strict.width = "wrap")

#Transformando a variável discreta em binária:
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
#A interpretação é a partir de 1; valores inferiores são uma redução da chance.

#Executando a predição com o modelo obtido
#Mantendo todas as variáveis independentes como a média, exceto 'rating', que ter-se-á seu próprio escopo
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
#Averigua-se a dispersão dos resíduos em relação à média.

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
#Variáveis categóricas não serão aceitas pelo comando 'ggpairs'
#Os próximos 5 comandos removem as variáveis indesejadas.
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
#Correlação probabilística entre 'survived' e 'fare'
#'estimate' é a chance dada a unidade da variável independente.

bm <- glm(survived ~ pclass,data = titanic_t,family = "binomial")
tidy(bm, conf.int = TRUE)
glance(bm)
# Os métodos glance sempre retornam um quadro de dados de uma linha (exceto em NULL, que retorna um quadro de dados vazio)
#Comparando ambas regressões.

summary(bm)

bm %>% augment(type.predict = "response")  %>% mutate(survivedNum = ifelse(survived == "1", 1, 0)) %>% ggplot(aes(x = pclass)) + geom_count(aes(y = survivedNum), alpha = 0.5) + geom_point(aes(y = .fitted), color = "blue")
#Gráfico que represença frequência.
#O círculo azul baliza o número de sobreviventes e não-sobreviventes.

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

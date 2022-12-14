##########################################################################
#
#                                  Probabilidade
#
##########################################################################


############ Modelo Binomial: exemplo da linha de produ??o ###############

# A: Probabilidade de uma pe?a defeituosa
dbinom(x = 1, size = 10, prob = 0.1)

# B: Probabilidade de nenhuma pe?a defeituosa
dbinom(x = 0, size = 10, prob = 0.1)

# C: Probabilidade de duas pe?as defeituosas
dbinom(x = 2, size = 10, prob = 0.1)

# D: Probabilidade de no m?nimo duas pe?as defeituosas
dbinom(x = 2, size = 10, prob = 0.1) + pbinom(q = 2, size = 10, prob = 0.1, lower.tail = FALSE)

# E: Probabilidade de no m?ximo duas pe?as defeituosas
pbinom(q = 2, size = 10, prob = 0.1, lower.tail = TRUE)

####### Modelo Poisson: exemplo da emiss?o de part?culas alfa ############

# A: Probabilidade de uma emiss?o por minuto
dpois(x = 1, lambda = 5)

# B: Probabilidade de n?o ocorrer emiss?o em um minuto
dpois(x = 0, lambda = 5)

# C: Probabilidade de duas ou mais emiss?es em um minuto
dpois(x = 2, lambda = 5) + ppois(q = 2, lambda = 5, lower.tail = FALSE)

# D: Probabilidade de no m?ximo 3 emiss?es em um minuto
ppois(q = 3, lambda = 5, lower.tail = TRUE)

# Gr?fico
x <- seq(0, 10)
plot(x, dpois(x, lambda = 5), pch = 16, ylab = 'P(X=x)', 
     main = 'Fun??o discreta de probabilidade')

############################ Modelo Normal #########################

# A: Probabilidade de 2 < X < 5
pnorm(q = 5, mean = 2, sd = 3, lower.tail = TRUE) - pnorm(q = 2, mean = 2, sd = 3, lower.tail = TRUE)

# B: Probabilidade de 0 < X < 2
pnorm(q = 2, mean = 2, sd = 3, lower.tail = TRUE) - pnorm(q = 0, mean = 2, sd = 3, lower.tail = TRUE)

# C: Probabilidade de X < 3
pnorm(q = 3, mean = 2, sd = 3, lower.tail = TRUE)

# D: Probabilidade de X > 4
pnorm(q = 4, mean = 2, sd = 3, lower.tail = FALSE)

########### Aproxima??o da Binomial pela Normal #########################
pbinom(q = 50, size = 200, prob = 0.3, lower.tail = FALSE)
pnorm(q = 50, mean = 60, sd = sqrt(42), lower.tail = FALSE)

par(mfrow=c(2,2))
x <- seq(0,10)
barplot(dbinom(x, size = 10, prob = 0.3), main = 'n = 10 e p = 0,3')
x <- seq(0,30)
barplot(dbinom(x, size = 30, prob = 0.3), main = 'n = 30 e p = 0,3')
x <- seq(0,50)
barplot(dbinom(x, size = 50, prob = 0.3), main = 'n = 50 e p = 0,3')
x <- seq(0,100)
barplot(dbinom(x, size = 100, prob = 0.3), main = 'n = 100 e p = 0,3')



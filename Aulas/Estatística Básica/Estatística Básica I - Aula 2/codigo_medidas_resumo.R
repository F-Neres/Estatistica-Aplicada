##############################################################################################
#
#                                     Estat?stica Descritiva
#                                         Medidas Resumo
#
##############################################################################################

# Alterando diret?rio do R
setwd("G:/UNICID/Estat?stica B?sica I/Dados")

# Instalando pacotes
require(dplyr)
require(xlsx)

# Leitura dos dados
dados <- read.csv('dados_cadastro.csv', sep=';', dec=',')

# Impress?o dos dados
glimpse(dados)

# Calculando a m?dia das vari?veis quantitativas
mean(dados$N.de.Filhos, na.rm=TRUE)
mean(dados$Salario, na.rm=TRUE)
mean(dados$Idade, na.rm=TRUE)
mean(dados$Altura, na.rm=TRUE)
mean(dados$Peso, na.rm=TRUE)
mean(dados$Exercicio, na.rm=TRUE)

# Calculando a mediana das vari?veis quantitativas
median(dados$N.de.Filhos, na.rm=TRUE)
median(dados$Salario, na.rm=TRUE)
median(dados$Idade, na.rm=TRUE)
median(dados$Altura, na.rm=TRUE)
median(dados$Peso, na.rm=TRUE)
median(dados$Exercicio, na.rm=TRUE)

# Calculando a moda das vari?veis quantitativas
mode <- function(x) {
  ux <- unique(x)
  ux <- ux[which(is.na(ux)==FALSE)]
  ux[which.max(tabulate(match(x, ux)))]
}

mode(dados$N.de.Filhos)
mode(dados$Salario)
mode(dados$Idade)
mode(dados$Altura)
mode(dados$Peso)
mode(dados$Exercicio)

# Calculando a amplitude das vari?veis quantitativas
max(dados$N.de.Filhos, na.rm=TRUE) - min(dados$N.de.Filhos, na.rm=TRUE)
max(dados$Salario, na.rm=TRUE) - min(dados$Salario, na.rm=TRUE)
max(dados$Idade, na.rm=TRUE) - min(dados$Idade, na.rm=TRUE)
max(dados$Altura, na.rm=TRUE) - min(dados$Altura, na.rm=TRUE)
max(dados$Peso, na.rm=TRUE) - min(dados$Peso, na.rm=TRUE)
max(dados$Exercicio, na.rm=TRUE) - min(dados$Exercicio, na.rm=TRUE)

# Calculando o desvio-padr?o das vari?veis quantitativas
sd(dados$N.de.Filhos, na.rm=TRUE)
sd(dados$Salario, na.rm=TRUE)
sd(dados$Idade, na.rm=TRUE)
sd(dados$Altura, na.rm=TRUE)
sd(dados$Peso, na.rm=TRUE)
sd(dados$Exercicio, na.rm=TRUE)

# Calculando o coeficiente de varia??o das vari?veis quantitativas
sd(dados$N.de.Filhos, na.rm=TRUE)/mean(dados$N.de.Filhos, na.rm=TRUE)
sd(dados$Salario, na.rm=TRUE)/mean(dados$Salario, na.rm=TRUE)
sd(dados$Idade, na.rm=TRUE)/mean(dados$Idade, na.rm=TRUE)
sd(dados$Altura, na.rm=TRUE)/mean(dados$Altura, na.rm=TRUE)
sd(dados$Peso, na.rm=TRUE)/mean(dados$Peso, na.rm=TRUE)
sd(dados$Exercicio, na.rm=TRUE)/mean(dados$Exercicio, na.rm=TRUE)

# Calculando m?dia, desvio-padr?o e coeficiente de varia??o por grupo de Toler?ncia
resumo_tolerancia <- dados %>%
  group_by(Tolerancia) %>%
  summarize(Media = mean(Exercicio, na.rm=TRUE),
            DP = sd(Exercicio, na.rm=TRUE),
            CV = DP/Media)
resumo_tolerancia

media = aggregate(dados$Exercicio, by=list(dados$Tolerancia), FUN = mean, na.rm=TRUE)
dp = aggregate(dados$Exercicio, by=list(dados$Tolerancia), FUN = sd, na.rm=TRUE)
cv = dp/media

































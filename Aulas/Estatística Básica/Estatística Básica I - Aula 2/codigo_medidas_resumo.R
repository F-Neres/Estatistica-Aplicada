##############################################################################################
#
#                                     Estatística Descritiva
#                                         Medidas Resumo
#
##############################################################################################

# Alterando diretório do R
setwd("G:/UNICID/Estatística Básica I/Dados")

# Instalando pacotes
require(dplyr)
require(xlsx)

# Leitura dos dados
dados <- read.csv('dados_cadastro.csv', sep=';', dec=',')

# Impressão dos dados
glimpse(dados)

# Calculando a média das variáveis quantitativas
mean(dados$N.de.Filhos, na.rm=TRUE)
mean(dados$Salario, na.rm=TRUE)
mean(dados$Idade, na.rm=TRUE)
mean(dados$Altura, na.rm=TRUE)
mean(dados$Peso, na.rm=TRUE)
mean(dados$Exercicio, na.rm=TRUE)

# Calculando a mediana das variáveis quantitativas
median(dados$N.de.Filhos, na.rm=TRUE)
median(dados$Salario, na.rm=TRUE)
median(dados$Idade, na.rm=TRUE)
median(dados$Altura, na.rm=TRUE)
median(dados$Peso, na.rm=TRUE)
median(dados$Exercicio, na.rm=TRUE)

# Calculando a moda das variáveis quantitativas
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

# Calculando a amplitude das variáveis quantitativas
max(dados$N.de.Filhos, na.rm=TRUE) - min(dados$N.de.Filhos, na.rm=TRUE)
max(dados$Salario, na.rm=TRUE) - min(dados$Salario, na.rm=TRUE)
max(dados$Idade, na.rm=TRUE) - min(dados$Idade, na.rm=TRUE)
max(dados$Altura, na.rm=TRUE) - min(dados$Altura, na.rm=TRUE)
max(dados$Peso, na.rm=TRUE) - min(dados$Peso, na.rm=TRUE)
max(dados$Exercicio, na.rm=TRUE) - min(dados$Exercicio, na.rm=TRUE)

# Calculando o desvio-padrão das variáveis quantitativas
sd(dados$N.de.Filhos, na.rm=TRUE)
sd(dados$Salario, na.rm=TRUE)
sd(dados$Idade, na.rm=TRUE)
sd(dados$Altura, na.rm=TRUE)
sd(dados$Peso, na.rm=TRUE)
sd(dados$Exercicio, na.rm=TRUE)

# Calculando o coeficiente de variação das variáveis quantitativas
sd(dados$N.de.Filhos, na.rm=TRUE)/mean(dados$N.de.Filhos, na.rm=TRUE)
sd(dados$Salario, na.rm=TRUE)/mean(dados$Salario, na.rm=TRUE)
sd(dados$Idade, na.rm=TRUE)/mean(dados$Idade, na.rm=TRUE)
sd(dados$Altura, na.rm=TRUE)/mean(dados$Altura, na.rm=TRUE)
sd(dados$Peso, na.rm=TRUE)/mean(dados$Peso, na.rm=TRUE)
sd(dados$Exercicio, na.rm=TRUE)/mean(dados$Exercicio, na.rm=TRUE)

# Calculando média, desvio-padrão e coeficiente de variação por grupo de Tolerância
resumo_tolerancia <- dados %>%
  group_by(Tolerancia) %>%
  summarize(Media = mean(Exercicio, na.rm=TRUE),
            DP = sd(Exercicio, na.rm=TRUE),
            CV = DP/Media)
resumo_tolerancia

media = aggregate(dados$Exercicio, by=list(dados$Tolerancia), FUN = mean, na.rm=TRUE)
dp = aggregate(dados$Exercicio, by=list(dados$Tolerancia), FUN = sd, na.rm=TRUE)
cv = dp/media

































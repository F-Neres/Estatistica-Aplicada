###########################################################################
#
#                      ESTATÍSTICA BÁSICA I - AULA 2
#              ANÁLISE DESCRITIVA - DADOS DE CÂNCER DE MAMA
#
###########################################################################


# Carregar pacotes para análise
require(dplyr)

# Verificar variáveis dos dados
glimpse(estudo_cardio)

###### ITEM A ######
# Tabela de Frequências para sexo
table(estudo_cardio$SEXO, useNA = 'always')
prop.table(table(estudo_cardio$SEXO))

# Gráfico de setores para Sexo
nomes_categorias <- names(table(estudo_cardio$SEXO))
porcentagem <- round(prop.table(table(estudo_cardio$SEXO))*100)
legenda <- paste0(paste(nomes_categorias, porcentagem), '%')

pie(x = table(estudo_cardio$SEXO), labels = legenda, 
    col = c('light blue', 'dark blue'),
    main = 'Gráfico de setores para Sexo')

###### ITEM B ######
# Tabela de Frequências para Etiologia
table(estudo_cardio$ETIOLOGIA, useNA = 'always')
prop.table(table(estudo_cardio$ETIOLOGIA))

# Gráfico de barras para Etiologia
counts <- table(estudo_cardio$ETIOLOGIA)
barplot(counts, main='Gráfico de barras para Etiologia', 
        xlab='', ylim=c(0,40))

###### ITEM C ######
# Medidas resumo para IMC
mean(estudo_cardio$IMC, na.rm = TRUE)
median(estudo_cardio$IMC, na.rm = TRUE)
sd(estudo_cardio$IMC, na.rm = TRUE)

# Histograma para IMC
summary(estudo_cardio$IMC)
# Regra de Sturges
1+3.3*log10(127)
hist(estudo_cardio$IMC, breaks = c(15, 18, 20, 22, 25, 30, 32, 35, 40), freq = FALSE,
     main='Histograma para IMC', 
     xlab='', ylab='Densidade de frequência')

# Medidas resumo para FC
mean(estudo_cardio$FC, na.rm = TRUE)
median(estudo_cardio$FC, na.rm = TRUE)
sd(estudo_cardio$FC, na.rm = TRUE)
#frequência relativa 
hist(estudo_cardio$FC, breaks = 8, freq = FALSE,
     main='Histograma para FC', 
     xlab='', ylab='Densidade de frequência')

###### ITEM D ######
# Medidas resumo para VO2
mean(estudo_cardio$VO2, na.rm = TRUE)
median(estudo_cardio$VO2, na.rm = TRUE)
sd(estudo_cardio$VO2, na.rm = TRUE)
# Boxplot para VO2
boxplot(estudo_cardio$VO2)

###### ITEM E ######
# Medidas resumo para VO2 por Sexo
aggregate(estudo_cardio$VO2, by=list(estudo_cardio$SEXO), FUN = mean, na.rm=TRUE)
aggregate(estudo_cardio$VO2, by=list(estudo_cardio$SEXO), FUN = sd, na.rm=TRUE)

# Boxplot de VO2 por sexo
boxplot(estudo_cardio$VO2 ~ estudo_cardio$SEXO)

###### ITEM F ######
# Medidas resumo para VO2 por Etiologia
aggregate(estudo_cardio$VO2, by=list(estudo_cardio$ETIOLOGIA), FUN = mean, na.rm=TRUE)
aggregate(estudo_cardio$VO2, by=list(estudo_cardio$ETIOLOGIA), FUN = sd, na.rm=TRUE)

# Boxplot de VO2 por Etiologia
boxplot(estudo_cardio$VO2 ~ estudo_cardio$ETIOLOGIA)

###### ITEM G ######
# Gráfico de dispersão de VO2 por FC
plot(estudo_cardio$FC, estudo_cardio$VO2,
     ylab = 'Consumo de O2', xlab = 'Frequencia cardíaca')

###### ITEM H ######
# Gráfico de dispersão de VO2 por IMC
plot(estudo_cardio$IMC, estudo_cardio$VO2,
     ylab = 'Consumo de O2', xlab = 'IMC')

table(estudo_cardio$SEXO, estudo_cardio$ETIOLOGIA)
prop.table(table(estudo_cardio$SEXO, estudo_cardio$ETIOLOGIA), 1)

###### EXEMPLO DE TABELA DE DUPLA ENTRADA ######

# 100% fixado no total (toda a tabela soma 100%)
prop.table(table(estudo_cardio$SEXO, estudo_cardio$ETIOLOGIA))

# 100% fixado nas linhas (todas as linhas somam 100%)
prop.table(table(estudo_cardio$SEXO, estudo_cardio$ETIOLOGIA), 1)

# 100% fixado nas colunas (todas as colunas somam 100%)
prop.table(table(estudo_cardio$SEXO, estudo_cardio$ETIOLOGIA), 2)

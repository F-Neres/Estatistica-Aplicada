# Instalando pacotes
require(dplyr)

###################################### Exerc?cio 3 - Dieta  ##################################
# Alterando diret?rio do R
setwd("G:/UNICID/Estat?stica B?sica I/Dados")

# Leitura dos dados
dados_dieta <- read.csv('dados_dieta.csv', sep=';', dec=',')
glimpse(dados_dieta)

# Calculando a m?dia das dietas
mean(dados_dieta$Dieta_1, na.rm=TRUE)
mean(dados_dieta$Dieta_2, na.rm=TRUE)

# Calculando o desvio-padr?o das dietas
sd(dados_dieta$Dieta_1, na.rm=TRUE)
sd(dados_dieta$Dieta_2, na.rm=TRUE)

# Calculando o coeficiente de varia??o das dietas
sd(dados_dieta$Dieta_1, na.rm=TRUE)/mean(dados_dieta$Dieta_1, na.rm=TRUE)
sd(dados_dieta$Dieta_2, na.rm=TRUE)/mean(dados_dieta$Dieta_2, na.rm=TRUE)

###################################### Exerc?cio 4 - C?ncer  ##################################
# Alterando diret?rio do R
setwd("C:/Users/AB1068107/Desktop/FMU/FMU - Estat?stica Descritiva/Aulas/Aula 3")

# Leitura dos dados
dados_cancer <- read.csv('cancer.csv', sep=';', dec=',')
glimpse(dados_cancer)

# Item A
# Calculando a m?dia
mean(dados_cancer$Idade, na.rm=TRUE)
mean(dados_cancer$GL, na.rm=TRUE)

# Calculando o desvio-padr?o
sd(dados_cancer$Idade, na.rm=TRUE)
sd(dados_cancer$GL, na.rm=TRUE)

# Item B
# Calculando as medidas resumo para os grupos

# Com o pacote dplyr
resumo_grupos <- dados_cancer %>%
  group_by(Grupo) %>%
  summarize(Media_Idade = mean(Idade, na.rm=TRUE),
            Media_Glicose = mean(GL, na.rm=TRUE),
            DP_Idade = sd(Idade, na.rm=TRUE),
            DP_Glicose = sd(GL, na.rm=TRUE)) %>%
  mutate(CV_Idade = DP_Idade/Media_Idade,
         CV_Glicose = DP_Glicose/Media_Glicose)
resumo_grupos

# Sem o pacote dplyr
aggregate(dados_cancer$Idade, list(dados_cancer$Grupo), mean)
aggregate(dados_cancer$Idade, list(dados_cancer$Grupo), sd)
aggregate(dados_cancer$GL, list(dados_cancer$Grupo), mean)
aggregate(dados_cancer$GL, list(dados_cancer$Grupo), sd)

#################################### Exerc?cio 5 - Comunidade  ################################
# Alterando diret?rio do R
setwd("G:/UNICID/Estat?stica B?sica I/Dados")

# Leitura dos dados
dados_comunidade <- read.csv('comunidade.csv', sep=';', dec=',')
glimpse(dados_comunidade)

# Item A
# Calculando a m?dia
mean(dados_comunidade$Idade, na.rm=TRUE)

# Calculando o desvio-padr?o
sd(dados_comunidade$Idade, na.rm=TRUE)

# Item B
resumo_series <- dados_comunidade %>%
  group_by(Serie) %>%
  summarize(Media_Idade = mean(Idade, na.rm=TRUE),
            DP_Idade = sd(Idade, na.rm=TRUE),
            CV_Idade = DP_Idade/Media_Idade,
            Mediana_Idade = median(Idade, na.rm=TRUE))
resumo_series

# Item C
resumo_sexo <- dados_comunidade %>%
  group_by(Sexo) %>%
  summarize(Media_Idade = mean(Idade, na.rm=TRUE),
            DP_Idade = sd(Idade, na.rm=TRUE)) %>%
  mutate(CV_Idade = DP_Idade/Media_Idade)
resumo_sexo

###################################### Exerc?cio 6 - Pe?as  ##################################
# Alterando diret?rio do R
setwd("G:/UNICID/Estat?stica B?sica I/Dados")

# Leitura dos dados
dados_pecas <- read.csv('dados_pecas.csv', sep=';', dec=',')
glimpse(dados_pecas)

# Calculando a m?dia das dietas
media <- mean(dados_pecas$N_defeituosas, na.rm=TRUE)

# Calculando o desvio-padr?o das dietas
desvio_padrao <- sd(dados_pecas$N_defeituosas, na.rm=TRUE)

# Retirando dados que se distanciam da m?dia com menos ou mais dois desvios-padr?o

# Com dplyr
dados_pecas <- dados_pecas %>%
  filter(N_defeituosas > media - 2 * desvio_padrao,
         N_defeituosas < media + 2 * desvio_padrao)
media_nova <- mean(dados_pecas$N_defeituosas)
desvio_padrao_novo <- sd(dados_pecas$N_defeituosas)

# Sem dplyr
limite_inf <- media - 2 * desvio_padrao
limite_sup <- media + 2 * desvio_padrao
pecas <- as.numeric(ifelse(
  dados_pecas$N_defeituosas > limite_inf & dados_pecas$N_defeituosas < limite_sup,
  dados_pecas$N_defeituosas, 'NA'))
mean(pecas, na.rm=TRUE)
sd(pecas, na.rm=TRUE)






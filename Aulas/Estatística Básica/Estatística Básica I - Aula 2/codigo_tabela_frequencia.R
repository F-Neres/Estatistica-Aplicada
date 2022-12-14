##############################################################################################
#
#                                     Estat?stica Descritiva
#                                     Tabelas de frequ?ncias
#
##############################################################################################

# Alterando diret?rio do R
setwd("G:/UNICID/Estat?stica B?sica I/Dados")

# Instalando pacotes
require(dplyr)

# Leitura dos dados - forma alternativa
dados <- read.csv('dados_cadastro.csv', sep=';', dec=',')

# Impress?o dos dados
glimpse(dados)

# Construindo tabela de frequ?ncias absolutas para Turma
tab_turma <- table(dados$Turma)
tab_turma

# Construindo tabela de frequ?ncias relativas para Turma
tab_turma_rel <- prop.table(tab_turma)
tab_turma_rel

# Tabela de frequ?ncias para Turma
tabela_turma <- dados %>%
  count(Turma) %>%
  mutate(prop = prop.table(n))
tabela_turma

# Construindo tabela de frequ?ncias absolutas para Idade
tab_idade <- table(dados$Idade)
tab_idade

# Construindo tabela de frequ?ncias relativas para Idade
tab_idade_rel <- prop.table(tab_idade)
tab_idade_rel

# Tabela de frequ?ncias para Idade
tabela_idade <- dados %>%
  count(Idade) %>%
  mutate(prop = prop.table(n))
tabela_idade

# Construindo tabela de frequ?ncias absolutas para Toler?ncia
tab_tolerancia <- table(dados$Tolerancia)
tab_tolerancia

# Construindo tabela de frequ?ncias relativas para Toler?ncia
tab_tolerancia_rel <- prop.table(tab_tolerancia)
tab_tolerancia_rel

# Tabela de frequ?ncias para Toler?ncia
tabela_tolerancia <- dados %>%
  count(Tolerancia) %>%
  mutate(prop = prop.table(n))
tabela_tolerancia

# Salvando tabela de frequ?ncias de Toler?ncia no Excel
write.table(tabela_tolerancia, file='Tabela de Frequencias Tolerancia.csv', sep=';', dec=',', row.names=FALSE)





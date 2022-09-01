##############################################################################################
#
#                                     Estatística Descritiva
#                                     Tabelas de frequências
#
##############################################################################################

# Alterando diretório do R
setwd("G:/UNICID/Estatística Básica I/Dados")

# Instalando pacotes
require(dplyr)

# Leitura dos dados - forma alternativa
dados <- read.csv('dados_cadastro.csv', sep=';', dec=',')

# Impressão dos dados
glimpse(dados)

# Construindo tabela de frequências absolutas para Turma
tab_turma <- table(dados$Turma)
tab_turma

# Construindo tabela de frequências relativas para Turma
tab_turma_rel <- prop.table(tab_turma)
tab_turma_rel

# Tabela de frequências para Turma
tabela_turma <- dados %>%
  count(Turma) %>%
  mutate(prop = prop.table(n))
tabela_turma

# Construindo tabela de frequências absolutas para Idade
tab_idade <- table(dados$Idade)
tab_idade

# Construindo tabela de frequências relativas para Idade
tab_idade_rel <- prop.table(tab_idade)
tab_idade_rel

# Tabela de frequências para Idade
tabela_idade <- dados %>%
  count(Idade) %>%
  mutate(prop = prop.table(n))
tabela_idade

# Construindo tabela de frequências absolutas para Tolerância
tab_tolerancia <- table(dados$Tolerancia)
tab_tolerancia

# Construindo tabela de frequências relativas para Tolerância
tab_tolerancia_rel <- prop.table(tab_tolerancia)
tab_tolerancia_rel

# Tabela de frequências para Tolerância
tabela_tolerancia <- dados %>%
  count(Tolerancia) %>%
  mutate(prop = prop.table(n))
tabela_tolerancia

# Salvando tabela de frequências de Tolerância no Excel
write.table(tabela_tolerancia, file='Tabela de Frequencias Tolerancia.csv', sep=';', dec=',', row.names=FALSE)





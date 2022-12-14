###########################################################################
#
#                      ESTAT?STICA B?SICA I - AULA 2
#              AN?LISE DESCRITIVA - DADOS DE C?NCER DE MAMA
#
###########################################################################


# Carregar pacotes para an?lise
require(dplyr)

# Verificar vari?veis dos dados
glimpse(cancer_mama)

# Tabela de Frequ?ncias para Cor
table(cancer_mama$COR, useNA = 'always')
prop.table(table(cancer_mama$COR))

# Gr?fico de setores para Cor
nomes_categorias <- names(table(cancer_mama$COR))
porcentagem <- round(prop.table(table(cancer_mama$COR))*100)
legenda <- paste0(paste(nomes_categorias, porcentagem), '%')

pie(x = table(cancer_mama$COR), labels = legenda, 
    col = c('light blue','blue', 'dark blue'),
    main = 'Gr?fico de setores para Cor')

# Tabela de Frequ?ncias para Estado Civil
table(cancer_mama$ESTADO.CIVIL, useNA = 'always')
prop.table(table(cancer_mama$ESTADO.CIVIL))

# Gr?fico de barras para Estado Civil
counts <- table(cancer_mama$ESTADO.CIVIL)
barplot(counts, main='Gr?fico de barras para Estado Civil', 
        xlab='', ylim=c(0,35),
        names.arg=c('Casada', 'Descasada', 'Solteira', 'Vi?va'))

# Tabela de Frequ?ncias para c?ncer na fam?lia
table(cancer_mama$CANCER.MAMA.FAMILIA, useNA = 'always')
prop.table(table(cancer_mama$CANCER.MAMA.FAMILIA, useNA = 'always'))

# Gr?fico de setores para c?ncer na fam?lia
nomes_categorias <- names(table(cancer_mama$CANCER.MAMA.FAMILIA))
porcentagem <- round(prop.table(table(cancer_mama$CANCER.MAMA.FAMILIA))*100)
legenda <- paste0(paste(nomes_categorias, porcentagem), '%')

pie(x = table(cancer_mama$CANCER.MAMA.FAMILIA), labels = legenda, 
    col = c('light blue', 'dark blue'),
    main = 'Gr?fico de setores para Cor')

# Tabela de Frequ?ncias para status com rela??o ? menopausa
table(cancer_mama$STATUS.MENOPAUSA, useNA = 'always')
prop.table(table(cancer_mama$STATUS.MENOPAUSA))

# Gr?fico de barras para status com rela??o ? menopausa
counts <- table(cancer_mama$STATUS.MENOPAUSA)
barplot(counts, main='Gr?fico de barras para Status de menopausa', 
        xlab='', ylim=c(0,35),
        names.arg=c('P?s', 'Pr?'))

# Medidas resumo para idade
mean(cancer_mama$IDADE, na.rm = TRUE)
median(cancer_mama$IDADE, na.rm = TRUE)
sd(cancer_mama$IDADE, na.rm = TRUE)

# Histograma para idade
summary(cancer_mama$IDADE)
#frequ?ncia relativa 
hist(cancer_mama$IDADE, breaks = 10, freq = TRUE,
     main='Histograma para Idade', 
     xlab='', ylab='Frequ?ncia Relativa')
#densidade de frequ?ncia 
hist(cancer_mama$IDADE, breaks = c(25, 35, 40, 45, 55, 65, 75),
     freq = FALSE,
     main='Histograma para Idade', 
     xlab='', ylab='Densidade de frequ?ncia')

# Medidas resumo para tamanho do tumor
mean(cancer_mama$MEDIDA.TUMOR, na.rm = TRUE)
median(cancer_mama$MEDIDA.TUMOR, na.rm = TRUE)
sd(cancer_mama$MEDIDA.TUMOR, na.rm = TRUE)

# Boxplot para tamanho do tumor
boxplot(cancer_mama$MEDIDA.TUMOR)

# Medidas resumo para tamanho do tumor por cor
aggregate(cancer_mama$MEDIDA.TUMOR, by=list(cancer_mama$COR), FUN = mean, na.rm=TRUE)
aggregate(cancer_mama$MEDIDA.TUMOR, by=list(cancer_mama$COR), FUN = sd, na.rm=TRUE)

# Boxplot de tamanho do tumor por cor
boxplot(cancer_mama$MEDIDA.TUMOR ~ cancer_mama$COR)

# Medidas resumo para tamanho do tumor por estado civil
aggregate(cancer_mama$MEDIDA.TUMOR, by=list(cancer_mama$ESTADO.CIVIL), FUN = mean, na.rm=TRUE)
aggregate(cancer_mama$MEDIDA.TUMOR, by=list(cancer_mama$ESTADO.CIVIL), FUN = sd, na.rm=TRUE)

# Boxplot de tamanho do tumor por estado civil
boxplot(cancer_mama$MEDIDA.TUMOR ~ cancer_mama$ESTADO.CIVIL)

# Medidas resumo para tamanho do tumor por c?ncer na fam?lia
aggregate(cancer_mama$MEDIDA.TUMOR, by=list(cancer_mama$CANCER.MAMA.FAMILIA), FUN = mean, na.rm=TRUE)
aggregate(cancer_mama$MEDIDA.TUMOR, by=list(cancer_mama$CANCER.MAMA.FAMILIA), FUN = sd, na.rm=TRUE)

# Boxplot de tamanho do tumor por c?ncer na fam?lia
boxplot(cancer_mama$MEDIDA.TUMOR ~ cancer_mama$CANCER.MAMA.FAMILIA)

# Medidas resumo para tamanho do tumor por status de menopausa
aggregate(cancer_mama$MEDIDA.TUMOR, by=list(cancer_mama$STATUS.MENOPAUSA), FUN = mean, na.rm=TRUE)
aggregate(cancer_mama$MEDIDA.TUMOR, by=list(cancer_mama$STATUS.MENOPAUSA), FUN = sd, na.rm=TRUE)

# Boxplot de tamanho do tumor por status de menopausa
boxplot(cancer_mama$MEDIDA.TUMOR ~ cancer_mama$STATUS.MENOPAUSA)

# Gr?fico de dispers?o de tamanho do tumor pela idade
plot(cancer_mama$IDADE, cancer_mama$MEDIDA.TUMOR,
     ylab = 'Medida de tumor (mm)', xlab = 'Idade (anos)')

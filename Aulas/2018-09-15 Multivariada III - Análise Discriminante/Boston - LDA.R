#      1. Preparar banco de dados
#   a) Coloque todas as variáveis em uma mesma escala. scale(dados)

# Baixe/instale na biblioteca o pacote MASS.
if(!require(MASS)){install.packages('MASS')}

# Baixe/instale na biblioteca os dados Boston e veja o cabeçalho dos dados.
data("Boston")
head(Boston)

# Centralize e padronize (standardize), usando a função scale as variáveis contidas na tabela Boston.
boston_scaled <- scale(Boston)

# Verifique a classe do objeto boston_scaled (Está em matriz).
class(boston_scaled)

# Mude o objeto da classe matriz para a classe data.frame.
boston_scaled<-as.data.frame(boston_scaled)


#   b) Transforme a variável quantitativa em grupos. cut(Y, breaks=quantile(Y))

# Veja o resumo dos dados escalonados com a função summary.
summary(boston_scaled)

# Crie um vetor dos quantis da coluna crim da tabela Boston, e a visualize.
bins <- quantile(boston_scaled$crim)
bins

# Crie uma variável categórica 'crime‘ da coluna crim a partir dos quantis (bins), dando os rótulos (labels) de ‘baixo’, ‘med_baixo’, ‘med_alto’ e ‘alto’.
crime <- cut(boston_scaled$crim,
             labels=c("baixo", "med_baixo", "med_alto", "alto"),
             breaks = bins,  include.lowest = TRUE)

# Inclua o novo veto crime aos dados escalonados.
boston_scaled <- data.frame(boston_scaled, crime)
boston_scaled <- dplyr::select(boston_scaled,-crim) #exige o pacote "dplyr"
# ou:
boston_scaled$crim <- NULL
#   boston_scaled <- boston_scaled[,-1] # CUIDADO ERRAR! Elimina a primeira coluna, na ausência de dplyr.


#   c) Faça subamostras para: “teste” (20%) e “treino” (80%). sample(...)

# Encontre o número de linhas (nrow) dos dados escalonados de Boston.
n <- nrow(boston_scaled)

# Escolha aleatoriamente 80% das linhas.
set.seed(42)
ind <- sample(n,  size = n * 0.8)

# Crie os dados treino com as linhas escolhidas aleatoriamente.
treino <- boston_scaled[ind,] #linhas dos indivíduos selecionado (ind) e todas as colunas (nada depois da vírgula)

# Crie os dados teste.
teste <- boston_scaled[- ind,]

# Salve as classes corretas de crime dos dados teste (grupos determinados antes da função discriminante).
Classes_corretas <- teste$crime


#      2. Verifique os pressupostos do teste (nesse caso: LDA)
#   a) Normalidade das variáveis independentes. shapiro.test(...)
#   b) Linearidade dos parâmetros. (não há x² ou x³...)
#   c) Falta de multicolinearidade entre variáveis independentes. pairs(...)
#   d) Matrizes de dispersão são iguais?  biotools::boxM(cbind(x1,x2,...), Y)


# Avaliar variâncias, correlações, relações não lineares, outras distribuições (não normais).
pairs(treino[,1:7])
pairs(treino[,8:13])
install.packages("GGally")
require(GGally)
ggpairs(treino, aes(color=crime))

# Avaliar distribuiçao normal da variável 1, no grupo “alto”.
shapiro.test(treino[treino$crime=="alto",8])
# Deveria ser feito para cada combinação de grupo e variável.

# Avaliar se Matrizes de dispersão são iguais.
# Se p<0.05 Matrizes de Var-Covar não são iguais.
if(!require(biotools)){install.packages('biotools')}
biotools::boxM(treino[, 1:13], treino$crime) #usando variáveis de 1 a 13, numéricas, em relação à categórica.
#As matrizes de var-covar são diferentes. Recomenda-se a não utilizar LDA.


#      3. Construa o modelo LDA:  lda(Y~x1+x2..., data=dados)

lda_boston <- lda(crime~.,data=treino, cv=TRUE)
lda_boston

### Centroides
# 1º: obter valores dos LDs para cada observação
LD_treino <- predict(lda_boston, treino)$x # $x indica "para cada uma das observações
LD_treino<-as.data.frame(LD_treino)
# 2º. Criar coluna com os nomes das classes a que pertencem
LD_treino$crime<-treino$crime
head(LD_treino)

centroides <- data.frame(
  LD1=tapply(LD_treino$LD1, 
             LD_treino$crime, 
             mean),
  LD2=tapply(LD_treino$LD2, 
             LD_treino$crime, 
             mean),
  LD3=tapply(LD_treino$LD3, 
             LD_treino$crime, 
             mean)
)
centroides

# 3º. Construir gráfico com os grupos em relação as FDs
if(!require(ggplot2)){install.packages("ggplot2")}
ggplot(LD_treino,
       aes(x=LD1, y=LD2,              # FDs 1 e 2
           shape=crime, color=crime))+
  geom_point()+
  stat_ellipse(aes(x=LD1,y=LD2,color=crime),type='norm')
# centroide do grupo "alto", coordenada LD1
mean(LD_treino$LD1[LD_treino$crime=="alto"])
# centroide do grupo "alto", coordenada LD2
mean(LD_treino$LD2[LD_treino$crime=="alto"])
# centroide do grupo "alto", coordenada LD3
mean(LD_treino$LD3[LD_treino$crime=="alto"])

# Gráfico em 3D interativo
if(!require(plotly)) {install.packages("plotly")}
plotly::plot_ly(LD_treino, x=~LD1, y=~LD2,z=~LD3,color=~crime)


#      5. Faça a validação do teste
#   a) Validação cruzada (CV=TRUE).
#   b) predict com a subamostra “teste”  Tabela confusão

#### Passo 5. Validação
predicao <- predict(lda_boston, teste) # testar modelo
predicao$posterior # probabilidades de pertencer a cada grupo
predicao$class     # valores previstos conforme modelo
previstos <- predicao$class
reais <-teste$crime
confusao <- table(Classes_corretas,predicao$class)  # Tabela confusão
confusao
mean(diag(confusao)/table(reais))

#Q de Press
N <- length(reais)
n <- sum (reais==previstos)
K <- length(levels(reais))

qpress <- ((N-(n*K))^2)/(N*(K-1))
1-pchisq(qpress, 3) # se zero, modelo muito bom.


#      Sugestões
#   Melhorar as divisões dos grupos (Juntar alguns grupos que são muito próximos, no caso de faixa de valores )
#   Transformar variáveis que não forem lineares
###############################################################################################
#                        Modelagem de equações estruturais
###############################################################################################

# Alterando diretório
setwd("D:/UNICID/Modelagem de equações estruturais/MEE/Dados")

##### Lavaan modeling
install.packages("lavaan")
library(lavaan)

################### Exercício 1 ##################
dat <- read.csv("C:/Users/logonlb/Desktop/MEE/Dados/SEM.2.1-Intro to Lavaan data.csv")



# Step 1: Specify model
mod.1 <- 'y1 ~ x1
y2 ~ x1
y3 ~ y1 + y2'

# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=dat)

# Check variances
varTable(mod.1.fit)

## Recode vars to roughly same scale
x1 <- dat$x1/100
y1 <- dat$y1/100
y2 <- dat$y2
y3 <- dat$y3/100

### Create Transformed Dataset
# overwrite file with recoded data
t.dat <- data.frame(x1, y1, y2, y3)
summary(t.dat)

##### Repeat Lavaan modeling

# Step 1: Specify model
mod.1 <- 'y1 ~ x1
y2 ~ x1
y3 ~ y1 + y2'

# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=t.dat)

# Step 3: Extract results
summary(mod.1.fit, fit.measures = TRUE) 
standardizedSolution(mod.1.fit, type = 'std.all')

# Step 4: Request Modification Indices
summary(mod.1.fit, modindices = TRUE)

# Step 5: Specify a new model
mod.2 <- 'y1 ~ x1
          y2 ~ x1
          y3 ~ y1 + y2 + x1'

# Step 6: Estimate the new model
mod.2.fit <- sem(mod.2, data=t.dat)

# Step 7: Extract results of the new model
summary(mod.2.fit, fit.measures = TRUE)
standardizedSolution(mod.1.fit, type = 'std.all')





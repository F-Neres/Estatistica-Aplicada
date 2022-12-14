##########################################################################
#
#                       Infer?ncia - Teste de Hip?teses
#
##########################################################################


############ Teste t para m?dia populacional ###############
amostra1 = c(14.9,13.4,14.5,13.5,15.0,13.9,14.9,16.4,14.6,15.4)

# Igual contra diferente
t.test(amostra1, alternative = "two.sided", mu = 15, conf.level = 0.95) 

# Igual contra menor
t.test(amostra1, alternative = "less", mu = 15, conf.level = 0.95) 

# Igual contra maior
t.test(amostra1, alternative = "greater", mu = 15, conf.level = 0.95) 

############ Teste para propor??o populacional ###############
# x: n?mero de sucessos na amostra
# n: n?mero de realiza??es da amostra
# p: probabilidade de sucesso a ser testada
# alternative: two.sided, less ou greater
prop.test(x = 104, n= 200, p = 0.6, alternative = "two.sided", correct = F, conf.level = 0.95) 

      #Prostate

install.packages("faraway")
require(faraway)
prostate

#Stepwise via AIC
#Trace: mostra o processo do stepwise.
nulo = lm(lcavol ~ 1,data=prostate)
completo = lm(lcavol~.,data=prostate)
step(completo,data=prostate,direction="backward",trace=TRUE)
step(nulo,scope = list(lower = nulo, upper = completo),data=prostate, direction = "forward", trace=TRUE)
step(completo,data=prostate,direction="both",trace=TRUE)

#Fazendo agora uma análise geral, temos:
summary(lm(lcavol ~ age + lbph + svi + gleason + lpsa, data=prostate))

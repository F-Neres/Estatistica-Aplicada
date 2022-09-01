data("airquality")

#Wind ~ Temp
plot(Wind~Temp, airquality)
Setembro <- airquality[airquality$Month==9,]
plot(Wind~Temp, Setembro)
mod1 <- lm(Wind~Temp, Setembro)
mod1
abline(mod1, col="red")
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))


#Temp~Solar.R
Setembro <- airquality[airquality$Month==9,]
plot(Temp~Solar.R, airquality)
mod2 <- lm(Wind~Temp, airquality)
mod2
abline(mod2, col="red")
summary(mod2)
par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))

#Ozone~Wind
plot(Ozone~Wind, airquality)
mod3 <- lm(Ozone~Wind, airquality)
mod3
abline(mod3, col="red")
summary(mod3)
par(mfrow=c(2,2))
plot(mod3)
par(mfrow=c(1,1))


#Ozone ~ Solar.R
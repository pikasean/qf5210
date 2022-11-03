library("tseries")
library(forecast)
library(fGarch)

training_period = 1:116
validation_period = 117:128

#covid
covid <- read.csv('covid.csv')
plot(ts(covid[,2:4]))

par(mfrow=c(1,3))
change.covid.sg = ts(diff(covid$Singapore[training_period]))
plot(change.covid.sg)
change.covid.uk = ts(diff(covid$United.Kingdom[training_period]))
plot(change.covid.uk)
change.covid.us = ts(diff(covid$United.States[training_period]))
plot(change.covid.us)

#sg

sgA.garch = garchFit(~arma(5,1)+garch(1,1),data=change.covid.sg,cond.dist="norm")
summary(sgA.garch)
sgB.garch = garchFit(~arma(4,1)+garch(1,1),data=change.covid.sg,cond.dist="norm")
summary(sgB.garch)
sgA.garch.t = garchFit(~arma(5,1)+garch(1,1),data=change.covid.sg,cond.dist="std")
summary(sgA.garch.t)
sgB.garch.t = garchFit(~arma(4,1)+garch(1,1),data=change.covid.sg,cond.dist="std")
summary(sgB.garch.t)
sgC.garch.t = garchFit(~arma(6,1)+garch(1,1),data=change.covid.sg,cond.dist="std")
summary(sgC.garch.t)
sgD.garch.t = garchFit(~arma(4,2)+garch(1,1),data=change.covid.sg,cond.dist="std")
summary(sgD.garch.t)

forecast.sg = predict(sgA.garch, n.ahead = 2)
forecast.sg$meanForecast
forecast.sg$standardDeviation



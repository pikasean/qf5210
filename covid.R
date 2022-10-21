setwd('C:/Users/bingc/Files/NUS/Y4S1/QF5210/Group Project Data')
library("tseries")
library(forecast)
library(fGarch)

#covid
covid <- read.csv('covid.csv') 
plot(ts(covid[,2:4]))

par(mfrow=c(1,3))
change.covid.sg = ts(diff(covid$Singapore))
plot(change.covid.sg)
change.covid.uk = ts(diff(covid$United.Kingdom))
plot(change.covid.uk)
change.covid.us = ts(diff(covid$United.States))
plot(change.covid.us)

#sg
par(mfrow=c(1,2))
acf(change.covid.sg, lag.max=50)
pacf(change.covid.sg, lag.max=50)
auto.arima(change.covid.sg, ic = "aic")
auto.arima(change.covid.sg, ic = "bic")
sgA = arima(change.covid.sg, order = c(5,0,1)) 
sgB = arima(change.covid.sg, order = c(4,0,1)) 
tsdiag(sgA)
tsdiag(sgB)
acf(sgA$residuals ^2)
acf(sgB$residuals ^2)

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

length(change.covid.sg)
plot(sgD.garch.t@residuals)

forecast.sg = predict(sgD.garch.t, n.ahead = 2)
forecast.sg$meanForecast
forecast.sg$standardDeviation

#sg but from 80th observation onwards
change.covid.sg.recent = change.covid.sg[80:127]
plot(ts(change.covid.sg.recent))
par(mfrow=c(1,2))
acf(change.covid.sg.recent, lag.max=50)
pacf(change.covid.sg.recent, lag.max=50)
auto.arima(change.covid.sg.recent, ic = "aic")
auto.arima(change.covid.sg.recent, ic = "bic")
sgR = arima(change.covid.sg.recent, order = c(2,0,1)) 
tsdiag(sgR)
acf(sgR$residuals ^2)
#sgR.garch = garchFit(~arma(2,1)+garch(1,1),data=change.covid.sg,cond.dist="std")
#summary(sgR.garch)
pred.sg.recent = predict(sgR, n.ahead = 8, se.fit = TRUE) 
pred.sg.recent$pred
pred.sg.recent$se
plot(sgR$residuals)

#uk
par(mfrow=c(1,2))
acf(change.covid.uk, lag.max=50)
pacf(change.covid.uk, lag.max=50)
#auto.arima(covid$United.Kingdom, ic = "aic")
auto.arima(change.covid.uk, ic = "aic")
auto.arima(change.covid.uk, ic = "bic")
uk = arima(change.covid.uk, order = c(2,0,1)) 
tsdiag(uk)
acf(uk$residuals ^2)
uk.garch = garchFit(~arma(2,1)+garch(1,1),data=change.covid.uk,cond.dist="std")
summary(uk.garch)
plot(uk.garch@residuals)

forecast.uk = predict(uk.garch, n.ahead = 8)
forecast.uk$meanForecast
forecast.uk$standardDeviation

#us
par(mfrow=c(1,2))
acf(change.covid.us, lag.max=50)
pacf(change.covid.us, lag.max=50)
#auto.arima(covid$United.States, ic = "aic")
auto.arima(change.covid.us, ic = "aic")
auto.arima(change.covid.us, ic = "bic")
usA = arima(change.covid.us, order = c(0,0,5))
usB = arima(change.covid.us, order = c(3,0,1))
usC = arima(change.covid.us, order = c(2,0,3))
tsdiag(usA)
tsdiag(usB)
tsdiag(usC)
par(mfrow=c(1,3))
acf(usA$residuals ^2)
acf(usB$residuals ^2)
acf(usC$residuals ^2)

usC.garch = garchFit(~arma(3,1)+garch(2,2),data=change.covid.us,cond.dist="std")
summary(usC.garch)

#VAR
library("MTS")
change.covid = apply(covid[,2:4],2,diff)
ccm(change.covid)
covid0=VARorder(change.covid, maxp = 20)
covid1 = VAR(change.covid,5)
covid2 = refVAR(covid1, thres=1.96)

MTSdiag(covid2,adj=12)
VARpred(covid2,8)
colMeans(change.covid) 
sqrt(apply(change.covid,2,var))

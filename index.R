setwd('C:/Users/bingc/Files/NUS/Y4S1/QF5210/Group Project Data')
library("tseries")
library(forecast)
library(fGarch)

sg <- read.csv('ftsesg.csv')
us <- read.csv('sp500.csv')
uk <- read.csv('ftse100.csv')

index.sg = sg[2:143,1:2]
index.sg[,2] = diff(log(sg[,2])) 
index.uk = uk[2:143,1:2]
index.uk[,2] = diff(log(uk[,2])) 
index.us = us[2:143,1:2]
index.us[,2] = diff(log(us[,2])) 

colnames(index.sg)[2] <- "Return"
colnames(index.uk)[2] <- "Return"
colnames(index.us)[2] <- "Return"

index.sg[,1] = as.Date(index.sg[,1], format = "%m/%d/%Y")
index.uk[,1] = as.Date(index.uk[,1], format = "%m/%d/%Y")
index.us[,1] = as.Date(index.us[,1], format = "%m/%d/%Y")

par(mfrow=c(3,1))
plot(index.sg, type='l', main='FTSE SG return')
plot(index.uk, type='l', main='FTSE100 return')
plot(index.us, type='l', main='SP500 return')

#sg
par(mfrow=c(1,2))
acf(index.sg$Return)
pacf(index.sg$Return)
#auto.arima(sg$Price, ic = "aic")
auto.arima(index.sg$Return, ic = "aic")
auto.arima(index.sg$Return, ic = "bic")
sgI = arima(index.sg$Return, order = c(0,0,0)) 
tsdiag(sgI)
par(mfrow=c(1,1))
acf(sgI$residuals ^2)

sgI.garch = garchFit(~garch(1,1),data=index.sg$Return,cond.dist="norm")
summary(sgI.garch)
sgI.garch.t = garchFit(~garch(1,1),data=index.sg$Return,cond.dist="std")
summary(sgI.garch.t)
plot(sgI.garch.t@residuals)

forecast.index.sg = predict(sgI.garch.t, n.ahead = 8)
forecast.index.sg$meanForecast
forecast.index.sg$standardDeviation

#uk
par(mfrow=c(1,2))
acf(index.uk$Return)
pacf(index.uk$Return)

par(mfrow=c(1,1))
acf(index.uk$Return ^2)

ukI.garch = garchFit(~garch(1,1),data=index.uk$Return,cond.dist="norm")
summary(ukI.garch)
ukI.garch.t = garchFit(~garch(1,1),data=index.uk$Return,cond.dist="std")
summary(ukI.garch.t)
ukI2.garch.t = garchFit(~garch(2,2),data=index.uk$Return,cond.dist="std")
summary(ukI2.garch.t)

plot(ukI2.garch.t@residuals)

forecast.index.uk = predict(ukI2.garch.t, n.ahead = 8)
forecast.index.uk$meanForecast
forecast.index.uk$standardDeviation

#us
par(mfrow=c(1,2))
acf(index.us$Return)
pacf(index.us$Return)

par(mfrow=c(1,1))
acf(index.us$Return ^2)

usI.garch.t = garchFit(~garch(1,1),data=index.us$Return,cond.dist="std")
summary(usI.garch.t)

plot(usI.garch.t@residuals)

forecast.index.us = predict(usI.garch.t, n.ahead = 8)
forecast.index.us$meanForecast
forecast.index.us$standardDeviation

#VAR
library("MTS")
returns = matrix(0,142,3)
returns[,1] = index.sg$Return
returns[,2] = index.uk$Return
returns[,3] = index.us$Return
ccm(returns)
r0=VARorder(returns, maxp = 20)
r1 = VAR(returns,2)
r2 = refVAR(r1, thres=1.96)

MTSdiag(r2,adj=12)
MTSdiag(r1,adj=12)

VARpred(r2,8)
colMeans(returns) 
sqrt(apply(returns,2,var))
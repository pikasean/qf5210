setwd('C:/Users/bingc/Files/NUS/Y4S1/QF5210/Group Project Data')
library("tseries")
library(forecast)
library(fGarch)
library("fUnitRoots")
library("urca")
library("MTS")

sg <- read.csv('ftsesg.csv')
us <- read.csv('sp500.csv')
uk <- read.csv('ftse100.csv')

index.sg = sg[,1:4]
index.sg[,3] = log(index.sg[,2])
index.sg[2:148,4] = diff(log(index.sg[,2]))
index.sg[1,4] = NA
index.uk = uk[,1:4]
index.uk[,3] = log(index.uk[,2])
index.uk[2:148,4] = diff(log(index.uk[,2]))
index.uk[1,4] = NA
index.us = us[,1:4]
index.us[,3] = log(index.us[,2])
index.us[2:148,4] = diff(log(index.us[,2]))
index.us[1,4] = NA
index.uk[,4] = as.numeric(index.uk[,4])
index.us[,4] = as.numeric(index.us[,4])

colnames(index.sg)[3] <- "LogPrice"
colnames(index.uk)[3] <- "LogPrice"
colnames(index.us)[3] <- "LogPrice"
colnames(index.sg)[4] <- "Return"
colnames(index.uk)[4] <- "Return"
colnames(index.us)[4] <- "Return"
index.sg[,1] = as.Date(index.sg[,1], format = "%m/%d/%Y")
index.uk[,1] = as.Date(index.uk[,1], format = "%m/%d/%Y")
index.us[,1] = as.Date(index.us[,1], format = "%m/%d/%Y")

#plot log price series of the 3 indexes
par(mfrow=c(3,1))
plot(x=index.sg[,1], y=index.sg[,3], type='l', main='FTSE SG log price')
plot(x=index.uk[,1], y=index.uk[,3], type='l', main='FTSE100 log price')
plot(x=index.us[,1], y=index.us[,3], type='l', main='SP500 log price')

#plot log return series of the 3 indexes
plot(x=index.sg[,1], y=index.sg[,4], type='l', main='FTSE SG return')
plot(x=index.uk[,1], y=index.uk[,4], type='l', main='FTSE100 return')
plot(x=index.us[,1], y=index.us[,4], type='l', main='SP500 return')

#test for unit-root nonstationary of log price series
adfTest(index.sg[,3],lags=5,type="c")
adfTest(index.uk[,3],lags=5,type="c")
adfTest(index.us[,3],lags=5,type="c")

#test for stationary of log return series
adfTest(index.sg[,4],lags=5,type="c")
adfTest(index.uk[,4],lags=5,type="c")
adfTest(index.us[,4],lags=5,type="c")

#separate datasets into training, validation, and test
index.train.sg = index.sg[1:126,]
index.valid.sg = index.sg[127:139,]
index.test.sg = index.sg[140:148,]
index.train.uk = index.uk[1:126,]
index.valid.uk = index.uk[127:139,]
index.test.uk = index.uk[140:148,]
index.train.us = index.us[1:126,]
index.valid.us = index.us[127:139,]
index.test.us = index.us[140:148,]

#choose the order of VAR model to be used in cointegration test to fit VEC model for log price series
log.price = matrix(0,126,3)
log.price[,1] = index.train.sg$LogPrice
log.price[,2] = index.train.uk$LogPrice
log.price[,3] = index.train.us$LogPrice
ccm(log.price)
m0=VARorder(log.price)

#cointegration test for log price series
m1=ca.jo(as.data.frame(log.price),K=6,ecdet=c("none"))
summary(m1)
mean(as.numeric(index.train.sg$Return), na.rm = TRUE)
mean(as.numeric(index.train.uk$Return), na.rm = TRUE)
mean(as.numeric(index.train.us$Return), na.rm = TRUE)

#testing to check that the cointegrated series is stationary
log.price = as.data.frame(log.price)
wt=log.price[,1]-1.971793*log.price[,2]+0.420634*log.price[,3]
plot(wt, type='l')
adfTest(wt,lags=5,type="c")

#fitting VECM and diagnostic checking
m1.ecmvar=ECMvar1(log.price,7,wt,include.const=FALSE)
m1.refecmvar = refECMvar1(m1.ecmvar)
MTSdiag(m1.ecmvar,adj=12)

m2=ca.jo(log.price,K=5,ecdet=c("none"))
summary(m2)
wt2=log.price[,1]-1.6767579*log.price[,2]+0.2899923*log.price[,3]
m2.ecmvar=ECMvar1(log.price,6,wt2,include.const=FALSE)
m2.refecmvar = refECMvar1(m2.ecmvar)
MTSdiag(m2.ecmvar,adj=12)

m3=ca.jo(log.price,K=2,ecdet=c("none"))
summary(m3)

#fitting VAR and diagnostic checking
returns = matrix(0,125,3)
returns[,1] = index.train.sg$Return[2:126]
returns[,2] = index.train.uk$Return[2:126]
returns[,3] = index.train.us$Return[2:126]
ccm(returns)
var0=VARorder(returns, maxp=10)
m4 = VAR(returns,6)
m4.ref = refVAR(m4, thres=1.96)
MTSdiag(m4,adj=12)

m5 = VAR(returns,5)
m5.ref = refVAR(m5, thres=1.96)
MTSdiag(m5,adj=12)

m6 = VAR(returns,2)
m6.ref = refVAR(m6, thres=1.96)
MTSdiag(m6,adj=12)

#validation
actual.valid.price = matrix(0,13,3)
actual.valid.price[,1] = index.valid.sg$Price
actual.valid.price[,2] = index.valid.uk$Price
actual.valid.price[,3] = index.valid.us$Price

#m4
forecast.return.m4 = VARpred(m4.ref,13)$pred
temp.log.price.m4 = as.matrix(log.price[126,])
for(i in 1:13){
temp.log.price.m4 = rbind(temp.log.price.m4, temp.log.price.m4[i,]+forecast.return.m4[i,])
}
forecast.price.m4 = exp(temp.log.price.m4[2:14,])
accuracy(as.vector(forecast.price.m4),as.vector(actual.valid.price))

#m5
forecast.return.m5 = VARpred(m5.ref,13)$pred
temp.log.price.m5 = as.matrix(log.price[126,])
for(i in 1:13){
temp.log.price.m5 = rbind(temp.log.price.m5, temp.log.price.m5[i,]+forecast.return.m5[i,])
}
forecast.price.m5 = exp(temp.log.price.m5[2:14,])
accuracy(as.vector(forecast.price.m5),as.vector(actual.valid.price))

#m6
forecast.return.m6 = VARpred(m6.ref,13)$pred
temp.log.price.m6 = as.matrix(log.price[126,])
for(i in 1:13){
temp.log.price.m6 = rbind(temp.log.price.m6, temp.log.price.m6[i,]+forecast.return.m6[i,])
}
forecast.price.m6 = exp(temp.log.price.m6[2:14,])
accuracy(as.vector(forecast.price.m6),as.vector(actual.valid.price))

#find MAPE of validation period for each country separately
accuracy(as.vector(forecast.price.m5[,1]),as.vector(actual.valid.price[,1])) #sg
accuracy(as.vector(forecast.price.m5[,2]),as.vector(actual.valid.price[,2])) #uk
accuracy(as.vector(forecast.price.m5[,3]),as.vector(actual.valid.price[,3])) #us

#find MAPE of training period
actual.train.price = matrix(0,104,3)
actual.train.price[,1] = index.train.sg$Price[23:126]
actual.train.price[,2] = index.train.uk$Price[23:126]
actual.train.price[,3] = index.train.us$Price[23:126]

forecast.return.m5.train=matrix(0,104,3)
#Note: The package is unable to provide predictions for i in 5:20, as it yields computationally singular systems, perhaps due to having too few data points.
for(i in 21:124){
forecast.return.m5.train[i-20,] = VARpred(m5.ref,h=1,orig=i)$pred
}
temp.log.price.m5.train = matrix(c(index.train.sg$LogPrice[22],index.train.uk$LogPrice[22],index.train.us$LogPrice[22]),1,3)
for(i in 1:104){
temp.log.price.m5.train = rbind(temp.log.price.m5.train, temp.log.price.m5.train[i,]+forecast.return.m5.train[i,])
}
forecast.price.m5.train = exp(temp.log.price.m5.train[2:105,])
accuracy(as.vector(forecast.price.m5.train),as.vector(actual.train.price))

#find MAPE of training period for each country separately
accuracy(as.vector(forecast.price.m5.train[,1]),as.vector(actual.train.price[,1])) #sg
accuracy(as.vector(forecast.price.m5.train[,2]),as.vector(actual.train.price[,2])) #uk
accuracy(as.vector(forecast.price.m5.train[,3]),as.vector(actual.train.price[,3])) #us


#test period: retrain model and forecast
returns.extend = matrix(0,125+13,3)
returns.extend[,1] = c(index.train.sg$Return[2:126],index.valid.sg$Return)
returns.extend[,2] = c(index.train.uk$Return[2:126],index.valid.uk$Return)
returns.extend[,3] = c(index.train.us$Return[2:126],index.valid.us$Return)

m5.extend = VAR(returns.extend,5)
m5.extend.ref = refVAR(m5.extend, thres=1.96)

actual.test.price = matrix(0,9,3)
actual.test.price[,1] = index.test.sg$Price
actual.test.price[,2] = index.test.uk$Price
actual.test.price[,3] = index.test.us$Price

forecast.return.m5.extend = VARpred(m5.extend.ref,9)$pred
temp.log.price.m5.extend = matrix(c(index.valid.sg$LogPrice[13],index.valid.uk$LogPrice[13],index.valid.us$LogPrice[13]),1,3)
for(i in 1:9){
temp.log.price.m5.extend = rbind(temp.log.price.m5.extend, temp.log.price.m5.extend[i,]+forecast.return.m5.extend[i,])
}
forecast.price.m5.extend = exp(temp.log.price.m5.extend[2:10,])
accuracy(as.vector(forecast.price.m5.extend),as.vector(actual.test.price))

#find MAPE of test period for each country separately
accuracy(as.vector(forecast.price.m5.extend[,1]),as.vector(actual.test.price[,1])) #sg
accuracy(as.vector(forecast.price.m5.extend[,2]),as.vector(actual.test.price[,2])) #uk
accuracy(as.vector(forecast.price.m5.extend[,3]),as.vector(actual.test.price[,3])) #us


#Graphs
par(mfrow=c(3,1))
se=VARpred(m5.extend.ref,9)$se.err
return.lower.bound = forecast.return.m5.extend - 1.96*se
return.upper.bound = forecast.return.m5.extend + 1.96*se

plot(x=index.sg[,1],y=index.sg[,4], type='l', main='FTSE SG Returns', xlab='Time', ylab='Return')
lines(x=index.sg[140:148,1],y=forecast.return.m5.extend[,1], col="blue")
lines(x=index.sg[140:148,1],y=return.lower.bound[,1], col="red")
lines(x=index.sg[140:148,1],y=return.upper.bound[,1], col="red")

plot(x=index.uk[,1],y=index.uk[,4], type='l', main='FTSE 100 Returns', xlab='Time', ylab='Return')
lines(x=index.uk[140:148,1],y=forecast.return.m5.extend[,2], col="blue")
lines(x=index.uk[140:148,1],y=return.lower.bound[,2], col="red")
lines(x=index.uk[140:148,1],y=return.upper.bound[,2], col="red")

plot(x=index.us[,1],y=index.us[,4], type='l', main='SP 500 Returns', xlab='Time', ylab='Return')
lines(x=index.us[140:148,1],y=forecast.return.m5.extend[,3], col="blue")
lines(x=index.us[140:148,1],y=return.lower.bound[,3], col="red")
lines(x=index.us[140:148,1],y=return.upper.bound[,3], col="red")

plot(x=index.sg[,1],y=index.sg[,2], type='l', main='FTSE SG Price', xlab='Time', ylab='Price')
lines(x=index.sg[140:148,1],y=forecast.price.m5.extend[,1], col="blue")

plot(x=index.uk[,1],y=index.uk[,2], type='l', main='FTSE 100 Price', xlab='Time', ylab='Price')
lines(x=index.uk[140:148,1],y=forecast.price.m5.extend[,2], col="blue")

plot(x=index.us[,1],y=index.us[,2], type='l', main='SP 500 Price', xlab='Time', ylab='Price')
lines(x=index.us[140:148,1],y=forecast.price.m5.extend[,3], col="blue")




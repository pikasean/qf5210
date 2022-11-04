library("tseries")
library(forecast)
library(fGarch)
library(rugarch)
library(fUnitRoots)

# For the differences
training_period = 1:116 
retraining_period = 1:128
validation_period = 116:128
test_period = 128:136

# For the original series
training_period_ori = 2:116
training_period_prev = 1:115
validation_period_ori = 117:128
validation_period_prev = 116:127
test_period_ori = 129:136
test_period_prev = 128:135

covid <- read.csv('covid.csv')
plot(ts(covid[,2:4]), main = "Weekly COVID Cases by Country")

# Prepare training data
par(mfrow=c(1,3))
change.covid.sg = ts(diff(covid$Singapore[training_period]))
change.covid.sg.retraining = ts(diff(covid$Singapore[retraining_period]))
plot(change.covid.sg)
change.covid.uk = ts(diff(covid$United.Kingdom[training_period]))
change.covid.uk.retraining = ts(diff(covid$United.Kingdom[retraining_period]))
plot(change.covid.uk)
change.covid.us = ts(diff(covid$United.States[training_period]))
change.covid.us.retraining = ts(diff(covid$United.States[retraining_period]))
plot(change.covid.us)

adf.test(covid$Singapore)
adf.test(covid$United.Kingdom)
adf.test(covid$United.States)
adf.test(covid$Singapore)
adf.test(covid$United.Kingdom)
adf.test(covid$United.States)
adf.test(change.covid.sg)
adf.test(change.covid.uk)
adf.test(change.covid.us)


# SG - Try ARIMA
par(mfrow=c(1,2))
acf(change.covid.sg, lag.max=50)
pacf(change.covid.sg, lag.max=50)
auto.arima(change.covid.sg, ic = "aic")
auto.arima(change.covid.sg, ic = "bic")
sg.arima.A = arima(change.covid.sg, order = c(4,0,1))
tsdiag(sg.arima.A)
par(mfrow=c(1,1))
acf(sg.arima.A$residuals ^2)

# SG - Try GARCH
forecast.length = 12
specA <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(3, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
sg.garch.A <- ugarchfit(spec = specA, data = change.covid.sg, solver ='hybrid')
sg.fc.A = ugarchforecast(sg.garch.A, n.ahead=forecast.length)

specB <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(4, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
sg.garch.B <- ugarchfit(spec = specB, data = change.covid.sg, solver ='hybrid')
sg.fc.B = ugarchforecast(sg.garch.B, n.ahead=forecast.length)

specC <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(5, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
sg.garch.C <- ugarchfit(spec = specC, data = change.covid.sg, solver ='hybrid')
sg.fc.C = ugarchforecast(sg.garch.C, n.ahead=forecast.length)

specD <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(3, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
sg.garch.D <- ugarchfit(spec = specD, data = change.covid.sg, solver ='hybrid')
sg.fc.D = ugarchforecast(sg.garch.D, n.ahead=forecast.length)

specE <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(4, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
sg.garch.E <- ugarchfit(spec = specE, data = change.covid.sg, solver ='hybrid')
sg.fc.E = ugarchforecast(sg.garch.E, n.ahead=forecast.length)

specF <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(5, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
sg.garch.F <- ugarchfit(spec = specF, data = change.covid.sg, solver ='hybrid')
sg.fc.F = ugarchforecast(sg.garch.F, n.ahead=forecast.length)


specG <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(4, 2), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
sg.garch.G <- ugarchfit(spec = specG, data = change.covid.sg, solver ='hybrid')
sg.fc.G = ugarchforecast(sg.garch.G, n.ahead=forecast.length)

valid.covid.sg = covid$Singapore[validation_period_ori]
valid.covid.sg.m1 = covid$Singapore[validation_period_prev]
accuracy(as.vector(fitted(sg.fc.A)) + valid.covid.sg.m1, valid.covid.sg)
accuracy(as.vector(fitted(sg.fc.B)) + valid.covid.sg.m1, valid.covid.sg)
accuracy(as.vector(fitted(sg.fc.C)) + valid.covid.sg.m1, valid.covid.sg)
accuracy(as.vector(fitted(sg.fc.D)) + valid.covid.sg.m1, valid.covid.sg)
accuracy(as.vector(fitted(sg.fc.E)) + valid.covid.sg.m1, valid.covid.sg)
accuracy(as.vector(fitted(sg.fc.F)) + valid.covid.sg.m1, valid.covid.sg)
accuracy(as.vector(fitted(sg.fc.G)) + valid.covid.sg.m1, valid.covid.sg)

# SG - Analyse best model

# E is the best model so
`%nin%` = Negate(`%in%`)
plot(sg.fc.E,which="all")
plot(as.vector(residuals(sg.garch.E)), main = "Residuals for SG", ylab = "Residuals")

train.covid.sg = covid$Singapore[training_period_ori]
train.covid.sg.m1 = covid$Singapore[training_period_prev]
accuracy((train.covid.sg.m1+(change.covid.sg-as.vector(residuals(sg.garch.E))))[train.covid.sg %nin% c(0)], train.covid.sg[train.covid.sg %nin% c(0)])

sg.garch.final <- ugarchfit(spec = specE, data = change.covid.sg.retraining, solver ='hybrid')
test.covid.sg = covid$Singapore[test_period_ori]
test.covid.sg.m1 = covid$Singapore[test_period_prev]
sg.fc.final = ugarchforecast(sg.garch.final, n.ahead=8)
accuracy(as.vector(fitted(sg.fc.final)) + test.covid.sg.m1, test.covid.sg)

# UK - Try ARIMA
par(mfrow=c(1,2))
acf(change.covid.uk, lag.max=50)
pacf(change.covid.uk, lag.max=50)
auto.arima(change.covid.uk, ic = "aic")
auto.arima(change.covid.uk, ic = "bic")
uk.arima.A = arima(change.covid.uk, order = c(2,0,1))
tsdiag(uk.arima.A)
acf(uk.arima.A$residuals ^2)

# UK - Try GARCH
forecast.length = 12
specA <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(1, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
uk.garch.A <- ugarchfit(spec = specA, data = change.covid.uk, solver ='hybrid')
uk.fc.A = ugarchforecast(uk.garch.A, n.ahead=forecast.length)

specB <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(2, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
uk.garch.B <- ugarchfit(spec = specB, data = change.covid.uk, solver ='hybrid')
uk.fc.B = ugarchforecast(uk.garch.B, n.ahead=forecast.length)

specC <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(3, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
uk.garch.C <- ugarchfit(spec = specC, data = change.covid.uk, solver ='hybrid')
uk.fc.C = ugarchforecast(uk.garch.C, n.ahead=forecast.length)

specD <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(1, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
uk.garch.D <- ugarchfit(spec = specD, data = change.covid.uk, solver ='hybrid')
uk.fc.D = ugarchforecast(uk.garch.D, n.ahead=forecast.length)

specE <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(2, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
uk.garch.E <- ugarchfit(spec = specE, data = change.covid.uk, solver ='hybrid')
uk.fc.E = ugarchforecast(uk.garch.E, n.ahead=forecast.length)

specF <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(3, 1), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
uk.garch.F <- ugarchfit(spec = specF, data = change.covid.uk, solver ='hybrid')
uk.fc.F = ugarchforecast(uk.garch.F, n.ahead=forecast.length)


specG <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(2, 2), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
uk.garch.G <- ugarchfit(spec = specG, data = change.covid.uk, solver ='hybrid')
uk.fc.G = ugarchforecast(uk.garch.G, n.ahead=forecast.length)

valid.covid.uk = covid$United.Kingdom[validation_period_ori]
valid.covid.uk.m1 = covid$United.Kingdom[validation_period_prev]
accuracy(as.vector(fitted(uk.fc.A)) + valid.covid.uk.m1, valid.covid.uk)
accuracy(as.vector(fitted(uk.fc.B)) + valid.covid.uk.m1, valid.covid.uk)
accuracy(as.vector(fitted(uk.fc.C)) + valid.covid.uk.m1, valid.covid.uk)
accuracy(as.vector(fitted(uk.fc.D)) + valid.covid.uk.m1, valid.covid.uk)
accuracy(as.vector(fitted(uk.fc.E)) + valid.covid.uk.m1, valid.covid.uk)
accuracy(as.vector(fitted(uk.fc.F)) + valid.covid.uk.m1, valid.covid.uk)
accuracy(as.vector(fitted(uk.fc.G)) + valid.covid.uk.m1, valid.covid.uk)

# UK - Analyse best model

# A is the best model so
`%nin%` = Negate(`%in%`)
plot(uk.fc.A,which="all")
plot(as.vector(residuals(uk.garch.A)), main = "Residuals for UK", ylab = "Residuals")

train.covid.uk = covid$United.Kingdom[training_period_ori]
train.covid.uk.m1 = covid$United.Kingdom[training_period_prev]
accuracy((train.covid.uk.m1+(change.covid.uk-as.vector(residuals(uk.garch.A))))[train.covid.uk %nin% c(0)], train.covid.uk[train.covid.uk %nin% c(0)])

uk.garch.final <- ugarchfit(spec = specA, data = change.covid.uk.retraining, solver ='hybrid')
test.covid.uk = covid$United.Kingdom[test_period_ori]
test.covid.uk.m1 = covid$United.Kingdom[test_period_prev]
uk.fc.final = ugarchforecast(uk.garch.final, n.ahead=8)
accuracy(as.vector(fitted(uk.fc.final)) + test.covid.uk.m1, test.covid.uk)

# US - Try ARIMA
par(mfrow=c(1,2))
acf(change.covid.us, lag.max=50)
pacf(change.covid.us, lag.max=50)
auto.arima(change.covid.us, ic = "aic")
auto.arima(change.covid.us, ic = "bic")
us.arima.A = arima(change.covid.us, order = c(2,0,3))
tsdiag(us.arima.A)
acf(us.arima.A$residuals ^2)

# us - Try GARCH
forecast.length = 12
specA <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(1, 3), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
us.garch.A <- ugarchfit(spec = specA, data = change.covid.us, solver ='hybrid')
us.fc.A = ugarchforecast(us.garch.A, n.ahead=forecast.length)

specB <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(2, 3), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
us.garch.B <- ugarchfit(spec = specB, data = change.covid.us, solver ='hybrid')
us.fc.B = ugarchforecast(us.garch.B, n.ahead=forecast.length)

specC <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(3, 3), 
                                          external.regressors = NULL),
                    
                    distribution.model = "norm",
                    start.pars = list(), 
                    fixed.pars = list())
us.garch.C <- ugarchfit(spec = specC, data = change.covid.us, solver ='hybrid')
us.fc.C = ugarchforecast(us.garch.C, n.ahead=forecast.length)

specD <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(1, 3), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
us.garch.D <- ugarchfit(spec = specD, data = change.covid.us, solver ='hybrid')
us.fc.D = ugarchforecast(us.garch.D, n.ahead=forecast.length)

specE <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(2, 3), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
us.garch.E <- ugarchfit(spec = specE, data = change.covid.us, solver ='hybrid')
us.fc.E = ugarchforecast(us.garch.E, n.ahead=forecast.length)

specF <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(3, 3), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
us.garch.F <- ugarchfit(spec = specF, data = change.covid.us, solver ='hybrid')
us.fc.F = ugarchforecast(us.garch.F, n.ahead=forecast.length)


specG <- ugarchspec(variance.model = list(model = "fGARCH", 
                                          garchOrder = c(1, 1),
                                          submodel = "GARCH",
                                          external.regressors = NULL, 
                                          variance.targeting = FALSE), 
                    
                    mean.model     = list(armaOrder = c(2, 2), 
                                          external.regressors = NULL),
                    
                    distribution.model = "std",
                    start.pars = list(), 
                    fixed.pars = list())
us.garch.G <- ugarchfit(spec = specG, data = change.covid.us, solver ='hybrid')
us.fc.G = ugarchforecast(us.garch.G, n.ahead=forecast.length)

valid.covid.us = covid$United.States[validation_period_ori]
valid.covid.us.m1 = covid$United.States[validation_period_prev]
accuracy(as.vector(fitted(us.fc.A)) + valid.covid.us.m1, valid.covid.us)
accuracy(as.vector(fitted(us.fc.B)) + valid.covid.us.m1, valid.covid.us)
accuracy(as.vector(fitted(us.fc.C)) + valid.covid.us.m1, valid.covid.us)
accuracy(as.vector(fitted(us.fc.D)) + valid.covid.us.m1, valid.covid.us)
accuracy(as.vector(fitted(us.fc.E)) + valid.covid.us.m1, valid.covid.us)
accuracy(as.vector(fitted(us.fc.F)) + valid.covid.us.m1, valid.covid.us)
accuracy(as.vector(fitted(us.fc.G)) + valid.covid.us.m1, valid.covid.us)

# US - Analyse best model

# E is the best model so
`%nin%` = Negate(`%in%`)
plot(us.fc.E,which="all")
plot(as.vector(residuals(us.garch.E)), main = "Residuals for US", ylab = "Residuals")

train.covid.us = covid$United.States[training_period_ori]
train.covid.us.m1 = covid$United.States[training_period_prev]
accuracy((train.covid.us.m1+(change.covid.us-as.vector(residuals(us.garch.E))))[train.covid.us %nin% c(0)], train.covid.us[train.covid.us %nin% c(0)])

us.garch.final <- ugarchfit(spec = specE, data = change.covid.us.retraining, solver ='hybrid')
test.covid.us = covid$United.States[test_period_ori]
test.covid.us.m1 = covid$United.States[test_period_prev]
us.fc.final = ugarchforecast(us.garch.final, n.ahead=8)
accuracy(as.vector(fitted(us.fc.final)) + test.covid.us.m1, test.covid.us)
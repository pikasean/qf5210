library("tseries")
library(forecast)
library(fGarch)
library(rugarch)

training_period = 1:116
validation_period = 116:128 # Because we are going to take the diff

covid <- read.csv('covid.csv')
plot(ts(covid[,2:4]), main = "Weekly COVID Cases by Country")

# Prepare training data
par(mfrow=c(1,3))
change.covid.sg = ts(diff(covid$Singapore[training_period]))
plot(change.covid.sg)
change.covid.uk = ts(diff(covid$United.Kingdom[training_period]))
plot(change.covid.uk)
change.covid.us = ts(diff(covid$United.States[training_period]))
plot(change.covid.us)

# SG - Try ARIMA
par(mfrow=c(1,2))
acf(change.covid.sg, lag.max=50)
pacf(change.covid.sg, lag.max=50)
auto.arima(change.covid.sg, ic = "aic")
auto.arima(change.covid.sg, ic = "bic")
sg.arima.A = arima(change.covid.sg, order = c(4,0,1))
tsdiag(sg.arima.A)
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

valid.change.covid.sg = ts(diff(covid$Singapore[validation_period]))
accuracy(as.vector(fitted(sg.fc.A)), valid.change.covid.sg)
accuracy(as.vector(fitted(sg.fc.B)), valid.change.covid.sg)
accuracy(as.vector(fitted(sg.fc.C)), valid.change.covid.sg)
accuracy(as.vector(fitted(sg.fc.D)), valid.change.covid.sg)
accuracy(as.vector(fitted(sg.fc.E)), valid.change.covid.sg)
accuracy(as.vector(fitted(sg.fc.F)), valid.change.covid.sg)
accuracy(as.vector(fitted(sg.fc.G)), valid.change.covid.sg)

# SG - Analyse best model

# E is the best model so
`%nin%` = Negate(`%in%`)
plot(sg.fc.E,which="all")
plot(residuals(sg.garch.E))
accuracy((change.covid.sg-as.vector(residuals(sg.garch.E)))[change.covid.sg %nin% c(0)], change.covid.sg[change.covid.sg %nin% c(0)])
# MAPE doesn't really make sense especially when true value = 0 or very small

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

valid.change.covid.uk = ts(diff(covid$United.Kingdom[validation_period]))
accuracy(as.vector(fitted(uk.fc.A)), valid.change.covid.uk)
accuracy(as.vector(fitted(uk.fc.B)), valid.change.covid.uk)
accuracy(as.vector(fitted(uk.fc.C)), valid.change.covid.uk)
accuracy(as.vector(fitted(uk.fc.D)), valid.change.covid.uk)
accuracy(as.vector(fitted(uk.fc.E)), valid.change.covid.uk)
accuracy(as.vector(fitted(uk.fc.F)), valid.change.covid.uk)
accuracy(as.vector(fitted(uk.fc.G)), valid.change.covid.uk)

# UK - Analyse best model

# E is the best model so
`%nin%` = Negate(`%in%`)
plot(uk.fc.E,which="all")
plot(residuals(uk.garch.E))
accuracy((change.covid.uk-as.vector(residuals(uk.garch.E)))[change.covid.uk %nin% c(0)], change.covid.uk[change.covid.uk %nin% c(0)])
# MAPE doesn't really make sense especially when true value = 0 or very small

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

valid.change.covid.us = ts(diff(covid$United.States[validation_period]))
accuracy(as.vector(fitted(us.fc.A)), valid.change.covid.us)
accuracy(as.vector(fitted(us.fc.B)), valid.change.covid.us)
accuracy(as.vector(fitted(us.fc.C)), valid.change.covid.us)
accuracy(as.vector(fitted(us.fc.D)), valid.change.covid.us)
accuracy(as.vector(fitted(us.fc.E)), valid.change.covid.us)
accuracy(as.vector(fitted(us.fc.F)), valid.change.covid.us)
accuracy(as.vector(fitted(us.fc.G)), valid.change.covid.us)

# US - Analyse best model

# E is the best model so
`%nin%` = Negate(`%in%`)
plot(us.fc.E,which="all")
plot(residuals(us.garch.E))
accuracy((change.covid.us-as.vector(residuals(us.garch.E)))[change.covid.us %nin% c(0)], change.covid.us[change.covid.us %nin% c(0)])
# MAPE doesn't really make sense especially when true value = 0 or very small
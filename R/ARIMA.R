# http://robjhyndman.com/hyndsight/piecewise-linear-trends/
# Good technical overview
require(fpp)
T <- length(livestock)
x1 <- seq(T)
fit <- auto.arima(livestock, xreg = x1)
fc <- forecast(fit, xreg = T + seq(10))
b0 <- coef(fit)["intercept"]
b1 <- coef(fit)["x1"]
t <- seq(T + 10)
trend <- ts(b0 + b1 * t, start = start(livestock))
plot(fc, main = "Linear trend with AR(1) errors")
lines(trend, col = 'red')
?livestock
#-------------------------------------
x2 <- pmax(0, x1 - 30)
x3 <- pmax(0, x1 - 32)
fit <- auto.arima(livestock, xreg = cbind(x1, x2, x3))
fc <- forecast(fit, xreg = cbind(max(x1) + seq(10), max(x2) + seq(10), 
                                 max(x3) + seq(10)))
b0 <- coef(fit)["intercept"]
b1 <- coef(fit)["x1"]
b2 <- coef(fit)["x2"]
b3 <- coef(fit)["x3"]
trend <- ts(b0 + b1 * t + b2 * pmax(0, t - 30) + b3 * pmax(0, t - 32),
            start = start(livestock))
plot(fc, main = "Piecwise lnear trend with AR(1) errors")
lines(trend, col = "red")
#-----------------------------------            
# https://www.otexts.org/fpp/8/7
# ARIMA
library("forecast")
library("fpp")
fit <- Arima(usconsumption[,1], order = c(0, 0, 3))
eeadj <- seasadj(stl(elecequip, s.window = "periodic"))
plot(eeadj)
tsdisplay(diff(eeadj), main = "")
fit <- Arima(eeadj, order = c(3, 1, 1))
summary(fit)
Acf(residuals(fit))
Box.test(residuals(fit), lag = 24, fitdf = 4, type = "Ljung")
plot(forecast(fit))

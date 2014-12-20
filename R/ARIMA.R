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

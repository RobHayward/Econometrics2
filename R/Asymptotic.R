# From http://davegiles.blogspot.com/2014/10/
# illustrating-asymptotic-behaviour-part-i.html
# reps <- 50
# n = 20
library(PerformanceAnalytics)
# this function will create the time series and lagged time series
Asymp <- function(n, reps){
y <- rep(NA, n)
y[1] <- 0
temp <- rep(NA, reps)
for(j in 1:reps){
for(i in 2:n){
  e <- runif(n, -1, 1)
  y[i] <- 1 + 0.5*y[i-1] + e[i]
  
}
ylag <- lag(y)
da <- data.frame(y[2:n], y[1:n-1])
colnames(da) <- c("y", "ylag")
eq <- lm(y ~ ylag, data = da)
temp[j] <- eq$coefficients[2]
}
m <- mean(temp)
s <- skewness(temp)
k <- kurtosis(temp)
title <- paste("Estimate of beta_1 with sample of ",n, sep = "")
#legend <- paste("Mean is",m, "Skew is", s, ,"Kurtosis is", k, sep = " " )
hist(temp, main = title)
}
Asymp(20, 500)
skewness(y)
kurtosis(y)

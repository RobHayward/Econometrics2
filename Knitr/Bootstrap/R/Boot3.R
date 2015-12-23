require(tseries)
#http://eranraviv.com/bootstrapping-time-series-r-code/
y <- read.csv("Data/EUmis.csv", stringsAsFactors = FALSE)
head(y)
# Restrict to where there is unemployment and inflation data.
y <- y[1:215,]
y$misery <- 0.75 * y[ ,2] + 0.25 * y[ ,3]
tail(y)
y = as.ts(y[, 4], frequency = 12, start = c(1998, 1))
par(mfrow = c(2, 1))
plot(y, main = "Misery Index Over Time", ylab = "Index", lwd = 2, ty = 'b', 
     cex.main = 2)
adf.test(y)
kpss.test(y)
numlags = 12
n = length(y)
ar1 <- ar.ols(y, demean = F, intercept = T, order.max = numlags, aic = F)
plot(ar1$res)
scaled.res = scale(ar1$res)
summ(scaled.res)
# need to look up this summ function.   
b1 <- y[1:numlags]
R = 100
res.star = matrix(nrow = (n - numlags), ncol = R)
obs.star = matrix(nrow = n, ncol = R)

for(i in 1:R){
  res.star[,i] = sample(na.omit(scaled.res), (n - numlags), replace = T)
  obs.star[1:numlags, i] = b1
  for(j in (numlags + 1):n){
    obs.star[j, i] = ar1$x.intercept + ar1$ar%*%obs.star[(j-1):(j-numlags), i] +
      res.star[(j - numlags), i]
  }
}
plot(obs.star[, sample(1:n, 1)], ty = 'l', main = "Pick Random Realisation")
plot(y, ty = 'l', main = "Original")
#-----------------------------------------------
# alternatively, reduce the standard deviation of simulation by sd(y)/2
plot(arima.sim(list(order = c(numlags, 0, 0), 
               ar = ar.ols(y, demand = T, intercept = F, order.max = numlags, 
                           aic = F)$ar), n = n, sd = sd(y)/2))
#------------------------------------------------
# block boostrap
# check the autocorrelation
acf(as.numeric(y))
gprob = 1/20
R = 100
ystar = matrix(nrow = n, ncol = R)
for(r in 1:R){
  loc = round(runif(1, 1, n))
  for(i in 1:n){
    g1 = runif(1, 0, 1)
    #80% of the time there will be a run, otherwise, choose another random number
    if(g1 > gprob){loc = loc + 1} else {loc = round(runif(1, 1, n))}
    if(loc > n) loc = loc - n
    ystar[i, r] = y[loc]
    
  }
}
plot(ystar[, sample(1:R, 1)], ty = 'l', main = "Pick random realisation")
plot(y, ty = 'l', main = "Original Series")


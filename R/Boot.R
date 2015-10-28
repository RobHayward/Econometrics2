# from http://www.mayin.org/ajayshah/KB/R/documents/boot.html
x <- c(10, 20, 30, 40, 50)
d <- c(3, 2, 2)
samplemean <- function(x, d){
  return(mean(x[d]))
}

samplemedian <- function(x, d){
  return(mean(x[d]))
}
b = boot(x, samplemedian, R = 1000)
print(sd(b$t[, 1]))
plot(b)
# from https://cran.r-project.org/doc/Rnews/Rnews_2002-3.pdf
# Times series boot
data(lynx)
lynx.fun = function(tsb){
  fit <- ar(tsb, order.max = 25)
  c(fit$order, mean(tsb))
}
tsboot(log(lynx), lynx.fun, R = 999, sim = "geom", l = 20)
# Burns http://www.burns-stat.com/documents/tutorials/
#the-statistical-bootstrap-and-other-resampling-methods-2/#prelim
spxibm <- as.matrix(read.table(
  "http://www.burns-stat.com/pages/Tutor/spx_ibm.txt",
  header=TRUE, sep='\t', row.names=1))
head(spxibm)
spxret <- spxibm[, "spx"]
ibmret <- spxibm[, "ibm"]
spx.boot.sum <- numeric(1000)
for(i in 1:1000){
  this.samp <- spxret[sample(251, 251, replace = TRUE)]
  spx.boot.sum[i] <- sum(this.samp)
}
plot(density(spx.boot.sum), lwd = 3, col = "steelblue")
abline(v = sum(spxret), lwd = 3, col = "gold")

spx.varsmu <- array(NA, c(251, 20)) # make 251 by 20 matrix
for(i in 1:20) {
  this.samp <- spxret[ sample(251, 251, replace=TRUE) ]
  spx.varsmu[,i] <- supsmu(1:251,
                           (this.samp - mean(this.samp))^2)$y
}
plot(supsmu(1:251, (spxret-mean(spxret))^2), type='l',
     xlab='Days', ylab='Variance')
matlines(1:251, spx.varsmu, lty=2, col='red')
#--------
# Two methods of boostrapping regression coefficients
# 1
beta.obs.boot <- numeric(1000)
for(i in 1:1000) {
  this.ind <- sample(251, 251, replace=TRUE)
  beta.obs.boot[i] <- coef(lm(
    ibmret[this.ind] ~ spxret[this.ind]))[2]
}
plot(density(beta.obs.boot), lwd=3, col="steelblue")
abline(v=coef(lm(ibmret ~ spxret))[2], lwd=3, col='gold')
#------------------------
# Sample the residuals from original regression
# use residuals to create new dependend variable (ibmret) using actual 
# explanatory variable (spxret). 
ibm.lm <- lm(ibmret ~ spxret)
ibm.fit <- fitted(ibm.lm)
ibm.resid <- resid(ibm.lm)
beta.resid.boot <- numeric(10000)
for(i in 1:10000) {
  this.ind <- sample(251, 251, replace=TRUE)
  beta.resid.boot[i] <- coef(lm(
    # fitted values plus residual regressed on spxret
    ibm.fit + ibm.resid[this.ind] ~ spxret))[2]
}
plot(density(beta.resid.boot), lwd=3, col="steelblue")
abline(v=coef(lm(ibmret ~ spxret))[2], lwd=3, col='gold')
# Boostrap package examples
require(boot)
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
boot(city, ratio, R = 999, stype = "w")  

diff_means <- function(d, f){
  n <- nrow(d)
  gp1 <- 1:table(as.numeric(d$series))[1]
  m1 <- sum(d[gp1, 1] * f[gp1])/sum(f[gp1])
  m2 <- sum(d[-gp1, 1] * f[-gp1])/sum(f[-gp1])
  ss1 <- sum(d[gp1, 1]^2 * f[gp1]) - (m1 * m2 * sum(f[gp1]))
  ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 * m2 * sum(f[-gp1]))
  c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
}
grav1 <- gravity[as.numeric(gravity[,2]) >= 7, ]
a <- boot(grav1, diff_means, R = 999, stype = "f", strata = grav1[,2])
str(a)
  
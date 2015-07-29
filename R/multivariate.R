#http://eranraviv.com/multivariate-volatility-forecasting-1/
library(quantmod)
k <- 3 # how many years back?
end<- format(Sys.Date(),"%Y-%m-%d") 
end_training <- "2015-01-01"
start<-format(Sys.Date() - (k*365),"%Y-%m-%d")
sym = c('SPY', 'TLT', "IEF") # S&P 500, long and medium term bonds, all ETFs
l <- length(sym)
dat0 = getSymbols(sym[1], src="yahoo", from=start, to=end, auto.assign = F, warnings = FALSE,symbol.lookup = F)
n = NROW(dat0)  
dat = array(dim = c(n,NCOL(dat0),l)) ; ret = matrix(nrow = n, ncol = l) 
for (i in 1:l){
  dat0 = getSymbols(sym[i], src="yahoo", from=start, to=end, auto.assign = F,warnings = FALSE,symbol.lookup = F)
  dat[1:n,,i] = dat0 
  ret[2:n,i] = (dat[2:n,4,i]/dat[1:(n-1),4,i] - 1) # returns, close to close, percent
}
ret <- na.omit(ret)# Remove the first observation
time <- index(dat0)[-1]
colnames(ret) <- sym
head(ret)
#----------------------
library(rugarch) # Alexios Ghalanos (2014). rugarch: Univariate GARCH models. R package version 1.3-4.
gjrtspec <- ugarchspec(mean.model=list(armaOrder=c(1,0)),variance.model =list(model = "gjrGARCH"),distribution="std") # std is student t distribution
Uvolatility_fit <- matrix(nrow=(TT),ncol=l) # container of a matrix to hold the volatilities of the three assets
for (i in 1:l){
  Tgjrmodel = ugarchfit(gjrtspec,ret[,i])
  Uvolatility_fit[,i] = as.numeric(sigma(Tgjrmodel))
}
#-------------------------------------------------------
# create a the covariance of the CCC model
nassets <- l # for better readability, the l looks too much like 1.
# make containers for the matrices over time:
samp_cor_tv <- cov_dcc <- cov_ccc <- array(dim=c(nassets, nassets, TT)) 
# Compute the sample unconditional correlation matrix:
samp_cor <- cor(ret) # will remain constant throughout the loop
wind <- 60 # roughly three months
for (i in (wind+1):TT){
  dt <- (Uvolatility_fit[i,])*diag(nassets)
  cov_ccc[,,i] <- dt %*% samp_cor %*% dt
  samp_cor_tv[,,i] <- cor(ret[(i-wind):i,])
  cov_dcc[,,i] <- dt %*% samp_cor_tv[,,i] %*% dt
}
#---------------------------
lwd1 <- 2
ann_percent <- 252*100
par()$mar # Margins are too wide for my taste
par(mfrow = c(2,1),mar=c(3.8,4.4,2.4,2.4))
plot(ann_percent*cov_ccc[1,1,]~time,ty="l",main="Annulaized variance, %",ylab="",lwd=lwd1,bty="o",
     cex.main=1.5,xlab="")
lines(ann_percent*cov_ccc[2,2,]~time, lwd=lwd1, col=2)
lines(ann_percent*cov_ccc[3,3,]~time, lwd=lwd1, col=3)
grid(col="blue")
legend("topleft", sym,col=1:3,lwd=lwd1,cex=1.1,bty="n",text.col=1:3)

plot(ann_percent*cov_ccc[1,2,]~time,ty="l",main="Annulaized covariance, %",ylab="",lwd=lwd1,bty="o",
     cex.main=1.5,xlab="",ylim=c(-2.5,1))
lines(ann_percent*cov_ccc[1,3,]~time, lwd=lwd1,col=2)
lines(ann_percent*cov_dcc[1,2,]~time, lwd=lwd1, col=1,lty=4)
lines(ann_percent*cov_dcc[1,3,]~time, lwd=lwd1, col=2,lty=4)
leg <-  c(paste("CCC-base<a href="http://eranraviv.com/wp-content/uploads/2015/04/DCC-based_covariance.png"><img src="http://eranraviv.com/wp-content/uploads/2015/04/DCC-based_covariance.png" alt="DCC-based covariance " width="1140" height="791" class="aligncenter size-full wp-image-2663" /></a>d covariance", sym[1], "with", sym[2]) , paste("CCC-based covariance", sym[1], "with", sym[3]))
legend("topleft",leg,col=1:2,lwd=lwd1,cex=1,bty="n",text.col=1:2)
leg <-  c(paste("DCC-based covariance", sym[1], "with", sym[2]) , paste("DCC-based covariance", sym[1], "with", sym[3]))
legend("topright",leg,col=1:2,lwd=lwd1,cex=1,bty="n",text.col=1:2,lty=4)
grid(col="blue")
#http://eranraviv.com/multivariate-volatility-forecasting-3-exponentially-weighted-model/
library(quantmod)
k <- 10 # how many years back?
end<- format(Sys.Date(),"%Y-%m-%d") 
start<-format(Sys.Date() - (k*365),"%Y-%m-%d")
sym = c('SPY', 'TLT', 'IEF')
l <- length(sym)
dat0 = getSymbols(sym[1], src="yahoo", from=start, to=end, auto.assign = F, warnings = FALSE, symbol.lookup = F)
n = NROW(dat0)  
dat = array(dim = c(n,NCOL(dat0),l)) ; ret = matrix(nrow = n, ncol = l) 
for (i in 1:l){
  dat0 = getSymbols(sym[i], src="yahoo", from=start, to=end, auto.assign = F, warnings = FALSE, symbol.lookup = F)
  dat[1:n,,i] = dat0 
  ret[2:n,i] = (dat[2:n,4,i]/dat[1:(n-1),4,i] - 1) # returns, close to close, percent
}
ret <- 100*na.omit(ret)# Remove the first observation
time <- index(dat0)[-1]
colnames(ret) <- sym
nassets <- dim(ret)[2]
TT <- dim(ret)[1]
tail(ret)
# need to download covEWMA function 
cor_mat <- covEWMA(as.data.frame(ret), lambda=.95, return.cor=T)
dim(cor_mat)
plot(cor_mat[,1,2]~time, ty = "l", las = 1, ylim = c(-1,1), main = "lambda= 0.95") ; grid()
lines(cor_mat[,1,3]~time, col = 2); lines(cor_mat[,2,3]~time, col = 3)

plot(cor_mat[,1,2]~time, ty = "l", las = 1, ylim = c(-1,1), main = "lambda= 0.95") ; grid()
lines(cor_mat[,1,3]~time, col = 2); lines(cor_mat[,2,3]~time, col = 3)

leg <-  c(paste(sym[1], "with", sym[2]) ,
          paste(sym[1], "with", sym[3]),
          paste(sym[2], "with", sym[3]))
legend("bottomleft",leg, col=1:3, lwd=lwd1, cex=.8, bty="n",text.col=1:3)

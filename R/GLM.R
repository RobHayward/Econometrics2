# http://www.r-bloggers.com/generalised-linear-models-in-r/
library(arm)
temp=c(11.9, 14.2, 15.2, 16.4, 17.2, 18.1, 18.5, 19.4, 22.1, 22.6, 23.4, 25.1)
units=c(185L, 215L, 332L, 325L, 408L, 421L, 406L, 412L, 522L, 445L, 544L, 614L)
icecream = data.frame(temp, units)
basicPlot <- function(...){
  plot(units ~ temp, data = icecream, bty = "n", lwd = 2, 
       main = "Number of ice creams sold", col = "#00526D", 
       xlab = "Tempreature (celsius)", ylab = "Units sold", ...)
  axis(side = 1, col = "grey")
  axis(side = 2, col = "grey")
}
basicPlot()
#-----------------
lsq.mod <- lsfit(icecream$temp, icecream$units)
basicPlot()
abline(lsq.mod, col = "orange", lwd = 2)
legend(x= "topleft", bty = "n", lwd = c(2, 2), lty = c(NA, 1), 
       legend = c("Observation", "linear least-squares"), 
       col  = c("#00526D", "orange"), pch = c(1, NA))
#-----------------------------------
lin.mod <- glm(units ~ temp, data = icecream, family = gaussian(link = "identity"))
library(arm)
display(lin.mod)
#-------------------------------------
log.lin.mod <- glm(log(units) ~ temp, data = icecream,
                   family = gaussian(link = "identity"))
display(log.lin.mod)
log.lin.sig <- summary(log.lin.mod)$dispersion
log.lin.pred <- exp(predict(log.lin.mod) + 0.5*log.lin.sig)
basicPlot()
lines(icecream$temp, log.lin.pred, col = "red", lwd = 2)
legend(x = "topleft", bty = "n", lwd = c(2, 2), lty = c(NA, 1), 
       legend = c("observation", "log-transformed LM"), 
       col = c("#00526D", "red"), pch = c(1, NA))
exp(coef(log.lin.mod)[1])
#--------------------------------------------
pois.mod <- glm(units ~ temp, data = icecream, 
                family = poisson(link = log))
display(pois.mod)
pois.pred <- predict(pois.mod, type = "response")
basicPlot()
lines(icecream$temp, pois.pred, col = "blue", lwd = 2)
legend(x = "topleft", bty = "n", lwd = c(2, 2), lty = c(NA, 1), 
       legend = c("observation", "Poission (log) GLM"), 
       col = c("#00526D", "blue"), pch = c(1, NA))
#----------------------------------------
market.size <- 800
icecream$opportunity <- market.size - icecream$units
bin.glm <- glm(cbind(units, opportunity) ~ temp, family = binomial(link = logit), 
               data = icecream)
display(bin.glm)
bin.pred <- predict(bin.glm, type = "response")*market.size
basicPlot()
lines(icecream$temp, bin.pred, col = "purple", lwd = 2)
legend(x = "topleft", bty = "n", lwd = c(2, 2), lty = c(NA, 1), 
       legend = c("observation", "Binomial (logit) GLM"), 
       col = c("#00526D", "purple"), pch = c(1, NA))
plogis(coef(bin.glm)[1])*market.size
plogis(coef(bin.glm)[1] + coef(bin.glm)[2]*35)*market.size
#-----------------------------------------
temp <- 0:35
p.lm <- predict(lin.mod, data.frame(temp=temp), type="response")
p.log.lm <- exp(predict(log.lin.mod, data.frame(temp=0:35), type="response") + 
                  0.5 * summary(log.lin.mod)$dispersion)
p.pois <- predict(pois.mod, data.frame(temp=temp), type="response")
p.bin <- predict(bin.glm, data.frame(temp=temp), type="response")*market.size 
basicPlot(xlim=range(temp), ylim=c(-20,market.size))
lines(temp, p.lm, type="l", col="orange", lwd=2)
lines(temp, p.log.lm, type="l", col="red", lwd=2)
lines(temp, p.pois, type="l", col="blue", lwd=2)
lines(temp, p.bin, type="l", col="purple", lwd=2)
legend(x="topleft", 
       legend=c("observation", 
                "linear model",
                "log-transformed LM",
                "Poisson (log) GLM",
                "Binomial (logit) GLM"),
       col=c("#00526D","orange", "red", 
             "blue", "purple"),  
       bty="n", lwd=rep(2,5), 
       lty=c(NA,rep(1,4)),
       pch=c(1,rep(NA,4)))
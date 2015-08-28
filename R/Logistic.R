library(caret)
data("GermanCredit")
Train <- createDataPartition(GermanCredit$Class, p = 0.6, list = FALSE)
training <- GermanCredit[Train, ]
testing <- GermanCredit[-Train,]
mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + 
                   Housing.Own + CreditHistory.Critical, data = training, 
                 method = "glm", family = "binomial")
exp(coef(mod_fit$finalModel))
predict(mod_fit, newdata = testing)
predict(mod_fit, newdata = testing, type = "prob")
head(predict(mod_fit, newdata = testing, type = "prob"))
# look at predictions vs the actual outcome
a <- cbind(predict(mod_fit, newdata = testing, type = "prob"), testing$Class)
head(a)
#---------------------
mod_fit_one <- glm(Class ~ Age + ForeignWorker + Property.RealEstate +
                     Housing.Own + CreditHistory.Critical, data = training,
                   family = "binomial")
mod_fit_two <- glm(Class ~ Age + ForeignWorker, data = training, family = 
                     "binomial")
library(lmtest)
lrtest(mod_fit_two, mod_fit_one)
# if the log-likellihoods are not the same, it suggests that the reduction
# in likelihood as a result of the additional variables is sufficient 
# to compensate for the additional variables. 
library(psc1)

# this comes from http://www.r-bloggers.com/computing-aic-on-a-validation-sample/
library(splines)
AIC(glm(dist ~ speed, data = train_cars, family = 
          poisson(link = "log")))
                                                            
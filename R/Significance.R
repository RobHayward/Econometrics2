# http://www.win-vector.com/blog/2015/08/how-do-you-know-if-your-data-has-signal/
library(ggplot2)
# return a frame of the deviance scores on the permuted data
permutation_test = function(dataf, ycol, nperm) {
  nrows = dim(dataf)[1]
  y = dataf[[ycol]]
  X = dataf[, setdiff(colnames(dataf), ycol), drop=FALSE]
  varnames = colnames(X)
  fmla = paste("y", paste(varnames, collapse=" + "), sep=" ~ ")
  deviances <- numeric(nperm)
  for(i in seq_len(nperm)) {
    # random order of rows
    ord = sample.int(nrows, size=nrows, replace=FALSE)
    model = glm(fmla, data=cbind(y=y[ord], X),
                family=binomial(link="logit"))
    #print(summary(model))
    deviances[[i]] =model$deviance
  }
  deviances
}

score_variable = function(dframe, ycol, var, nperm,
                          title='') {
  df=data.frame(y=dframe[[ycol]], x=dframe[[var]])
  
  mod = glm("y~x", data=df,
            family=binomial(link="logit"))
  vdev = mod$deviance
  vperm = permutation_test(df, "y", nperm)
  
  # count how many times vdev >= deviances from perm test
  num = sum(vperm <= vdev)
  vscore = num/nperm
  print(ggplot(data.frame(nullperm=vperm), aes(x=nullperm)) +
          geom_density() + geom_vline(xintercept=vdev, color='red') +
          ggtitle(paste(title, "left tail area ~", vscore)))
}  

set.seed(3266)
N = 1000
s1 = rnorm(N)
n1 = rnorm(N)
y = 2*s1 + rnorm(N)
dframe = data.frame(y=y>0, s1=s1, n1=n1)

nperm=500

# First, the model on the signaling variable
score_variable(dframe, "y", "s1", nperm,
               title='Signal variable deviance,')

score_variable(dframe, "y", "n1", nperm,
               title='Noise variable deviance,')

# get the significance of glm model 
get_significance = function(model) {
  delta_deviance = model$null.deviance - model$deviance
  df = model$df.null - model$df.residual
  sig = pchisq(delta_deviance, df, lower.tail=FALSE)
}
  
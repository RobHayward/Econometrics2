# This comes from http://datascienceplus.com/modelling-dependence-with-copulas/
# Aim is to model the correlation structure and the marginals
# this is easy with normal distribution but not other distributions
require(MAss)
# I cannot load the MASS package?  whatsup?????
set.seed(100)
m <- 3
n <- 2000
sigma <- matrix(c(1, 0.4, 0.2, 0.4, 1, -0.8, 0.2, -0.8, 1), nrow = 3)
z <- mvrnom(n, mu = rep(0, m), Sigma = sigma, empirical = T)

#     Test code for rp.likelihood

# setwd("/Volumes/adrian/hea/rpanel/rpanel_1.0-5-not-yet-released/fake-package.r")
setwd("~/Desktop/rpanel_1.1-0-not-yet-released/rpanel/R")

library(rpanel)

data(aircond)

source("rp-likelihood.r")
rp.likelihood("sum(log(dexp(data, theta)))", aircond, 
        0.005, 0.03)

source("rp-likelihood.r")
rp.likelihood("sum(log(dgamma(data, theta[1], theta[2])))",
        aircond, c(0.3, 0.005), c(3, 0.06))

example(rp.logistic)


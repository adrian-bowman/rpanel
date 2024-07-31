#     Test code for rp.likelihood

library(rpanel)
if (reinstall) devtools::install("rpanel")

data(aircond)

rp.likelihood("sum(log(dexp(data, theta)))", aircond, 
        0.005, 0.03)

rp.likelihood("sum(log(dgamma(data, theta[1], theta[2])))",
        aircond, c(0.3, 0.005), c(3, 0.06))

rp.logistic(river$Temperature, river$Low)

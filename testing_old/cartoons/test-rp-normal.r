#     Test code for rp.logistic

library(rpanel)
if (reinstall) devtools::install("rpanel")

y <- rnorm(50, mean = 10, sd = 0.5)
rp.normal(y)

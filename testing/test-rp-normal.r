#     Test code for rp.logistic

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

y <- rnorm(50, mean = 10, sd = 0.5)
rp.normal(y)

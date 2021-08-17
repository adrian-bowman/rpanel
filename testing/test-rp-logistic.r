#     Test code for rp.logistic

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

attach(river)

rp.logistic(Temperature, Low)

rp.logistic(Temperature, Low, panel = FALSE, beta = 2,
         display = c("jitter" = TRUE, 
                     "regression line" = TRUE, 
                     "fitted model" = TRUE))


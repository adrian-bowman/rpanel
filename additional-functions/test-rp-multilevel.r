#     Multilevel models cartoon

setwd("~/Desktop/rpanel/scripts")

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

source("rp-multilevel.r")
rp.multilevel(sd.levels = c(1, 0.5, 0.1), n.levels = c(4, 3, 4),
   names = c("Batch", "Barrel", "Sample"), mu = 10)

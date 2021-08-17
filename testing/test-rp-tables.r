#     Test of rp.tables

# setwd("/Volumes/adrian/research/rpanel")
# setwd("~/Desktop")
setwd("rpanel_1.1-0-not-yet-released/rpanel/R")

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

rp.tables(panel = FALSE)
rp.tables(panel = FALSE, observed.value = 1)
rp.tables(panel = FALSE, observed.value = 1, 
   tail.probability = "from observed")
rp.tables(panel = FALSE, tail.probability = "from observed")
rp.tables(panel = FALSE, observed.value = 1, 
   tail.probability = "from observed", tail.direction = "upper")

rp.tables(panel.plot = FALSE)

rp.tables()

#     Test of rp.tables

library(rpanel)
if (reinstall) devtools::install("rpanel")

rp.tables()

rp.tables(panel = FALSE)
rp.tables(panel = FALSE, observed.value = 1)
rp.tables(panel = FALSE, observed.value = 1, 
   tail.probability = "from observed")
rp.tables(panel = FALSE, tail.probability = "from observed")
rp.tables(panel = FALSE, observed.value = 1, 
   tail.probability = "from observed", tail.direction = "upper")

rp.tables(panel.plot = FALSE)

rp.tables()

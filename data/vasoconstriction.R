#     Vaso-constriction data from Finney (1947)

library(tidyverse)

data(vaso, package = 'catdata')
vaso <- vaso %>%
   rename(Volume = 1, Rate = 2, Vasoconstriction = 3) %>%
   mutate(Volume = round(exp(Volume), 4), Rate = round(exp(Rate), 4),
          Vasoconstriction = 2 - Vasoconstriction)
vaso$Rate[32] <- 0.30
vasoconstriction <- vaso

save(vasoconstriction, file = 'rpanel/data/vasoconstriction.rda')

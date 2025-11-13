#     Data on cholesterol from Snedecor & Cochran

library(tidyverse)

cholesterol <- read.table('data/cholesterol.txt', col.names = c('Age', 'Cholesterol')) %>%
   mutate(Location = factor(rep(c('Iowa', 'Nebraska'), c(11, 19))))
save(cholesterol, file = 'rpanel/data/cholesterol.rda')

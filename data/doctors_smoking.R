#     Doctors and smoking

library(tidyverse)
data(breslow, package = 'boot')
doctors_smoking <- breslow %>%
   dplyr::select(1, 'smoking' = 2, 'person_years' = 3, 'deaths' = 4) %>%
   mutate(age = as.numeric(as.character(age)),
          smoking = as.factor(smoking),
          rate = deaths * 100000 / person_years)
save(doctors_smoking, file = 'rpanel/data/doctors_smoking.rda')

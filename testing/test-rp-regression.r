detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

attach(CofE)
rp.regression(Giving ~ Employ + Elect + Attend, range = c(-60, 40), col = "green")
rp.regression(Giving ~ Employ         + Attend, range = c(-60, 40))

model <- lm(Giving ~ Employ + Elect + Attend, x = TRUE)

rp.regression(Employ, Giving)
rp.regression(cbind(Employ, Attend), Giving)

#     ests for the rp.regression function

devtools::install("rpanel")
library(rpanel)

with(CofE, 
  rp.regression(Giving ~ Employ + Elect + Attend, yrange = c(-60, 40), col = "green"))
with(CofE, 
  rp.regression(Giving ~ Employ         + Attend, yrange = c(-60, 40)))

model <- lm(Giving ~ Employ + Elect + Attend, data = CofE, x = TRUE)
rp.regression(model, yrange = c(-60, 40))

with(CofE, 
  rp.regression(Employ, Giving))


with(CofE, 
  rp.regression(cbind(Employ, Attend), Giving))

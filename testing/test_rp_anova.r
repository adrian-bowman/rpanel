#     Two-way anova

library(rpanel)
if (reinstall) devtools::install("rpanel")

with(poisons, 
   rp.anova(1/stime, treatment, model = c(TRUE, TRUE), model0 = c(TRUE, FALSE), panel = FALSE)
)

attach(poisons)
poison <- paste("poison", poison)

rp.anova(1/stime, treatment)
rp.anova(1/stime, treatment, poison)
rp.anova(1/stime, treatment, model = c(TRUE, TRUE), model0 = c(TRUE, TRUE), panel = FALSE)
rp.anova(1/stime, treatment, model = c(TRUE, TRUE), model0 = c(TRUE, TRUE), panel.plot = FALSE)

rp.anova(1/stime, treatment, poison,
   model = c(TRUE, TRUE, FALSE, FALSE),
   model0 = c(TRUE, TRUE, FALSE, FALSE))

rp.anova(1/stime, treatment, poison, hscale = 1.5)

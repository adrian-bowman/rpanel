#     Tests for the rp.regression function

library(rpanel)
if (reinstall) devtools::install("rpanel")

# ------------------------------------------------------------------------
test_label("Regression with more than two variables using a formula", test.prompt)
with(CofE, 
  rp.regression(Giving ~ Employ + Elect + Attend, yrange = c(-60, 40), col = "green"))
with(CofE, 
  rp.regression(Giving ~ Employ         + Attend, yrange = c(-60, 40)))

# ------------------------------------------------------------------------
test_label("Regression with more than two variables using a fitted model", test.prompt)
fac   <- rep("C", nrow(CofE))
fac   <- rep(c("A", "B", "C"), each = nrow(CofE) / 3)
model <- lm(Giving ~ Employ + Elect + Attend + fac, data = CofE)
rp.regression(model, yrange = c(-60, 40))
model <- lm(Giving ~ Employ + Elect + Attend + fac, data = CofE, x = TRUE)
summary(model)
rp.regression(model, yrange = c(-60, 40))

d        <- poisons
d$treat  <- factor(d$treatment)
d$poison <- factor(d$poison)
model    <- lm(1/stime ~ treat + poison, data = d, x = TRUE)
rp.regression(model)

summy <- summary(model)$coefficients[-1, ]
rownames(summy)[1] <- "t2"
effect.plot(summy[ , 1], summy[ , 2], rng = rep(1, 5))

# ------------------------------------------------------------------------
test_label("Regression with one variable", test.prompt)
with(CofE, 
  rp.regression(Employ, Giving))

# ------------------------------------------------------------------------
test_label("Regression with two variables", test.prompt)
with(CofE, 
  rp.regression(cbind(Employ, Attend), Giving))

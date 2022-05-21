#     Tests for the rp.regression function

library(rpanel)
if (reinstall) devtools::install("rpanel")

# ------------------------------------------------------------------------
test_label("Regression with more than two variables using a formula", test.prompt)
cat("Standard example:\n")
with(CofE, 
     rp.regression(Giving ~ Employ + Elect + Attend))
cat("Amend the returned ggplot:\n")
with(CofE, 
     rp.regression(Giving ~ Employ + Elect + Attend) + ggplot2::ylab("Covariates"))
cat("Set arguments:\n")
with(CofE, 
     rp.regression(Giving ~ Employ + Elect + Attend,
                   yrange = c(-60, 40), col = "blue", subset = 1:2))

# ------------------------------------------------------------------------
test_label("Regression with more than two variables using a fitted model", test.prompt)
fac   <- rep("C", nrow(CofE))
fac   <- rep(c("A", "B", "C"), each = nrow(CofE) / 3)
model <- lm(Giving ~ Employ + Elect + Attend + fac, data = CofE)
cat("Fail with 'x' not set to 'TRUE'\n")
try(rp.regression(model))
model <- lm(Giving ~ Employ + Elect + Attend + fac, data = CofE, x = TRUE)
rp.regression(model)

d        <- poisons
d$treat  <- factor(d$treatment)
d$poison <- factor(d$poison)
model    <- lm(1/stime ~ treat * poison, data = d, x = TRUE)
rp.regression(model)

# ------------------------------------------------------------------------
test_label("Regression with one variable", test.prompt)
with(CofE, 
  rp.regression(Employ, Giving))

# ------------------------------------------------------------------------
test_label("Regression with two variables", test.prompt)
with(CofE, 
  rp.regression(cbind(Employ, Attend), Giving))

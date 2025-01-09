#     Tests for the rp.lmsmall function

library(rpanel)
if (reinstall) devtools::install("rpanel")

# ------------------------------------------------------------------------
test_label("Regression with more than two variables using a formula", test.prompt)
cat("Standard example:\n")

devtools::install("rpanel")
library(rpanel)
cofeplus <- CofE
cofeplus$group <- factor(sample(rep(1:3, each = 14)))
cofeplus$x1 <- rnorm(42)
cofeplus$x2 <- rnorm(42)
cofeplus$x3 <- rnorm(42)
model <- lm(Giving ~ Employ + Elect + Attend * group + x1 + x2 +x3,
            data = cofeplus, x = TRUE)
model <- lm(Giving ~ Employ + Elect + Attend + group,
            data = cofeplus, x = TRUE)
model <- lm(Giving ~ Employ + Elect + Attend,
            data = cofeplus, x = TRUE)

# Simple linear regression
model <- lm(Giving ~ Employ, data = CofE)
rp.lmsmall(Giving ~ Employ, data = CofE)
model <- lm(Giving ~ Employ, data = CofE)
rp.lmsmall(model)
rp.lmsmall(log(Speed) ~ log(Mass), data = rodent)

# Regression with two covariates
rp.lmsmall(Giving ~ Employ + Attend, data = CofE)

# Ancova
# This doesn't work
rp.lmsmall(weight ~ hab + factor(month), data = gullweight)

# One- and two-way anova
rp.lmsmall(stime ~ factor(treatment), data = poisons)
rp.lmsmall(stime ~ factor(treatment) + factor(poison), data = poisons)

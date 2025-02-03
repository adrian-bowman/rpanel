#     Tests for the rp.regression function

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

rp.drop1(model)
rp.drop1(model, p.reference = NULL)
rp.coefficients(model)

# Regression with two covariates
rp.lmsmall(Giving ~ Employ + Attend, data = CofE)
with(CofE, rp.regression(cbind(Employ, Attend), Giving))

# Ancova
rp.lmsmall(weight ~ hab + month, data = gullweight)

# One- and two-way anova
rp.lmsmall(stime ~ factor(treatment), data = poisons)
rp.lmsmall(stime ~ factor(treatment) + factor(poison), data = poisons)

rp.regression(model)
rp.regression(model) + ggplot2::coord_flip()
with(CofE, 
     rp.regression(Giving ~ Employ + Elect + Attend))
with(CofE, 
     rp.regression(Giving ~ Employ + Elect + Attend,
                   model = panel = FALSE))
cat("Amend the returned ggplot:\n")
cat('This produces an error in the y-label.\n')
with(CofE, 
     rp.coefficients(Giving ~ Employ + Elect + Attend) + ggplot2::xlab("Covariates"))
cat("Set arguments:\n")
model <- lm(Giving ~ Employ + Elect + Attend, data = CofE)
rp.coefficients(model)
# Problem here as the intercept is showing
rp.coefficients(model, yrange = c(-60, 40), col = "blue", subset = 1:2)
with(CofE, 
     rp.regression(Giving ~ Employ + Elect + Attend,
                   yrange = c(-60, 40), col = "blue", subset = 1:2))
cat("Hypothesis testing:\n")
rp.datalink("~/iCloud/teaching/book/data", "set local directory")
cofe_2019 <- rp.wrangle('cofe_2019')
with(cofe_2019, rp.regression(log(Giving_per_member) ~ Attachment + IMD,
                              ci = FALSE) + ggplot2::coord_flip())
cat('Fails with a model which has only an intercept:\n')
mdl <- lm(response ~ 1, x = TRUE)
rp.regression(mddl)

# ------------------------------------------------------------------------
test_label("Regression with more than two variables using a fitted model", test.prompt)
fac   <- rep("C", nrow(CofE))
fac   <- rep(c("A", "B", "C"), each = nrow(CofE) / 3)
model <- lm(Giving ~ Employ + Elect + Attend + fac, data = CofE)
cat("Failure with 'x' not set to 'TRUE' ...\n")
try(rp.coefficients(model))
cat("Success with 'x' set to 'TRUE' ...\n")
model <- lm(Giving ~ Employ + Elect + Attend + fac, data = CofE, x = TRUE)
rp.regression(model)
cat("Change labels ...\n")
lbls <- rownames(summary(model)$coefficients)[-1]
lbls[5] <- "Sex (M vs. F)"
rp.regression(model, labels = lbls)
cat("Other arguments ...\n")
rp.regression(model, subset = 1:3, point.estimate = TRUE)

d        <- poisons
d$treat  <- factor(d$treatment)
d$poison <- factor(d$poison)
model    <- lm(1/stime ~ treat * poison, data = d, x = TRUE)
rp.regression(model)

# ------------------------------------------------------------------------
test_label("Logistic regression", test.prompt)
Conc   <- c(0.375, 0.75, 1.5, 3, 6, 12, 24)
fac    <- c(0, 1, 0, 1, 0, 1, 0)
Killed <- c(0, 1, 8, 11, 16, 18, 20)
N      <- rep(20, 7)
p      <- Killed / N
x      <- log(Conc)
model  <- glm(cbind(Killed, N - Killed) ~ x + fac, family = "binomial", x = TRUE)
rp.regression(model)

# ------------------------------------------------------------------------
test_label("Regression with one variable", test.prompt)
with(CofE, 
  rp.regression(Employ, Giving))

# ------------------------------------------------------------------------
test_label("Regression with two variables", test.prompt)
with(CofE, 
  rp.regression(cbind(Employ, Attend), Giving))

#     Tests for the rp.coefficients function

library(rpanel)
if (reinstall) devtools::install("rpanel")

# ------------------------------------------------------------------------
test_label("rp.coefficients", test.prompt)
cat("Standard example:\n")

devtools::install("rpanel")
library(rpanel)

cofeplus <- CofE
cofeplus$group <- factor(sample(rep(1:3, each = 14)))
cofeplus$x1 <- rnorm(42)
cofeplus$x2 <- rnorm(42)
cofeplus$x3 <- rnorm(42)
model <- lm(Giving ~ Employ + Elect + Attend + group + x1 + x2 +x3, data = cofeplus)
model <- lm(Giving ~ Employ + Elect + Attend + group, data = cofeplus)
model <- lm(Giving ~ Employ + Elect + Attend,         data = cofeplus)
model <- lm(Giving ~ Employ + Elect + Attend, data = CofE)
rp.coefficients(model)
rp.coefficients(model, ci = FALSE)
rp.coefficients(model, se.scale = TRUE)
rp.coefficients(model, style = 'shading', se.scale = TRUE)
rp.coefficients(model) + ggplot2::ylim(-100, 50)
rp.coefficients(model) + ggplot2::ylim(-20, 10)
rp.coefficients(model, subset = 2:3)
model0 <- lm(Giving ~ Employ + Attend, data = CofE)
rp.coefficients(model0, subset = 2:3, labels = names(coefficients(model))[-1])
rp.coefficients(model, style = 'shading')
rp.coefficients(model) + ggplot2::coord_flip()
rp.coefficients(model, se.scale = TRUE) + ggplot2::coord_flip()
rp.coefficients(model) + ggplot2::xlab("Covariates")
model <- lm(Giving ~ 1, data = CofE)
rp.coefficients(model)

rp.datalink("~/iCloud/teaching/book/data", "set local directory")
cofe_2019 <- rp.wrangle('cofe_2019')
model <- lm(log(Giving_per_member) ~ Attachment + IMD, data = cofe_2019)
rp.coefficients(model, ci = FALSE) + ggplot2::coord_flip()
cat('Fails with a model which has only an intercept:\n')

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

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
rp.lmsmall(Giving ~ Employ, data = CofE)
rp.lmsmall(Giving ~ Employ, data = CofE, xlab = 'x', ylab = 'y', glab = 'g')
model <- lm(Giving ~ Employ, data = CofE)
rp.lmsmall(model)
rp.lmsmall(log(Speed) ~ log(Mass), data = rodent)
# Calls generating errors
rp.lmsmall(Giving ~ 1, data = CofE)

# Regression with two covariates
rp.lmsmall(Giving ~ Employ + Attend, data = CofE)
rp.lmsmall(Giving ~ Employ + Attend, data = CofE,
           xlab = 'x', ylab = 'y', zlab = 'g')
path <- rp.datalink("DO_Clyde")
load(path)
clyde.sub  <- subset(clyde, Station == 4)
rp.lmsmall(DO ~ Temperature + Salinity, data = clyde.sub,
           residuals.showing = TRUE)

# Ancova
gullweight <- dplyr::mutate(gullweight, month = factor(month))
rp.lmsmall(weight ~ hab + month, data = gullweight)
rp.lmsmall(weight ~ factor(month) + hab, data = gullweight)
rp.lmsmall(weight ~ month, data = gullweight)

# First argument is a model
mdl <- lm(weight ~ hab + factor(month), data = gullweight)
rp.lmsmall(mdl)
rp.lmsmall(mdl, data = gullweight)

# One- and two-way anova
poisons <- dplyr::mutate(poisons,
                         treatment = factor(treatment),
                         poison    = factor(poison))
rp.lmsmall(stime ~ treatment, data = poisons)
rp.lmsmall(stime ~ treatment + poison, data = poisons)

rp.lmsmall(weight ~ month, data = gullweight)
mdl <- lm(weight ~ month, data = gullweight)
drop1(mdl, test = 'F')[-1, ]

with(poisons, rp.anova(stime, treatment, poison))

# panel = FALSE
rp.lmsmall(Giving ~ Employ + Attend, data = CofE, panel = FALSE)
rp.lmsmall(Giving ~ Employ + Attend, data = CofE, panel = FALSE,
           xlab = 'x')
rp.lmsmall(Giving ~ Employ + Attend, data = CofE, panel = FALSE,
           display = ~ Employ)
rp.lmsmall(Giving ~ Employ + Attend, data = CofE, panel = FALSE,
           display = ~ Attend)
rp.lmsmall(Giving ~ Employ + Attend, data = CofE, panel = FALSE,
           display = ~ 1)

# Coefficients make more sense if they are mean-centred
hab0 <- gullweight$hab - mean(gullweight$hab)
rp.lmsmall(weight ~ hab0 + factor(month), data = gullweight)

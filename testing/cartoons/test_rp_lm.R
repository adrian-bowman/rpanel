#     Tests for the rp.lm function

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
model <- lm(Giving ~ Employ + Elect + Attend, data = CofE)
model <- lm(Giving ~ Employ + Attend, data = CofE)

rp.lm(model)
rp.coefficients(model)

# Simple linear regression
rp.lm(Giving ~ Employ, data = CofE)
rp.lm(Giving ~ Employ, data = CofE, xlab = 'x', ylab = 'y', glab = 'g')
model <- lm(Giving ~ Employ, data = CofE)
rp.lm(model)
rp.lm(log(Speed) ~ log(Mass), data = rodent)

cat('Calls generating errors:\n')
try(rp.lm(Giving ~ 1, data = CofE))

# Regression with two covariates
rp.lm(Giving ~ Employ + Attend, data = CofE)
rp.lm(Giving ~ Employ + Attend, data = CofE,
           xlab = 'x', ylab = 'y', zlab = 'g')
path <- rp.datalink("DO_Clyde")
load(path)
clyde.sub  <- subset(clyde, Station == 4)
rp.lm(DO ~ Temperature + Salinity, data = clyde.sub,
           residuals.showing = TRUE)

# Ancova
gullweight <- dplyr::mutate(gullweight, month = factor(month),
                            x = rnorm(nrow(gullweight)),
                            month2 = rnorm(nrow(gullweight)))
rp.lm(weight ~ hab + month, data = gullweight)
rp.lm(weight ~ factor(month) + hab, data = gullweight)
rp.lm(weight ~ factor(month) * hab, data = gullweight)
rp.lm(weight ~ month, data = gullweight)
rp.lm(weight ~ hab + month, data = gullweight, panel = TRUE)

model <- lm(weight ~ hab + month, data = gullweight)
rp.drop1(model)

# Disallow covariates with names that match factors + levels
# The contrasts function shows the names (colnames)

model <- lm(weight ~ x + hab * month, data = gullweight)
model <- lm(weight ~ x + hab + month, data = gullweight)
coefficients(model)
rp.coefficients(model)

# Anova
poisons <- dplyr::mutate(poisons, poison = factor(poison),
                                  treatment = factor(treatment))
model <- lm(stime ~ poison * treatment, data = poisons)
coefficients(model)
rp.coefficients(model)
rp.drop1(model)
rp.lm(model)
model <- lm(stime ~ poison, data = poisons)

rp.lm(model, panel = FALSE)
rp.drop1(model)
rp.lm(model, inference = 'terms')
rp.coefficients(model)

# First argument is a model
mdl <- lm(weight ~ hab + factor(month), data = gullweight)
rp.lm(mdl)
rp.lm(mdl, data = gullweight)

# One- and two-way anova
poisons <- dplyr::mutate(poisons,
                         treatment = factor(treatment),
                         poison    = factor(poison))
rp.lm(stime ~ treatment, data = poisons)
rp.lm(stime ~ treatment + poison, data = poisons)

rp.lm(weight ~ month, data = gullweight)
mdl <- lm(weight ~ month, data = gullweight)
drop1(mdl, test = 'F')[-1, ]

with(poisons, rp.anova(stime, treatment, poison))

# panel = FALSE
rp.lm(Giving ~ Employ + Attend, data = CofE, panel = FALSE)
rp.lm(Giving ~ Employ + Attend, data = CofE, panel = FALSE,
           xlab = 'x')
rp.lm(Giving ~ Employ + Attend, data = CofE, panel = FALSE,
           display = ~ Employ)
rp.lm(Giving ~ Employ + Attend, data = CofE, panel = FALSE,
           display = ~ Attend)
rp.lm(Giving ~ Employ + Attend, data = CofE, panel = FALSE,
           display = ~ 1)

# Valid errors
y  <- rnorm(50)
x1 <- rnorm(50)
x2 <- rnorm(50)
g  <- factor(rbinom(50, 1, 0.5))
# This doesn't cause an error as it reverts to additive. Is that ok?
rp.lm(y ~ x1 * x2)
model <- lm(y ~ g + x1 * x2)
try(rp.lm(model))
try(rp.coefficients(model))
try(rp.drop1(model))

# Coefficients make more sense if they are mean-centred
hab0 <- gullweight$hab - mean(gullweight$hab)
rp.lm(weight ~ hab0 + factor(month), data = gullweight)

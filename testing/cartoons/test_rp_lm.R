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

# Disallow covariates with names that match factors + levels
# The contrasts function shows the names (colunames)

model <- lm(weight ~ x + hab * month, data = gullweight)
model <- lm(weight ~ x + hab + month, data = gullweight)
coefficients(model)
rp.coefficients(model)

# What if there are interactions among factors?
poisons <- dplyr::mutate(poisons, poison = factor(poison), treatment = factor(treatment))
model <- lm(stime ~ poison * treatment, data = poisons)
coefficients(model)
rp.coefficients(model)
rp.drop1(model)
rp.lm(model)

# Restrict to order 2 in terms of interactions?

# Does the range step work for factors? Currently based on levels?

# Does it work with factor interactions?

# Does it work for higher-order interactions?

#  How to handle subset and labels?

# Disallow terms which have interactions between numeric variables (by checking the factors table)

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

# Coefficients make more sense if they are mean-centred
hab0 <- gullweight$hab - mean(gullweight$hab)
rp.lm(weight ~ hab0 + factor(month), data = gullweight)

#     Code to test rp.ancova

library(rpanel)
if (reinstall) devtools::install("rpanel")

devtools::install("rpanel")
library(rpanel)
covariate <- runif(51)
response  <- 0.1 * covariate + rnorm(51, 0.05)
gp        <- sample(rep(1:3, each = 17))
rp.ancova(covariate, response, gp)

devtools::install("rpanel")
library(rpanel)
data(gullweight)
attach(gullweight)

rp.ancova(hab, weight, month)
rp.ancova(hab, weight, month, style = "new")
rp.ancova(hab, weight, month, style = "old")

model <- lm(weight ~ hab * month, x = TRUE)
summary(model)$coefficients
names(model$coefficients)
plt <- rp.regression(model)

# This doesn't work
# rp.ancova(hab, weight, month, model = c(TRUE, TRUE, TRUE, FALSE),
# model0 = c(TRUE, FALSE, FALSE, TRUE))

# rp.checkbox.change not currently implemented.
# fn <- function(panel) {
#    print(panel$adrian)
#    panel
# }
# panel <- rp.control()
# rp.checkbox(panel, adrian, fn, name = "checkbox")
# rp.checkbox.change(panel, "checkbox", "adrian", TRUE, action = fn)
# rp.checkbox.change(panel, "checkbox", "adrian", FALSE, action = fn)

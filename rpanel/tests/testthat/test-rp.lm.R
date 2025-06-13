#     Tests for the rp.lm function

# Regression with one covariate

test_that('Standard call', {
   expect_no_error(rp.lm(Giving ~ Employ, data = CofE))
})
test_that('Change axis labels', {
   expect_no_error(rp.lm(Giving ~ Employ, data = CofE, xlab = 'x', ylab = 'y'))
})
test_that('Model as input', {
   model <- lm(Giving ~ Employ, data = CofE)
   expect_no_error(rp.lm(model))
})
test_that('Error if no covariate is specified', {
   expect_error(rp.lm(Giving ~ 1, data = CofE))
})

# Regression with two covariates

test_that('Standard call', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE))
})
test_that('Change axis labels', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z'))
})
test_that('Residuals showing', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z',
                         residuals.showing = TRUE, panel = FALSE))
})
test_that('Select the model to be displayed', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         display.model = ~ Employ, panel = FALSE))
})
test_that('Select the null model to be displayed', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         display.model = ~ 1, residuals.showing = TRUE,
                         panel = FALSE))
})
test_that('Interaction between two covariates', {
   expect_no_error(rp.lm(Giving ~ Employ * Attend, data = CofE))
})

# One covariate and one factor

test_that('Standard call', {
   gullweight <- dplyr::mutate(gullweight, month = factor(month))
   expect_no_error(rp.lm(weight ~ hab + month, data = gullweight))
})

# One factor

test_that('Standard call', {
   poisons <- dplyr::mutate(poisons, poison = factor(poison),
                            treatment = factor(treatment))
   expect_no_error(rp.lm(stime ~ poison, data = poisons))
})

# Two factors

test_that('Standard call', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons))
})
test_that('Density display', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons,
                        uncertainty.display = 'shading'))
})
test_that('Static mode: standard call', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE))
})
test_that('Static mode: standard call, density display', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         uncertainty.display = 'shading'))
})
test_that('Static mode: valid display.model', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = ~ poison * treatment))
})
test_that('Static mode: invalid display.model', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      display.model = ~ something))
})
test_that('Static mode: valid comparison.model', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = ~ poison * treatment,
                         comparison.model = ~ poison + treatment))
})
test_that('Static mode: invalid setting of comparison.model without display.model', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      comparison.model = ~ poison))
})
test_that('Static mode: display.model and comparison.model are not adjacent', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      display = ~ poison, comparison.model = ~ poison * treatment))
})

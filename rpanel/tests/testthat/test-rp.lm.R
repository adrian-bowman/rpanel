#     Tests for the rp.lm function

#----------------------------------------------------------------
cat('\nRegression with one covariate\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Change axis labels', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ, data = CofE, xlab = 'x', ylab = 'y'))
   rp.control.dispose(pnl)
})
test_that('Model as input', {
   model <- lm(pnl <- Giving ~ Employ, data = CofE)
   expect_no_error(pnl <- rp.lm(model))
   rp.control.dispose(pnl)
})
test_that('Error if no covariate is specified', {
   expect_error(rp.lm(Giving ~ 1, data = CofE))
})

#----------------------------------------------------------------
cat('\nRegression with two covariates\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ + Attend, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Change axis labels', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z'))
   rp.control.dispose(pnl)
})
test_that('Interaction between two covariates', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ * Attend, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Static mode: change axis labels with a specified model', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z',
                         display.model = ~ Employ, panel = FALSE))
})
test_that('Static mode: residuals showing', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z',
                         residuals.showing = TRUE, panel = FALSE))
})
test_that('Static mode: select the model to be displayed', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         display.model = ~ Employ, panel = FALSE))
})
test_that('Static mode: select the null model to be displayed', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         display.model = ~ 1, residuals.showing = TRUE,
                         panel = FALSE))
})

# Remove rgl windows
rgl::close3d(rgl::rgl.dev.list())

#----------------------------------------------------------------
      cat('\nOne covariate and one factor\n')
#----------------------------------------------------------------

gullweight <- dplyr::mutate(gullweight, month = factor(month))
test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(weight ~ hab + month, data = gullweight))
   rp.control.dispose(pnl)
})

#----------------------------------------------------------------
      cat('\nOne factor\n')
#----------------------------------------------------------------

poisons <- dplyr::mutate(poisons, poison = factor(poison),
                         treatment = factor(treatment))
test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(stime ~ poison, data = poisons))
   rp.control.dispose(pnl)
})

#----------------------------------------------------------------
      cat('\nTwo factors\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(stime ~ poison + treatment, data = poisons))
   rp.control.dispose(pnl)
})
test_that('Density display', {
   expect_no_error(pnl <- rp.lm(stime ~ poison + treatment, data = poisons,
                                uncertainty.display = 'shading'))
   rp.control.dispose(pnl)
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

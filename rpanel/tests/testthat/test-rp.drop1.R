#     Tests for the rp.drop1 function

model <- lm(Giving ~ Employ, data = CofE)
test_that('Standard call', {
   expect_no_error(rp.drop1(model))
})
test_that('Change reference significance levels', {
   expect_no_error(rp.drop1(model, p.reference = c(0.05, 0.01, 0.001)))
})

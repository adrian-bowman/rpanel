#     Tests for the rp.drop1 function

test_that('Standard call', {
   expect_no_error(rp.drop1(lm(Giving ~ Employ, data = CofE)))
})

load_all()
rp.drop1(lm(Giving ~ Employ, data = CofE), p.reference = c(0.05, 0.01, 0.001))

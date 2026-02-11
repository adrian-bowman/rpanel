#     Tests for the rp.lm function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

snk <- function(x) capture.output(x, '~/Desktop/temp.txt')
snk(rp.datalink("~/iCloud/teaching/book/data", "set local directory"))

test_that('Standard calls', {
   expect_no_error(rp.coefficients(lm(Giving ~ Employ, data = CofE)))
   expect_no_error(rp.coefficients(lm(Giving ~ Employ + Attend, data = CofE)))
})

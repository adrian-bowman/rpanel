#     Tests for the rp.t_test function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

snk <- function(x) capture.output(x, '~/Desktop/temp.txt')
snk(rp.datalink("~/iCloud/teaching/book/data", "set local directory"))

x <- matrix(c(19, 41, 32, 28), ncol = 2,
            dimnames = list(c("non-smoker", "smoker"),
                            c("cases", "controls")))

x2 <- t(matrix(c(87, 73, 64, 15, 25, 16, 59, 84, 92), ncol = 3,
               dimnames = list(c("red", "yellow", "blue"),
                                c("round", "square", "oblong"))))

test_that('Standard calls', {
   expect_no_error(rp.contingency(x))
   expect_no_error(rp.contingency(t(x)))
   expect_no_error(rp.contingency(x2))
   expect_no_error(rp.contingency(x2, style = 'aligned'))
   expect_error(rp.contingency(x2, style = 'proportions'))
   expect_no_error(rp.contingency(x, uncertainty = 'shading'))
   expect_no_error(rp.contingency(x, uncertainty = 'shading'))
   expect_no_error(rp.contingency(x, uncertainty = 'shading', proportion.scale = 'free'))
   expect_no_error(rp.contingency(x, style = 'aligned', uncertainty = 'shading'))
   expect_no_error(rp.contingency(x, style = 'aligned', uncertainty = 'shading',
                                  proportion.scale = 'free'))
   expect_no_error(rp.contingency(x, uncertainty = 'shading'))
   expect_no_error(rp.contingency(x, style = 'aligned', uncertainty = 'shading'))
   expect_no_error(rp.contingency(x, style = 'aligned', uncertainty = 'shading'))
   expect_no_error(rp.contingency(x, uncertainty = 'shading') + ggplot2::coord_flip())
   expect_no_error(rp.contingency(x, style = "aligned"))
})

test_that('Errors in input', {
   expect_error(rp.contingency(matrix(c(19.1, 41, 32, 28), ncol = 2)))
   expect_error(rp.contingency(matrix(c(-19, 41, 32, 28), ncol = 2)))
   expect_error(rp.contingency(array(1:8, dim = rep(2, 3))))
   expect_error(rp.contingency( matrix(1:3)))
})

test_that('Add to the ggplot object', {
   p <- rp.contingency(x)
   expect_no_error(p + ggplot2::ggtitle("A contingency table"))
   expect_no_error(rp.contingency(x) + ggplot2::ggtitle("A contingency table"))
})

test_that('Displayed values', {
   p <- rp.contingency(x)
   expect_no_error(rp.contingency(x, values = 'observed'))
   expect_no_error(rp.contingency(x, values = 'expected'))
   expect_no_error(rp.contingency(x, values = 'proportions'))
   expect_error(rp.contingency(x, values = 'ddd'))
})

#     Tests for the rp.t_test function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()
# rp.datalink("~/iCloud/teaching/book/data", "set local directory")

x <- matrix(c(19, 41, 32, 28), ncol = 2,
            dimnames = list(c("non-smoker", "smoker"),
                            c("cases", "controls")))

# Allow a table of (non-integer) expected values to be plotted in mosaic style
# for comparison?
# xo <- matrix(c(34.5, 25.5, 34.5, 25.5), ncol = 2,
#              dimnames = list(c("smoker", "non-smoker"),
#                              c("cases", "controls")))
# rp.contingency(xo)

x <- t(matrix(c(87,73,64,15,25,16,109,84,92), ncol = 3))
rownames(x) <- c("red", "yellow", "blue")
colnames(x) <- c("round", "square", "oblong")

# Look at the range of things mosaicplot can do - residuals etc.
mosaicplot(t(x))
mosaicplot(t(x), shade = TRUE, type = "p")
mosaicplot(t(x), shade = TRUE, type = "d")
mosaicplot(t(x), shade = TRUE, type = "F")

rp.contingency(x)

rp.contingency(x, uncertainty = TRUE)

load_all()
rp.contingency(x, uncertainty = TRUE, uncertainty.style = 'violin')

rp.contingency(x, style = 'aligned', uncertainty = TRUE)
rp.contingency(x, style = 'aligned', uncertainty = TRUE, uncertainty.style = 'violin')

rp.contingency(x, uncertainty = TRUE) + ggplot2::coord_flip()
rp.contingency(x, style = "aligned")
rp.contingency(x, style = "aligned", uncertainty = TRUE)

# Checks on x
x <- matrix(c(19.1, 41, 32, 28), ncol = 2)
rp.contingency(x)
x <- array(1:8, dim = rep(2, 3))
rp.contingency(x)
x <- matrix(1:3)
rp.contingency(x)

p <- rp.contingency(x)
p + ggplot2::ggtitle("A contingency table")

rp.contingency(x, values = "observed")
rp.contingency(x, values = "expected")
rp.contingency(x, values = "proportions")
rp.contingency(x, values = c('observed', 'expected'))
rp.contingency(x, values = "ddd")

rp.contingency(t(x))
rp.contingency(t(x), values = "observed")
rp.contingency(t(x), values = "expected")

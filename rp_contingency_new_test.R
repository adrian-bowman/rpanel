#     A new go at graphics for a contingency table

x <- matrix(c(19, 41, 32, 28), ncol = 2,
            dimnames = list(c("non-smoker", "smoker"),
                            c("cases", "controls")))

x <- t(matrix(c(87,73,64,15,25,16,109,84,92), ncol = 3))
rownames(x) <- c("red", "yellow", "blue")
colnames(x) <- c("round", "square", "oblong")

# Look at the range of things mosaicplot can do - residuals etc.
mosaicplot(t(x))
mosaicplot(t(x), shade = TRUE, type = "p")
mosaicplot(t(x), shade = TRUE, type = "d")
mosaicplot(t(x), shade = TRUE, type = "F")

rp.contingency(x)
rp.contingency(x, style = "aligned")
rp.contingency(x, style = "aligned", uncertainty = TRUE)
rp.contingency(x, uncertainty = TRUE)

# Checks on x
x <- matrix(c(19.1, 41, 32, 28), ncol = 2)
rp.contingency(x)
x <- array(1:8, dim = rep(2, 3))
rp.contingency(x)
x <- matrix(1:3)
rp.contingency(x)

mosaicplot(x)
rp.contingency(t(x), display = "comparison")

rp.contingency(x)

p <- rp.contingency(x)
p + ggplot2::ggtitle("A contingency table")

rp.contingency(x, values = "observed")
rp.contingency(x, values = "expected")
rp.contingency(x, values = "ddd")

rp.contingency(t(x))
rp.contingency(t(x), values = "observed")
rp.contingency(t(x), values = "expected")


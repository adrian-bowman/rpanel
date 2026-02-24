#     Tests for the rp.power function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

snk <- function(x) capture.output(x, '~/Desktop/temp.txt')
snk(rp.datalink("~/iCloud/teaching/book/data", "set local directory"))

test_that('Standard calls', {
   expect_no_error(pnl <- rp.power())
   rp.control.dispose(pnl)
   expect_no_error(rp.power(panel = FALSE, populations.showing = TRUE))
})

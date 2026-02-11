#     Tests for rp.colours

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

snk <- function(x) capture.output(x, '~/Desktop/temp.txt')
snk(rp.datalink("~/iCloud/teaching/book/data", "set local directory"))

# test_that('Standard calls', {
#    expect_no_error(rp.colours())
#    expect_no_error(rp.colours())
#    expect_no_error(rp.colours('notch'))
#    expect_no_error(rp.colours(c('estimate', 'notch')))
# })
# 
# test_that('Warnings', {
#    expect_warning(rp.colours(c('estimate', 'xnotch')))
#    expect_warning(rp.colours(c('xestimate', 'xnotch')))
# })
# 
# test_that('Errors', {
#    expect_error(rp.colours(1))
# })

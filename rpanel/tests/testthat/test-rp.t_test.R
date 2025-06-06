# It's worth noting that the vdiffr package also supports comparing plots.
# A nice feature is that it integrates with the testthat package -- it's actually
# used for testing in ggplot2 -- and it has an add-in for RStudio to help manage your testsuite.

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

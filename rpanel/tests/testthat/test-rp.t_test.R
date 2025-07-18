#     Tests for the rp.t_test function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()
# rp.datalink("~/iCloud/teaching/book/data", "set local directory")

sleep_wide <- tidyr::pivot_wider(sleep, values_from = extra, names_from = group,
                                 names_prefix = 'drug_')
sleep_diff <- with(sleep_wide, drug_2 - drug_1)

load_all()
rp.t_test(sleep_diff)

test_that('Single sample: standard calls', {
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'none'))
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'none', se.scale = FALSE))
   expect_no_error(rp.t_test(sleep_diff))
   expect_no_error(rp.t_test(sleep_diff ~ 1))
   expect_no_error(rp.t_test(sleep_diff, mu = 1))
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'sample mean', ci = FALSE))
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'sample mean'))
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'sample mean', conf.level = 0.99))
   expect_no_error(rp.t_test(sleep_diff, mu = 0, uncertainty = 'reference'))
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'reference'))
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'reference', vlab = 'Something'))
   expect_no_error(rp.t_test(sleep_diff, mu = 10, uncertainty = 'reference'))
   expect_no_error(rp.t_test(sleep_diff, mu = -5, uncertainty = 'reference'))
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'reference', alternative = 'greater'))
   expect_no_error(rp.t_test(sleep_diff, uncertainty = 'reference', alternative = 'less')
})

test_that('Single sample: systematic calls with arguments', {
   for (display in c('histogram', 'density')) {
     for (scl in c(FALSE, TRUE)) {
       expect_no_error(rp.t_test(sleep_diff, uncertainty = 'none', display = display, scale = scl))
       expect_no_error(rp.t_test(sleep_diff, display = display, scale = scl))
       expect_no_error(rp.t_test(sleep_diff, mu = 0, display = display, scale = scl))
       expect_no_error(rp.t_test(sleep_diff, uncertainty = 'reference',
                                 display = display, scale = scl))
   }
}
})

test_that('Paired data', {
   s1 <- sleep$extra[sleep$group == 1]
   s2 <- sleep$extra[sleep$group == 2]
   expect_no_error(rp.t_test(s2, s1, paired = TRUE, uncertainty = 'none'))
   expect_no_error(rp.t_test(s2, s1, paired = TRUE, vlab = 'Something'))
   expect_no_error(rp.t_test(s2, s1, paired = TRUE, mu = 0))
   expect_no_error(rp.t_test(s2, s1, paired = TRUE, uncertainty = 'reference'))
   sleep2 <- reshape(sleep, direction = "wide", idvar = "ID", timevar = "group")
   expect_no_error(rp.t_test(Pair(extra.1, extra.2) ~ 1, data = sleep2))
})

x  <- rnorm(25) + 1

test_that('Single sample: simulated data', {
   expect_no_error(rp.t_test(x))
   expect_no_error(rp.t_test(x + 10))
   expect_no_error(rp.t_test(x + 10, mu = 0))
   expect_no_error(rp.t_test(x, zoom = TRUE))
   expect_no_error(rp.t_test(x + 10, zoom = TRUE))
   expect_no_error(rp.t_test(x + 10, mu = 0, zoom = TRUE))
   expect_no_error(rp.t_test(x, uncertainty = 'reference'))
   expect_no_error(rp.t_test(x, uncertainty = 'none', mu = 0))
   expect_no_error(rp.t_test(x, mu = 0))
   expect_no_error(rp.t_test(x, mu = 0, uncertainty = 'reference'))
   expect_no_error(rp.t_test(x, mu = 0))
   expect_no_error(rp.t_test(x, mu = 1))
   expect_no_error(rp.t_test(x, mu = 1, scale = TRUE))
})

y  <- rnorm(25) + 2
xy <- c(x, y)
g  <- paste('group', as.character(rep(1:2, each = 25)))

test_that('Two-sample data', {
   expect_no_error(rp.t_test(x, y, paired = TRUE))
   expect_no_error(rp.t_test(xy, g))
   expect_no_error(rp.t_test(x, y, uncertainty = 'none'))
   expect_no_error(rp.t_test(x, y, uncertainty = 'none', se.scale = TRUE))
   expect_no_error(rp.t_test(x, y, uncertainty = 'none', se.scale = TRUE, mu = 0))
   expect_no_error(rp.t_test(x, y))
   expect_no_error(rp.t_test(x, y, xlab = 'xlabel', ylab = 'ylabel', vlab = 'Something'))
   expect_no_error(rp.t_test(x, y, mu = 0))
   expect_no_error(rp.t_test(x, y, uncertainty = 'reference'))
   expect_no_error(rp.t_test(x, y, display = 'density'))
   expect_no_error(rp.t_test(x, y, display = 'histogram'))
   expect_no_error(rp.t_test(x, y, scale = FALSE))
   expect_no_error(rp.t_test(x, y, uncertainty = 'reference', scale = TRUE))
   expect_no_error(rp.t_test(extra ~ group, data = sleep))
   for (display in c('histogram', 'density')) {
      expect_no_error(rp.t_test(x, y))
      expect_no_error(rp.t_test(x, y, display = display, uncertainty = 'none'))
      expect_no_error(rp.t_test(x, y, display = display, zoom = TRUE))
      expect_no_error(rp.t_test(x, y, mu = 0, display = display))
      expect_no_error(rp.t_test(x, y, mu = 0, display = display, zoom = TRUE))
      expect_no_error(rp.t_test(x, y, display = display, seed = 6245))
      expect_no_error(rp.t_test(x, y, uncertainty = 'reference',
                                display = display, seed = 6245))
      expect_no_error(rp.t_test(x, y, uncertainty = 'reference', mu = 1,
                                display = display, seed = 6245))
      expect_no_error(rp.t_test(x, y, mu = 1, , display = display, seed = 6245))
      expect_no_error(rp.t_test(x, y, var.equal = TRUE, display = display, seed = 6245))
      expect_no_error(rp.t_test(y, x, zoom = TRUE))
      expect_warning(rp.t_test(y, x, display = 'something else'))
   }
})

test_that('Two-sample data: formula input', {
   gf  <- factor(g)
   expect_no_error(rp.t_test(xy ~ gf))
   expect_no_error(rp.t_test(xy ~ gf, se.scale = FALSE))
   expect_no_error(rp.t_test(xy ~ gf, uncertainty = 'sample mean'))
   expect_no_error(rp.t_test(xy ~ gf, uncertainty = 'reference'))
   gf3 <- factor(paste('group', as.character(c(rep(1:2, each = 20), rep(3, 10)))))
   expect_error(rp.t_test(xy ~ gf3))
   
})

test_that('Zoom', {
})
 
# load_all()
# rp.t_test(x, zoom = TRUE)
# rp.t_test(x, y, se.scale = FALSE, zoom = TRUE)
# rp.t_test(x, y, zoom = TRUE)
# rp.t_test(x, y, uncertainty = 'sample mean', zoom = TRUE)
# rp.t_test(x, y, uncertainty = 'reference', zoom = TRUE)

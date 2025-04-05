#     Tests for the rp.t_test function

library(rpanel)
if (reinstall) devtools::install("rpanel")

# ------------------------------------------------------------------------
test_label("t-tests and confidence intervals for one and two samples", test.prompt)
cat("Standard example:\n")

devtools::install("rpanel")
library(rpanel)

library(tidyverse)
sleep_wide <- pivot_wider(sleep, values_from = extra, names_from = group,
                          names_prefix = 'drug_')
sleep_diff <- with(sleep_wide, drug_2 - drug_1)

rp.t_test(sleep_diff)

for (display in c('histogram', 'density', 'violin')) {
  for (scl in c(FALSE, TRUE)) {
    rp.t_test(sleep_diff, uncertainty = 'none', display = display, scale = scl)
    rp.t_test(sleep_diff, display = display, scale = scl)
    rp.t_test(sleep_diff, mu = 0, display = display, scale = scl)
    rp.t_test(sleep_diff, uncertainty = 'reference', display = display, scale = scl)
  }
}


rp.t_test(sleep$extra[sleep$group == 2], sleep$extra[sleep$group == 1],
         paired = TRUE, uncertainty = 'none')
rp.t_test(sleep$extra[sleep$group == 2], sleep$extra[sleep$group == 1],
         paired = TRUE)
rp.t_test(sleep$extra[sleep$group == 2], sleep$extra[sleep$group == 1],
         paired = TRUE, mu = 0)
rp.t_test(sleep$extra[sleep$group == 2], sleep$extra[sleep$group == 1],
         paired = TRUE, uncertainty = 'reference')


x <- rnorm(50)
y <- rnorm(50) + 1
g <- rep(1:2, each = 25)

rp.t_test(x)
rp.t_test(x + 10)
rp.t_test(x + 10, mu = 0)
rp.t_test(x, zoom = TRUE)
rp.t_test(x + 10, zoom = TRUE)
rp.t_test(x + 10, mu = 0, zoom = TRUE)
rp.t_test(x, uncertainty = 'reference')
rp.t_test(x, uncertainty = 'none', mu = 0)
rp.t_test(x, mu = 0)
rp.t_test(x, mu = 0, uncertainty = 'reference')
rp.t_test(x, mu = 0)
rp.t_test(x, mu = 1)
rp.t_test(x, mu = 1, scale = TRUE)
rp.t_test(x, y, paired = TRUE)

# Two-sample data

x <- x + 1
y <- y + 10
display <- 'histogram'
display <- 'density'
display <- 'violin'
rp.t_test(x, y)
rp.t_test(x, y, display = display, uncertainty = 'none')
rp.t_test(x, y, display = display, zoom = TRUE)
rp.t_test(x, y, mu = 0, display = display)
rp.t_test(x, y, mu = 0, display = display, zoom = TRUE)
rp.t_test(x, y, display = display, seed = 6245)
rp.t_test(x, y, uncertainty = 'reference', display = display, seed = 6245)
rp.t_test(x, y, uncertainty = 'reference', mu = 1, , display = display, seed = 6245)
rp.t_test(x, y, mu = 1, , display = display, seed = 6245)
rp.t_test(x, y, var.equal = TRUE, display = display, seed = 6245)
rp.t_test(x, y, , display = display, seed = 6245) + ggplot2::coord_flip()
rp.t_test(y, x)
rp.t_test(y, x, zoom = TRUE)

# Allow a formula to be passed?


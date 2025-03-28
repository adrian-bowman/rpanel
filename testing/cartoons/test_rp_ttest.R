#     Tests for the rp.ttest function

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

rp.ttest(sleep_diff)

for (display in c('histogram', 'density', 'violin')) {
  for (scl in c(FALSE, TRUE)) {
    rp.ttest(sleep_diff, uncertainty = 'none', display = display, scale = scl)
    rp.ttest(sleep_diff, display = display, scale = scl)
    rp.ttest(sleep_diff, mu = 0, display = display, scale = scl)
    rp.ttest(sleep_diff, uncertainty = 'reference', display = display, scale = scl)
  }
}


rp.ttest(sleep$extra[sleep$group == 2], sleep$extra[sleep$group == 1],
         paired = TRUE, uncertainty = 'none')
rp.ttest(sleep$extra[sleep$group == 2], sleep$extra[sleep$group == 1],
         paired = TRUE)
rp.ttest(sleep$extra[sleep$group == 2], sleep$extra[sleep$group == 1],
         paired = TRUE, mu = 0)
rp.ttest(sleep$extra[sleep$group == 2], sleep$extra[sleep$group == 1],
         paired = TRUE, uncertainty = 'reference')


x <- rnorm(50)
y <- rnorm(50) + 1
g <- rep(1:2, each = 25)

rp.ttest(x)
rp.ttest(x, uncertainty = 'reference')
rp.ttest(x, uncertainty = 'none', mu = 0)
rp.ttest(x, mu = 0)
rp.ttest(x, mu = 0, uncertainty = 'reference')
rp.ttest(x, mu = 0)
rp.ttest(x, mu = 1)
rp.ttest(x, mu = 1, scale = TRUE)
rp.ttest(x, y, paired = TRUE)

# Two-sample data

rp.ttest(x, y)
rp.ttest(x, y, seed = 6245)
rp.ttest(x, y, uncertainty = 'reference', seed = 6245)
rp.ttest(x, y, uncertainty = 'reference', reference = 1, seed = 6245)
rp.ttest(x, y, var.equal = TRUE, seed = 6245)
rp.ttest(x, y, seed = 6245) + ggplot2::coord_flip()

# Allow a formula to be passed?


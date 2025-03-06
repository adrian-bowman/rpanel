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
rp.ttest(sleep_diff, seed = 1673)
rp.ttest(sleep_diff, mu = 0, seed = 1673)
rp.ttest(sleep_diff, uncertainty = 'reference', seed = 1673)
rp.ttest(sleep_diff, uncertainty = 'none', seed = 1673)



x <- rnorm(50)
y <- rnorm(50) + 1
g <- rep(1:2, each = 25)

rp.ttest(x, seed = 4276)
rp.ttest(x, uncertainty = 'reference', seed = 4276)
rp.ttest(x, uncertainty = 'none', mu = 0, seed = 4276)
rp.ttest(x, mu = 0, seed = 4276)
rp.ttest(x, mu = 0, uncertainty = 'reference', seed = 4276)
rp.ttest(x, mu = 0, seed = 4276)
rp.ttest(x, mu = 1, seed = 4276)
rp.ttest(x, y, paired = TRUE)

rp.ttest(x, y)
rp.ttest(x, y, seed = 6245)
rp.ttest(x, y, uncertainty = 'reference', seed = 6245)
rp.ttest(x, y, uncertainty = 'reference', reference = 1, seed = 6245)
rp.ttest(x, y, var.equal = TRUE, seed = 6245)
rp.ttest(x, y, seed = 6245) + ggplot2::coord_flip()

# Plot the reference if it is provided.
# Allow a formula to be passed.


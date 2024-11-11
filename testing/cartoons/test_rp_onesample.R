#     rp.onesample tests

library(rpanel)
if (reinstall) devtools::install("rpanel")

test_label("standard call", test.prompt)
library(ggplot2)
library(ggforce)
x <- rnorm(50)
rp.onesample(x)
rp.onesample(x, seed = 1234)
rp.onesample(x, scale = TRUE, seed = 1234)
rp.onesample(x, reference = 0, seed = 1234)
rp.onesample(x, reference = 0, scale = TRUE, seed = 1234)
rp.onesample(x, reference = 0, uncertainty = 'sample mean', seed = 1234)
plt <- rp.onesample(x)
plt + ggplot2::ggtitle("One-sample inference")
rp.onesample(x, height = 0.5)
rp.onesample(x, data_plot = 'jitter')
cat('This one gives a warning for some reason:\n')
rp.onesample(x) + ggplot2::xlim(-3, 3)
rp.onesample(x, uncertainty = 'g')

rp.sample()

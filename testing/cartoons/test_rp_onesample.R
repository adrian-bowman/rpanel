#     rp.geosim tests

library(rpanel)
if (reinstall) devtools::install("rpanel")

test_label("standard call", test.prompt)
x <- rnorm(50)
rp.onesample(x) + ggplot2::xlim(-3, 3)
rp.onesample(x, reference = 0)
plt <- rp.onesample(x, data_plot = "sina")
plt + ggplot2::ggtitle("One-sample inference")

rp.sample()

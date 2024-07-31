#     Test code for rp.sample

library(rpanel)
if (reinstall) devtools::install("rpanel")

rp.sample()

# This does not work without restarting R.  Why is that?
# rp.sample(shiny = FALSE)

rp.sample(hscale = 1.5)

# Radioactivity in samples
rp.sample(4.7, 0.35, 73)


ind <- grep("green", colors())
plot(1:length(ind), type = "n")
abline(h = 1:length(ind), col = colors()[ind])


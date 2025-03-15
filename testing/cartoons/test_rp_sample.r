#     Test code for rp.sample

library(rpanel)
if (reinstall) devtools::install("rpanel")

rp.sample()
rp.sample(display = 'violin')

library(ggplot2)
x <- rnorm(25)
dens <- density(x)
dfrm <- data.frame(x = dens$x, y = dens$y)
col.dens <- 'grey75'
col.pars <- 'darkblue'
col.pars <- 'blue'
col.pars <- 'darkgreen'
ggplot2::ggplot(dfrm, aes(x, y)) +
   geom_area(, col = col.dens, fill = col.dens) +
   geom_function(fun = dnorm, args = list(mean = 0, sd = 1), linewidth = 1.5, col = col.pars)

rp.sample(hscale = 1.5)

# Radioactivity in samples
rp.sample(4.7, 0.35, 73)


ind <- grep("green", colors())
plot(1:length(ind), type = "n")
abline(h = 1:length(ind), col = colors()[ind])

# Check out the old style function
rp.sample(style = 'old')

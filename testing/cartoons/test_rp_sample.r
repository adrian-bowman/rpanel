#     Test code for rp.sample

library(rpanel)
if (reinstall) devtools::install("rpanel")

rp.sample()
rp.sample(display = 'violin')
rp.sample(ggplot = FALSE)

result <- rp.sample(panel = FALSE)
result <- rp.sample(panel = FALSE, display.sample = c('+/- 2 st.dev.' = TRUE))
result <- rp.sample(panel = FALSE,
                    display.sample = c('mean' = TRUE, '+/- 2 st.dev.' = TRUE))
result <- rp.sample(panel = FALSE, display = 'density',
                    display.sample = c('mean' = TRUE, '+/- 2 st.dev.' = TRUE,
                                       'population' = TRUE),
                    display.mean = c('sample mean' = TRUE))
print(result$data)
print(result$mean)

ggplot2::ggplot(data.frame(x = rnorm(1)), ggplot2::aes(x)) +
   ggplot2::geom_histogram(breaks = -3:3, ggplot2::aes(y = ggplot2::after_stat(density)))

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

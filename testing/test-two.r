library(rpanel)

draw <- function(panel) {
   hist(panel$x)
   abline(v = panel$v, col = "red", lty = 2)
   print(panel$v)
   panel
}
redraw <- function(panel) {
   rp.tkrreplot(panel, plot)
   panel
}

x <- rnorm(25)
panel <- rp.control(x = x, v = 0)
rp.tkrplot(panel, plot, draw, pos = "right")
rp.slider(panel, v, min(x), max(x), redraw)
rp.doublebutton(panel, v, 0.01, action = redraw)

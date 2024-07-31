#------------------------------------------------------
#       rp.tkrplot
#------------------------------------------------------

library(rpanel)
if (reinstall) devtools::install("rpanel")

# Check the use of foreground

panel <- rp.control()
# This gif file is not transparent so the plot doesn't show.
rp.tkrplot(panel, plot, function(panel) plot(runif(10), runif(10)),
           action = function(panel, x, y) print(c(x, y)),
           foreground = 'testing/tools/compressed.gif')

#  Check settings of hscale and vscale

rp.geosim()
rp.geosim(hscale = 0.9, vscale = 0.9)
rp.geosim(hscale = 1.25, vscale = 1.25)

# The influence of a new point on a regression line

plotdata <- function(panel) {
   plot(panel$x, panel$y)
   abline(lm(panel$y ~ panel$x))
   panel
   }
plotnewdata <- function(panel, x, y) {
   panel$x <- c(x, panel$x)
	panel$y <- c(y, panel$y)
	rp.control.put(panel$panelname, panel)
	rp.tkrreplot(panel, tkrp)
	panel$x <- panel$x[-1]
	panel$y <- panel$y[-1]
	panel
   }
plotolddata <- function(panel, x, y) {
   rp.tkrreplot(panel, tkrp)
   panel
   }
redraw <- function(panel) {
   rp.tkrreplot(panel, tkrp)
   panel
   }
   
x <- runif(20, 0, 1000)
y <- x + rnorm(20, sd = 50)
rpplot <- rp.control(title = "Demonstration of click and drag", x = x, y = y,
                     pos = "top")
rp.tkrplot(rpplot, tkrp, plotdata, 
      action = plotnewdata, mousedrag = plotnewdata, mouseup = plotolddata)

#  Simple slider

draw  <- function(panel) {
   plot(seq(0, 1, length = 10)^panel$pow)
   panel
}
click <- function(panel, x, y) {
   print(c(x, y))
   panel
}
redraw <- function(panel) {
   rp.tkrreplot(panel, plot1)
   panel
}
panel <- rp.control(pow = 1)
rp.tkrplot(panel, plot1, draw, click, hscale = 1.5, vscale = 1.5)
rp.slider(panel, pow, from = 0.5, to = 2, action = redraw)

# rp.plot4d

with(SO2, {
      model <- mgcv::gam(logSO2 ~ s(longitude, latitude, year))
      loc1  <- seq(min(longitude), max(longitude), length = 30)
      loc2  <- seq(min(latitude), max(latitude), length = 30)
      yr    <- seq(min(year), max(year), length = 30)
      newdata <- expand.grid(loc1, loc2, yr)
      names(newdata) <- c("longitude", "latitude", "year")
      model <- predict(model, newdata)
      model <- list(x = cbind(loc1, loc2), z = yr,
                    y = array(model, dim = rep(30, 3)))
      mapxy <- maps::map('world', plot = FALSE,
                   xlim = range(longitude), ylim = range(latitude))
      location <- cbind(longitude, latitude)
      rp.plot4d(location, year, logSO2, model,
                col.palette = rev(heat.colors(20)),
                foreground.plot = function() maps::map(mapxy, add = TRUE))
})

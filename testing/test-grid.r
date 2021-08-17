#     Test rp.grid etc.

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

density.draw <- function(panel) {
  plot(density(panel$x, bw = panel$h))
  panel
}
panel <- rp.control(x = rnorm(50), pars = c(5, 10, 3))
rp.grid(panel,"bluegrid",background="navy")
rp.doublebutton(panel, var = h, step = 0.05,
  title = "Density estimate", action = density.draw,
  range = c(0.1, 5), initval=1, column=0, row=0, grid="bluegrid")
rp.doublebutton(panel, var = h, step = 0.05,
  title = "Density estimate", action = density.draw,
  range = c(0.1, 5), initval=1, column=1, row=1, grid="bluegrid")
gulls.click <- function(panel, x, y) {
  print(c(x, y))
  panel
}
image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
rp.image(panel, image.file, id = "gulls.image", action = gulls.click, grid="bluegrid", row=2, column=0, columnspan=2)

data.plotfn <- function(panel) {
  if (panel$plot.type == "histogram")
    hist(panel$x)
  else
    if (panel$plot.type == "boxplot")
      boxplot(panel$x)
    else
      plot(density(panel$x))
    panel
}

rp.listbox(panel, plot.type,
  vals=c("histogram", "boxplot", "density estimate"),
  action = data.plotfn, title = "Plot type", grid="bluegrid", row=0,column=1)

rp.radiogroup(panel, plot.type,
  c("histogram", "boxplot", "density estimate"),
  action = data.plotfn, title = "Plot type", grid="bluegrid", row=1, column=0)

rp.slider(panel, h, 0.5, 5, log = TRUE, action = density.draw, grid="bluegrid", row=3, column=0, columnspan=2,
horizontal=TRUE)

rp.slider(panel, h, 0.5, 5, log = TRUE, action = density.draw, grid="bluegrid", rowspan=3, row=0, column=2, horizontal=FALSE)

plotf <- function(panel) {
    with(panel, {
         pars   <- as.numeric(pars)
         xgrid <- seq(0.1, max(c(pars[3], 5), na.rm = TRUE),  
                        length = 50)
         dgrid <- df(xgrid, pars[1], pars[2])
         plot(xgrid, dgrid, type = "l", col = "blue", lwd = 3)
         	print(pars)
         if (!is.na(pars[3])) {
            lines(rep(pars[3], 2), c(0, 0.95 * max(dgrid)), 
                   lty = 2, col = "red")
            text(pars[3], max(dgrid), as.character(pars[3]), 
                   col = "red")
         }
     })
     panel
}

rp.textentry(panel, pars, plotf, 
      labels = c("df1", "df2", "observed"),
      initval = c("10", "5", "3"), grid="bluegrid", row=3, column=2)

redraw <- function(panel) {
   rp.tkrreplot(panel, tkrp)
   panel
}

fn <- function(panel) {
	plot(1:20, (1:20)^panel$h)
	panel
}
rp.tkrplot(panel, tkrp, fn,
   grid="bluegrid", column=4, row=0, rowspan=3)

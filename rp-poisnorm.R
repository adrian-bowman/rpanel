mn <- 5

draw.poisnorm <- function(panel) {
	with(panel, {
		xgrid <- seq(xlim[1], xlim[2], length = 100)
		x     <- xlim[1]:xlim[2]
		y     <- dpois(x, mn)
		plot(x, y, type = "n")
		segments(x, 0, x, y, lwd = 2, col = "red")
		if (display) lines(xgrid, dnorm(xgrid, mn, sqrt(mn)), lwd = 2, col = "blue")
	})
	panel
}

redraw.poisnorm <- function(panel) {
	xlim1      <- max(0, panel$mn - round(3 * sqrt(panel$mn)))
	xlim2      <- panel$mn + round(3 * sqrt(panel$mn))
	panel$xlim <- c(min(xlim1, panel$xlim[1]), max(xlim2, panel$xlim[2]))
	rp.control.put(panel$panelname, panel)
	rp.tkrreplot(panel, plot)
	panel
}

rescale.poisnorm <- function(panel) {
	print(panel$xlim)
	xlim1      <- max(0, panel$mn - round(3 * sqrt(panel$mn)))
	xlim2      <- panel$mn + round(3 * sqrt(panel$mn))
	panel$xlim <- c(xlim1, xlim2)
	rp.control.put(panel$panelname, panel)
	rp.tkrreplot(panel, plot)
	print(panel$xlim)
	panel
}

panel <- rp.control(mn = mn, display = FALSE,
										xlim = c(max(0, mn - round(3 * sqrt(mn))), mn + round(3 * sqrt(mn))))
rp.tkrplot(panel, plot, draw.poisnorm, pos = "right")
rp.slider(panel, mn, 0.1, 30, redraw.poisnorm, "mean", showvalue = TRUE)
rp.checkbox(panel, display, redraw.poisnorm, "normal approximation")
rp.button(panel, rescale.poisnorm, "rescale")

#     Dounreay: simulation tool for quality control

# SPC demo
# 
# Repeated control sampling to show variability.
# Control limits: 3 and 2.
# Marginal distribution.
# Limit at 2sd: blank limit, detection limit (power).
# Limit at 3sd: not so many false alarms.
# Trend: 5 + 0.2 * x, limits greatly increased.
# Monitored data: variability as in control.
# Trend: 6 (few alarms), 7 (more).
# Trend: 5 + 0.04 * x

# detach(package:rpanel)
# unloadNamespace("rpanel")
library(rpanel)

n.b          <- 30
n.s          <- 100
std.b        <- 1
std.s        <- 1
trend.b      <- "5"
trend.s      <- "5"
ylab         <- "Value"
hscale       <- 0.9
ggplot.style <- TRUE

if (ggplot.style) {
  plot.bgd <- grey(0.6)
  axis.lwd <- 0
  col.b    <- "lightblue"
} else {
	plot.bgd <- grey(0.1)
	axis.lwd <- 1
	col.b    <- "blue"
}

draw.b <- function(panel) {
	with(panel, {
		par(mar = c(3, 3, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
		plot(x.b, y.b, type = "n", ylim = ylim, axes = FALSE, xlab = "Time", ylab = ylab)
		usr <- par("usr")
		rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
		grid(col = "white", lty = 1)
		axis(1, lwd = axis.lwd, lwd.ticks = 2, col = plot.bgd, col.ticks = plot.bgd,
				 col.axis = plot.bgd, cex.axis = 0.8)
		axis(2, lwd = axis.lwd, lwd.ticks = 2, col = plot.bgd, col.ticks = plot.bgd,
				 col.axis = plot.bgd, cex.axis = 0.8)
		if (Display.b["trend"]) lines(x.b, trnd.b, lwd = 2, col = "blue")
		if (Display.b["control limits"]) abline(h = limits, lwd = 2, col = "darkgreen")
		clr <- rep(col.b, n.b)
		if (Display.b["control limits"]) clr[y.b > limits[2]] <- "red"
		if (!first.b) points(x.b, y.b, pch = 16, col = clr)
		title("Control sample")
	})
	panel
}

draw.s <- function(panel) {
	with(panel, {
		par(mar = c(3, 0, 1, 3) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
		plot(x.s, y.s, type = "n", ylim = ylim, axes = FALSE, xlab = "Time", ylab = "Value")
		usr <- par("usr")
		rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
		grid(col = "white", lty = 1)
		axis(1, lwd = axis.lwd, lwd.ticks = 2, col = plot.bgd, col.ticks = plot.bgd,
				 col.axis = plot.bgd, cex.axis = 0.8)
		axis(4, lwd = axis.lwd, lwd.ticks = 2, col = plot.bgd, col.ticks = plot.bgd,
				 col.axis = plot.bgd, cex.axis = 0.8)
		if (Display.s["trend"]) lines(x.s, trnd.s, lwd = 2, col = "blue")
		if (Display.s["control limits"]) abline(h = limits, lwd = 2, col = "darkgreen")
		clr <- rep("darkblue", n.s)
		if (Display.s["control limits"]) clr[y.s > limits[2]] <- "red"
		if (!first.s) points(x.s, y.s, pch = 16, col = clr)
		mtext(ylab, 4, 1.5)
		title("Monitored sample")
	})
	panel
}

norm.b <- function(panel) {
	with(panel, {
		xgrid    <- seq(ylim[1], ylim[2], length =100)
		ygrid    <- dnorm(xgrid, mean(trnd.b), std.b)
		col.norm <- if (Display.b["marginal distribution"]) col.b else "white"
		par(mar = c(3, 0, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
		plot(ygrid, xgrid, type = "n", ylim = ylim, axes = FALSE, xlab = "")
		polygon(c(ygrid, ygrid[1]), c(xgrid, xgrid[1]), col = col.norm, border = NA)
		if (Display.b["control limits"] & Display.b["marginal distribution"]) {
			ind <- which(xgrid > limits[2])
			polygon(c(dnorm(limits[2], mean(trnd.b), std.b), ygrid[ind], ygrid[1]),
							c(limits[2], xgrid[ind], limits[2]), col = "red", border = NA)
			abline(h = limits, lwd = 2, col = "darkgreen")
		}
	})
	panel
}

norm.s <- function(panel) {
	with(panel, {
		xgrid    <- seq(ylim[1], ylim[2], length =100)
		ygrid    <- dnorm(xgrid, mean(trnd.s), std.s)
		col.norm <- if (Display.s["marginal distribution"]) "darkblue" else "white"
		par(mar = c(3, 0, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
		plot(ygrid, xgrid, type = "n", ylim = ylim, axes = FALSE, xlab = "")
		polygon(c(ygrid, ygrid[1]), c(xgrid, xgrid[1]), col = col.norm, border = NA)
		if (Display.s["control limits"] & Display.s["marginal distribution"]) {
			ind <- which(xgrid > limits[2])
	  	polygon(c(dnorm(limits[2], mean(trnd.b), std.b), ygrid[ind], ygrid[1]),
	  					c(limits[2], xgrid[ind], limits[2]), col = "red", border = NA)
		  abline(h = limits, lwd = 2, col = "darkgreen")
		}
	})
	panel
}

trend.fn <- function(panel) {
	panel$n.b      <- as.numeric(panel$tentry.b[1])
	panel$n.s      <- as.numeric(panel$tentry.s[1])
	panel$x.b      <- 1:panel$n.b
	panel$x.s      <- 1:panel$n.s
	trend.fb       <- eval(parse(text = paste("function(x)", panel$tentry.b[3])))
	trend.fs       <- eval(parse(text = paste("function(x)", panel$tentry.s[3])))
	panel$trnd.b   <- trend.fb(panel$x.b)
	panel$trnd.s   <- trend.fs(panel$x.s)
	panel$trnd.b   <- if (length(panel$trnd.b) == 1) rep(panel$trnd.b, length(panel$x.b))
	                  else panel$trnd.b
	panel$trnd.s   <- if (length(panel$trnd.s) == 1) rep(panel$trnd.s, length(panel$x.s))
	                  else panel$trnd.s
	panel$mean.b   <- mean(panel$trnd.b)
	panel$mean.s   <- mean(panel$trnd.s)
	panel$sd.b     <- as.numeric(panel$tentry.b[2])
	panel$sd.s     <- as.numeric(panel$tentry.s[2])
	panel$std.b    <- sqrt(panel$sd.b^2 + mean((panel$trnd.b - panel$mean.b)^2))
	panel$std.s    <- sqrt(panel$sd.s^2 + mean((panel$trnd.s - panel$mean.s)^2))
	panel$ylim     <- c(min(c(panel$trnd.b - 3.5 * panel$std.b, panel$trnd.s - 3.5 * panel$std.s)),
							   		 max(c(panel$trnd.b + 3.5 * panel$std.b, panel$trnd.s + 3.5 * panel$std.s)))
	panel$sd.limit <- as.numeric(panel$tentry.b[4])
	panel$limits   <- c(panel$mean.b - panel$sd.limit * panel$std.b, panel$mean.b + panel$sd.limit * panel$std.b)
  panel
}
	
sim.b <- function(panel) {
	panel$y.b   <- panel$trnd.b + rnorm(panel$n.b, sd = panel$sd.b)
	rp.control.put(panel$panelname, panel)
	if (!panel$first.b) {
		rp.tkrreplot(panel, plot.b)
		rp.tkrreplot(panel, marg.b)
	}
	panel
}

button.b <- function(panel) {
	panel$first.b <- FALSE
	panel <- sim.b(panel)
	panel
}

button.s <- function(panel) {
	panel$first.s <- FALSE
	panel <- sim.s(panel)
	panel
}

sim.s <- function(panel) {
	panel$y.s   <- panel$trnd.s + rnorm(panel$n.s, sd = panel$sd.s)
	rp.control.put(panel$panelname, panel)
	if (!panel$first.s) {
		rp.tkrreplot(panel, plot.s)
		rp.tkrreplot(panel, marg.s)
	}
	panel
}

pars.b <- function(panel) {
	ylim.old <- panel$ylim
	panel    <- trend.fn(panel)
	if (panel$sd.limit == panel$sd.limit.old)
		panel  <- sim.b(panel)
	else {
		rp.control.put(panel$panelname, panel)
		rp.tkrreplot(panel, plot.b)
	  rp.tkrreplot(panel, marg.b)
	}
	if (any(panel$ylim != ylim.old) | (panel$sd.limit != panel$sd.limit.old)) {
	  rp.tkrreplot(panel, plot.s)
	  rp.tkrreplot(panel, marg.s)
	}
	panel$sd.limit.old <- panel$sd.limit
	panel
}

pars.s <- function(panel) {
	ylim.old <- panel$ylim
	panel <- trend.fn(panel)
	panel <- sim.s(panel)
	if (any(panel$ylim != ylim.old)) {
		rp.tkrreplot(panel, plot.b)
		rp.tkrreplot(panel, marg.b)
	}
	panel
}

redraw.b <- function(panel) {
	rp.tkrreplot(panel, plot.b)
	rp.tkrreplot(panel, marg.b)
	panel
}

redraw.s <- function(panel) {
	rp.tkrreplot(panel, plot.s)
	rp.tkrreplot(panel, marg.s)
	panel
}

panel <- rp.control(n.b = n.b, n.s = n.s, first.b = TRUE, first.s = TRUE, ylab = ylab,
										std.b = std.b, std.s = std.s, sd.limit = 3, sd.limit.old = 3,
										trnd.b = trend.b, trnd.s = trend.s,
										plot.bgd = plot.bgd, axis.lwd = axis.lwd, col.b = col.b,
										tentry.b = c(n.b, std.b, trend.b, 3), tentry.s = c(n.s, std.s, trend.s, 3),
                    Display.b = c("trend" = FALSE, "marginal distribution" = FALSE, "control limits" = FALSE),
                    Display.s = c("trend" = FALSE, "marginal distribution" = FALSE, "control limits" = FALSE))
rp.grid(panel, "controls.b", row = 0, column = 0, sticky = "n")
rp.grid(panel, "controls.s", row = 0, column = 3, sticky = "n")
rp.grid(panel, "plot.b",     row = 1, column = 0, background = "white")
rp.grid(panel, "marg.b",     row = 1, column = 1, background = "white")
rp.grid(panel, "marg.s",     row = 1, column = 2, background = "white")
rp.grid(panel, "plot.s",     row = 1, column = 3, background = "white")
rp.grid(panel, "controls.b1", parentname = "controls.b", row = 0, column = 1, background = "white")
rp.grid(panel, "controls.s1", parentname = "controls.s", row = 0, column = 1, background = "white")
rp.do(panel, trend.fn)
rp.do(panel, sim.b)
rp.do(panel, sim.s)
rp.tkrplot(panel, plot.b, draw.b, hscale = hscale, vscale = hscale,
					 grid = "plot.b", row = 0, column = 0, background = "white")
rp.tkrplot(panel, plot.s, draw.s, hscale = hscale, vscale = hscale,
					 grid = "plot.s", row = 0, column = 0, background = "white")
rp.tkrplot(panel, marg.b, norm.b, hscale = 0.15 * hscale, vscale = hscale,
					 grid = "marg.b", row = 0, column = 0, background = "white")
rp.tkrplot(panel, marg.s, norm.s, hscale = 0.15 * hscale, vscale = hscale,
					 grid = "marg.s", row = 0, column = 0, background = "white")

rp.textentry(panel, tentry.b, pars.b, labels = c("n", "sd", "trend", "sd limit"),
						 title = "Settings", grid = "controls.b", row = 0, column = 0)
rp.checkbox(panel, Display.b, redraw.b, c("trend", "marginal distribution", "control limits"),
						 title = "Display", grid = "controls.b1", row = 0, column = 0)
rp.button(panel, button.b, "New sample", grid = "controls.b1", row = 1, column = 0)

rp.textentry(panel, tentry.s, pars.s, labels = c("n", "sd", "trend"), title = "Settings",
						 grid = "controls.s", row = 0, column = 0)
rp.checkbox(panel, Display.s, redraw.s, c("trend", "marginal distribution", "control limits"),
						 title = "Display", grid = "controls.s1", row = 0, column = 0)
rp.button(panel, button.s, "New sample", grid = "controls.s1", row = 1, column = 0)

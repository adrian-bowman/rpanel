rp.sample <- function(mu = 0, sigma = 1, n = 25,
                      ggplot = TRUE, display = 'histogram',
                      panel.plot = TRUE, hscale = NA, vscale = hscale, pause = 0.01) {

   if (ggplot & !requireNamespace('ggplot2', quietly = TRUE)) {
      ggplot <- FALSE
      message('the ggplot package is not available - reverting to standard graphics.')
   }
   if (!(display %in% c('histogram', 'density', 'violin'))) {
      display <- 'histogram'
      message('display not recognised - using histogram.')
   }
      
   if (!ggplot)
      return(rp.sample.old(mu = 0, sigma = 1, n = 25,
                    panel.plot = TRUE, hscale = NA, vscale = hscale))
   
   sample.draw.ggplot <- function(panel) {
      
      mu       <- panel$pars["mu"]
      sigma    <- panel$pars["sigma"]
      n        <- panel$pars["n"]
      col.pars <- "green3"
      
      with(panel, {
         
         if (display == 'histogram') {
            plt  <- ggplot2::ggplot(data.frame(x = y), ggplot2::aes(x)) +
               ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1 / sigma))
            if (display.sample["data"]) {
               brks <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 20)
               plt  <- plt +
                  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                          breaks = brks, col = 'grey90', fill = 'grey50')
            }
            # Mark observations outside the scale
            # ind <- which(hst$density > usr[4])
            # if (length(ind) > 0)
            #    segments(hst$breaks[ind], usr[4], hst$breaks[ind + 1], usr[4], col = "red", lwd = 3)
            # ind <- any(hst$density > 0 & hst$breaks[-1] < usr[1])
            # if (ind) segments(usr[1], 0, usr[1], usr[4], col = "red", lwd = 3)
            # ind <- any(hst$density > 0 & hst$breaks[-length(hst$breaks)] > usr[2])
            # if (ind) segments(usr[2], 0, usr[2], usr[4], col = "red", lwd = 3)
            if (display.sample["population"])
               plt <- plt + ggplot2::geom_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                                                   col = 'blue')
            if (display.sample["mean"])
               plt <- plt + ggplot2::geom_segment(x = mu, y = 0.9 / sigma, yend = 0,
                                                col = col.pars)
            if (display.sample["+/- 2 st.dev."])
               plt <- plt + ggplot2::geom_segment(x = mu - 2 * sigma, xend = mu + 2 * sigma,
                                                  y = 0.8 / sigma, col = col.pars,
                                                  arrow = ggplot2::arrow(ends = 'both',
                                                          length = grid::unit(0.1, "inches")))
         }

         if (display == 'density') {
            plt <- ggplot2::ggplot(data.frame(x = y, y = 1), ggplot2::aes(x, y)) +
               ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 2 * dmax))
            if (display.sample["data"]) {
               sz <- 1
               if (length(panel$y) >= panel$nmin) {
                  plt <- plt +
                     ggplot2::geom_area(ggplot2::aes(x = xgrid, y = dgrid),
                                        data = d.dens, col = 'grey75', fill = 'grey75')
                  sz <- 0.2
               }
               plt <- plt +
                  ggplot2::geom_point(ggplot2::aes(x, y), data = d.densd, size = sz)
            }
            if (display.sample["population"]) {
               xgrid <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
               pgrid <- dnorm(xgrid, mu, sigma)
               d.pop <- data.frame(xgrid, pgrid)
               plt <- plt +
                  ggplot2::geom_line(ggplot2::aes(xgrid, pgrid), data = d.pop, col = 'blue')
            }
            if (display.sample["mean"])
               plt <- plt + ggplot2::geom_segment(x = mu, y = 0, yend = 1.75 * dmax,
                                                  col = col.pars)
            if (display.sample["+/- 2 st.dev."])
               plt <- plt + ggplot2::geom_segment(x = mu - 2 * sigma, xend = mu + 2 * sigma,
                                                  y = 1.5 * dmax, col = col.pars,
                                                  arrow = ggplot2::arrow(ends = 'both',
                                                           length = grid::unit(0.1, "inches")))
         }
         
         if (display == 'violin') {
            plt <- ggplot2::ggplot(data.frame(x = y, y = 1), ggplot2::aes(x, y)) +
               ggplot2::ylim(1 - 2 * dmax, 1 + 2 * dmax)
            if (display.sample["data"]) {
               if (length(panel$y) >= panel$nmin)
                  plt <- plt +
                     ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 1,
                                                       ymin = 1 - dgrid, ymax = 1 + dgrid),
                                       data = d.dens, col = 'grey75', fill = 'grey75')
                  plt <- plt +
                     ggplot2::geom_point(ggplot2::aes(x, y), data = d.densv, size = 0.2)
            }
            if (display.sample["population"]) {
               xgrid <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
               pgrid <- dnorm(xgrid, mu, sigma)
               d.pop <- data.frame(xgrid, pgrid)
               plt <- plt +
                  ggplot2::geom_line(ggplot2::aes(xgrid, 1 + pgrid), data = d.pop, col = 'blue') +
                  ggplot2::geom_line(ggplot2::aes(xgrid, 1 - pgrid), data = d.pop, col = 'blue')
            }
            if (display.sample["mean"])
               plt <- plt + ggplot2::geom_segment(x = mu, y = 1 + 1.75 * dmax,
                                                  yend = 1 - 1.75 * dmax, col = col.pars)
            if (display.sample["+/- 2 st.dev."])
               plt <- plt + ggplot2::geom_segment(x = mu - 2 * sigma, xend = mu + 2 * sigma,
                                                  y = 1 + 1.5 * dmax, col = col.pars,
                                                  arrow = ggplot2::arrow(ends = 'both',
                                                         length = grid::unit(0.1, "inches")))
         }
         
         plt <- plt +
            ggplot2::xlim(mu - 3 * sigma, mu + 3 * sigma) +
            ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                           axis.text.y  = ggplot2::element_blank(),
                           axis.ticks.y = ggplot2::element_blank())
         print(plt)
      })
      
      panel
   }
   
   samplemean.draw.ggplot <- function(panel) {
      
      mu       <- panel$pars["mu"]
      sigma    <- panel$pars["sigma"]
      n        <- panel$pars["n"]
      col.pars <- "green3"
      
      with(panel, {
         if (any(display.mean)) {
            
            if (display == 'histogram') {
               plt <- ggplot2::ggplot(data.frame(x = mns, y = 1), ggplot2::aes(x, y)) +
                  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, sqrt(n) / sigma))
               brks <- seq(mu - 3 * sigma / sqrt(n), mu + 3 * sigma / sqrt(n), length = 20)
            
               # hst <- hist(mns, plot = FALSE,
               #             breaks = seq(mu - 3 * sigma, mu + 3 * sigma, length = 50))
               # plot(mu + c(-3, 3) * sigma, c(0, sqrt(n) / sigma),
               #      type = "n", axes = FALSE, xlab = "Sample mean", ylab = "")
               # usr <- par("usr")
               # rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
               # grid(col = "white", lty = 1)
               # axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
               #      col.axis = grey(0.6), cex.axis = 0.8)
            
               if (display.mean["sample mean"]) {
                  plt <- plt + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                              breaks = brks, col = 'grey90', fill = 'grey50')
               }
               if (display.sample["mean"])
                  plt <- plt + ggplot2::geom_segment(y = 0.9 * sqrt(n) / sigma, yend = 0,
                                            x = mu, col = col.pars)
               if (display.mean["+/- 2 se"])
                  plt <- plt + ggplot2::geom_segment(x    = mu - 2 * sigma / sqrt(n),
                                            xend = mu + 2 * sigma / sqrt(n),
                                            y = 0.8 * sqrt(n) / sigma, col = col.pars,
                                            arrow = ggplot2::arrow(ends = 'both',
                                                                length = grid::unit(0.1, "inches")))
               if (display.mean["distribution"])
                  plt <- plt + ggplot2::geom_function(fun = dnorm,
                                             args = list(mean = mu, sd = sigma / sqrt(n)),
                                             col = 'blue')
            }
            
            if (display == 'density') {
               plt <- ggplot2::ggplot(data.frame(x = mns, y = 1), ggplot2::aes(x, y)) +
                  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0,2 * dmax * sqrt(n)))
               if (display.mean["sample mean"]) {
                  sz <- 1
                  if (length(panel$mns) >= panel$nmin) {
                     plt <- plt +
                        ggplot2::geom_area(ggplot2::aes(x = xgrid, y = dgrid),
                                             data = d.mdens, col = 'grey75', fill = 'grey75')
                     sz <- 0.2
                  }
                  plt <- plt +
                     ggplot2::geom_point(ggplot2::aes(x, y), data = d.mdensd, size = sz)
               }
               if (display.mean["distribution"]) {
                  xgrid <- seq(mu - 3 * sigma / sqrt(n), mu + 3 * sigma / sqrt(n), length = 200)
                  pgrid <- dnorm(xgrid, mu, sigma / sqrt(n))
                  d.pop <- data.frame(xgrid, pgrid)
                  plt <- plt +
                     ggplot2::geom_area(ggplot2::aes(x = xgrid, y = pgrid),
                                          data = d.pop, col = '#86B875', fill = '#86B875',
                                          alpha = 0.25) 
                  # ggplot2::geom_line(ggplot2::aes(xgrid, 1 + pgrid), data = d.pop, col = 'blue') +
                  # ggplot2::geom_line(ggplot2::aes(xgrid, 1 - pgrid), data = d.pop, col = 'blue')
               }
               if (display.sample["mean"])
                  plt <- plt + ggplot2::geom_segment(x = mu, y = 1.75 * dmmax,
                                                     yend = 0, col = col.pars)
               if (display.mean["+/- 2 se"])
                  plt <- plt + ggplot2::geom_segment(x    = mu - 2 * sigma / sqrt(n),
                                                     xend = mu + 2 * sigma / sqrt(n),
                                                     y = 1.5 * dmmax, col = col.pars,
                                                     arrow = ggplot2::arrow(ends = 'both',
                                                                            length = grid::unit(0.1, "inches")))
            }
            
            if (display == 'violin') {
               plt <- ggplot2::ggplot(data.frame(x = mns, y = 1), ggplot2::aes(x, y)) +
                  ggplot2::ylim(1 - 2 * dmax * sqrt(n), 1 + 2 * dmax * sqrt(n))
               if (display.mean["sample mean"]) {
                  if (length(panel$mns) >= panel$nmin) {
                     plt <- plt +
                        ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 1, ymin = 1 - dgrid, ymax = 1 + dgrid),
                                             data = d.mdens, col = 'grey75', fill = 'grey75')
                  }
                  plt <- plt +
                     ggplot2::geom_point(ggplot2::aes(x, y), data = d.mdensv, size = 0.2)
               }
               if (display.mean["distribution"]) {
                  xgrid <- seq(mu - 3 * sigma / sqrt(n), mu + 3 * sigma / sqrt(n), length = 200)
                  pgrid <- dnorm(xgrid, mu, sigma / sqrt(n))
                  d.pop <- data.frame(xgrid, pgrid)
                  plt <- plt +
                     ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 1,
                                                       ymin = 1 - pgrid, ymax = 1 + pgrid),
                                          data = d.pop, col = '#86B875', fill = '#86B875',
                                          alpha = 0.25) 
                  # ggplot2::geom_line(ggplot2::aes(xgrid, 1 + pgrid), data = d.pop, col = 'blue') +
                  # ggplot2::geom_line(ggplot2::aes(xgrid, 1 - pgrid), data = d.pop, col = 'blue')
               }
               if (display.sample["mean"])
                  plt <- plt + ggplot2::geom_segment(x = mu, y = 1 + 1.75 * dmmax,
                                                     yend = 1 - 1.75 * dmmax, col = col.pars)
               if (display.mean["+/- 2 se"])
                  plt <- plt + ggplot2::geom_segment(x    = mu - 2 * sigma / sqrt(n),
                                                     xend = mu + 2 * sigma / sqrt(n),
                                                     y = 1 + 1.5 * dmmax, col = col.pars,
                                                     arrow = ggplot2::arrow(ends = 'both',
                                                                            length = grid::unit(0.1, "inches")))
            }
            
            sb  <- if (!zoom) sigma else sigma / sqrt(n)
            plt <- plt + ggplot2::xlim(mu - 3 * sb, mu + 3 * sb) +
                         ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                        axis.text.y  = ggplot2::element_blank(),
                                        axis.ticks.y = ggplot2::element_blank())
         }
         else
            plt <- ggplot2::ggplot() +
               ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white', colour = 'white'))
         print(plt)
      })
      
      panel
   }
   
   sample.redraw <- function(panel) {
      rp.tkrreplot(panel, plotdata)
      rp.tkrreplot(panel, plotmean)
      panel
   }
   
   sample.changepars <- function(panel) {
      panel$mns <- NULL
      rp.control.put(panel$panelname, panel)
      rp.do(panel, sample.new)
      panel
   }

   sample.new <- function(panel) {
      mu            <- panel$pars["mu"]
      sigma         <- panel$pars["sigma"]
      panel$y       <- rnorm(panel$pars["n"], mu, sigma)
      xgrid         <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
      bw.norm       <- function(x) sd(x) * (4/(3 * length(x)))^0.2
      dens          <- density(panel$y, bw = bw.norm(panel$y))
      dens.y        <- if (length(panel$y) >= panel$nmin) approx(dens$x, dens$y, xout = panel$y)$y
                       else dnorm(panel$y, mu, sigma)
      panel$d.dens  <- data.frame(xgrid = dens$x, dgrid = dens$y)
      panel$d.densd <- data.frame(x = panel$y, y = dens.y * runif(length(dens.y), 0, 1))
      panel$d.densv <- data.frame(x = panel$y, y = 1 + dens.y * runif(length(dens.y), -1, 1))
      if (!panel$display.mean["accumulate"]) panel$mns <- NULL
      panel$mns <- c(mean(panel$y), panel$mns)
      if (length(panel$mns) >= panel$nmin) {
         dens           <- density(panel$mns, bw.norm(panel$mns))
         dens.y         <- approx(dens$x, dens$y, xout = panel$mns)$y
         panel$d.mdens  <- data.frame(xgrid = dens$x, dgrid = dens$y)
      }
      else
         dens.y         <- dnorm(panel$mns, mu, sigma / sqrt(n))
      panel$d.mdensd <- data.frame(x = panel$mns, y = dens.y * runif(length(dens.y)), 0, 1)
      panel$d.mdensv <- data.frame(x = panel$mns, y = 1 + dens.y * runif(length(dens.y), -1, 1))
      rp.control.put(panel$panelname, panel)
      if (panel$pplot) {
         rp.tkrreplot(panel, plotdata)
         rp.tkrreplot(panel, plotmean)
      }
      else
         sample.draw(panel)
      panel
   }

   if (is.na(hscale)) hscale <- 1
   if (is.na(vscale)) vscale <- hscale
   
   display.sample        <- c(TRUE, rep(FALSE, 3))
   display.mean          <- rep(FALSE, 4)
   names(display.sample) <- c("data", "population", "mean", "+/- 2 st.dev.")
   names(display.mean)   <- c("sample mean", "accumulate", "+/- 2 se", "distribution")
   pars                  <- c(mu, sigma, n)
   names(pars)           <- c("mu", "sigma", "n")
   y                     <- rnorm(n, mu, sigma)
   dmax                  <- dnorm(mu, mu, sigma)
   dmmax                 <- dnorm(mu, mu, sigma / sqrt(n))
   xgrid                 <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
   dens                  <- density(y)
   dens.y                <- approx(dens$x, dens$y, xout = y)$y
   d.dens                <- data.frame(xgrid = dens$x, dgrid = dens$y)
   d.densd               <- data.frame(x = y, y = dens.y * runif(length(dens.y), 0, 1))
   d.densv               <- data.frame(x = y, y = 1 + dens.y * runif(length(dens.y), -1, 1))
   mns                   <- mean(y)
   d.mdensd              <- data.frame(x = mns, y = dens.y * runif(length(dens.y)), 0, 1)
   d.mdensv              <- data.frame(x = mns, y = 1 + dens.y * runif(length(dens.y), -1, 1))
   
   panel <- rp.control(pars = pars, y = y, mns = mns, nmin = 10,
                       d.dens = d.dens, d.densd = d.densd, d.densv = d.densv,
                       d.mdensd = d.mdensd, d.mdensv = d.mdensv, dmax = dmax, dmmax = dmmax,
                       display.sample = display.sample, display.mean = display.mean,
                       pplot = panel.plot)
   Sys.sleep(pause)
   rp.grid(panel, "controls", row = 0, column = 0)
   Sys.sleep(pause)
   rp.grid(panel, "plots",    row = 0, column = 1, background = "white")
   Sys.sleep(pause)
   
   if (panel.plot) {
      rp.tkrplot(panel, plotdata, sample.draw.ggplot,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "plots", row = 0, column = 0, background = "white")
      Sys.sleep(pause)
      rp.tkrplot(panel, plotmean, samplemean.draw.ggplot,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "plots", row = 1, column = 0, background = "white")
      action.fn <- sample.redraw
   }
   else {
      action.fn <- sample.draw
      rp.do(panel, action.fn)
   }
   rp.textentry(panel, pars, sample.changepars, width = 10, title = 'Parameters',
               c("mean", "st.dev.", "sample size"), c("mu", "sigma", "n"),
               grid = "controls", row = 0, column = 0, sticky = "ew")
   Sys.sleep(pause)
   rp.radiogroup(panel, display, c('histogram', 'density', 'violin'), title = 'Display',
               action = action.fn,
               grid = "controls", row = 1, column = 0, sticky = "ew")
   Sys.sleep(pause)
   rp.button(panel, sample.new, "Sample", repeatinterval = 1, repeatdelay = 1,
             grid = "controls", row = 2, column = 0, sticky = "ew")
   Sys.sleep(pause)
   rp.checkbox(panel, display.sample, action.fn, names(display.sample),
               title = "Sample",
               grid = "controls", row = 3, column = 0, sticky = "ew")
   Sys.sleep(pause)
   rp.checkbox(panel, display.mean,   action.fn, names(display.mean),
               title = "Sample mean",
               grid = "controls", row = 4, column = 0, sticky = "ew")
   Sys.sleep(pause)
   rp.checkbox(panel, zoom, action.fn, labels = "zoom in",
               grid = "controls", row = 5, column = 0, sticky = "ew")

   invisible()
}

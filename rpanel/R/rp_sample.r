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
   
   sample.draw <- function(panel) {
      
      mu       <- panel$pars["mu"]
      n        <- panel$pars["n"]
      col.pars <- "green3"
      col.pars <- '#86B875'
      col.pars <- 'black'
      col.pars <- 'darkblue'
      col.dens <- 'grey75'
      
      with(panel, {
         
         if (plot.mean & !any(display.mean)) {
            plt <- ggplot2::ggplot() +
                   ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white',
                                                                           colour = 'white'))
            print(plt)
            return(panel)
         }

         y        <- if (plot.mean) mns else ydata
         dmax     <- if (plot.mean) dmax.mean else dmax.data
         stdev    <- if (plot.mean) panel$pars["sigma"] / sqrt(panel$pars['n'])
                     else panel$pars["sigma"]
         df.dens  <- if (plot.mean) d.mdens  else d.dens
         df.densd <- if (plot.mean) d.mdensd else d.densd
         orig <- if (display == 'violin') 0.7 * dmax else 0
         scl  <- if (display == 'violin') 0.5 else 1
         
         plt <- ggplot2::ggplot(data.frame(x = y, y = 0), ggplot2::aes(x, y)) +
            ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 2 * dmax))
         
         if ((!plot.mean && display.sample['data']) | (plot.mean && display.mean['sample mean'])) {
            
            if (display == 'histogram') {
               brks <- seq(mu - 3 * stdev, mu + 3 * stdev, length = 20)
               plt  <- plt +
                  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                          breaks = brks, col = 'grey50', fill = col.dens)
            # Mark observations outside the scale
            # ind <- which(hst$density > usr[4])
            # if (length(ind) > 0)
            #    segments(hst$breaks[ind], usr[4], hst$breaks[ind + 1], usr[4], col = "red", lwd = 3)
            # ind <- any(hst$density > 0 & hst$breaks[-1] < usr[1])
            # if (ind) segments(usr[1], 0, usr[1], usr[4], col = "red", lwd = 3)
            # ind <- any(hst$density > 0 & hst$breaks[-length(hst$breaks)] > usr[2])
            # if (ind) segments(usr[2], 0, usr[2], usr[4], col = "red", lwd = 3)

            # if (display.sample["population"])
            #    plt <- plt + ggplot2::geom_function(fun = dnorm, args = list(mean = mu, sd = sigma),
            #                                        linewidth = 1, col = col.pars)
               # plt <- plt + ggplot2::geom_segment(x = mu - 2 * sigma, xend = mu + 2 * sigma,
               #                                    y = 0.8 / sigma,
               #                                    col = col.pars,
               #                                    arrow = ggplot2::arrow(ends = 'both',
               #                                            length = grid::unit(0.1, "inches")))
            }

            if (display %in% c('density', 'violin')) {
               if (length(y) >= nmin) {
                  plt <- plt +
                     ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0, ymin = orig,
                                                       ymax = pmin(orig + scl * dgrid, 2 * dmax)),
                                          data = df.dens, col = col.dens, fill = col.dens)
                  if (display == 'violin')
                     plt <- plt +
                        ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0, ymax = orig,
                                                          ymin = pmax(orig - scl * dgrid, 0)),
                                             data = df.dens, col = col.dens, fill = col.dens)
               }
               dsgn <- if (display == 'density') 1 else df.densd$sgn
               sz   <- if (length(y) >= nmin) 0.2 else 1
               plt <- plt +
                  ggplot2::geom_point(ggplot2::aes(x, orig + dsgn * r * scl * d),
                                      data = df.densd, size = sz)
            }
         }

         if ((!plot.mean && display.sample['population']) |
             (plot.mean && display.mean['distribution'])) {
            xgrid <- seq(mu - 3 * stdev, mu + 3 * stdev, length = 200)
            pgrid <- dnorm(xgrid, mu, stdev)
            d.pop <- data.frame(xgrid, pgrid)
            plt <- plt +
               ggplot2::geom_line(ggplot2::aes(xgrid, orig + scl * pgrid),
                                  data = d.pop, linewidth = 1, col = col.pars)
            if (display == 'violin')
               plt <- plt +
               ggplot2::geom_line(ggplot2::aes(xgrid, orig - scl * pgrid),
                                  data = d.pop, linewidth = 1, col = col.pars)
         }
            
         if (display.sample['mean'] & (!plot.mean | any(display.mean))) {
            bottomm <- if (display == 'density') 0 else orig -0.65 * dmax
            plt <- plt + ggplot2::geom_segment(x = mu, y = 1.3 * dmax, yend = bottomm,
                                               col = col.pars) +
               ggplot2::annotate('text', x = mu, y = 1.4 * dmax,
                                 label = 'mean', col = col.pars)
         }
         
         if ((!plot.mean && display.sample['+/- 2 st.dev.']) |
             ( plot.mean && display.mean['+/- 2 se'])) {
            ypos <- 1.75 * dmax
            tpos <- mu + (-3:3) * stdev
            plt  <- plt +
               ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                                 y = ypos, col = col.pars) +
               ggplot2::annotate('segment', x = tpos, col = col.pars,
                                 y = ypos, yend = ypos - 0.04 * dmax) +
               ggplot2::annotate('text',    x = tpos,
                                 y = ypos - 0.12 * dmax, label = as.character(-3:3),
                                 col = col.pars) +
               ggplot2::annotate('text', x = mu, y = ypos + 0.12 * dmax,
                                 label = 'standard deviation scale', col = col.pars)
         }
         
         # Set horizontal range, allowing for zoom in
         sb  <- if (plot.mean & zoom) 3.5 * sigma / sqrt(n) else 3 * sigma
         plt <- plt +
            ggplot2::xlim(mu - sb, mu + sb) +
            ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                           axis.text.y  = ggplot2::element_blank(),
                           axis.ticks.y = ggplot2::element_blank(),
                           panel.grid.major.y = ggplot2::element_blank(),
                           panel.grid.minor.y = ggplot2::element_blank())
         print(plt)
      })
      
      panel
   }
   
   sample.draw.data <- function(panel) {
      panel$plot.mean <- FALSE
      rp.control.put(panel$panelname, panel)
      rp.do(panel, sample.draw)
      panel
   }
   
   sample.draw.mean <- function(panel) {
      panel$plot.mean <- TRUE
      rp.control.put(panel$panelname, panel)
      rp.do(panel, sample.draw)
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
      n             <- panel$pars["n"]
      panel$ydata    <- rnorm(n, mu, sigma)
      xgrid         <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
      dens          <- density(panel$ydata, bw = bw.norm(panel$ydata))
      panel$d.dens  <- data.frame(xgrid = dens$x, dgrid = dens$y)
      dens.y        <- if (length(panel$ydata) >= panel$nmin) approx(dens$x, dens$y,
                                                                    xout = panel$ydata)$y
                       else dnorm(panel$ydata, mu, sigma)
      sgn           <- sign(rbinom(length(panel$ydata), 1, 0.5) - 0.5)
      panel$d.densd <- data.frame(x = panel$ydata, d = dens.y,
                                  r = runif(length(dens.y), 0, 1), sgn = sgn)
      panel$mns     <- if (!panel$display.mean["accumulate"]) panel$mns <- mean(panel$ydata)
                       else c(mean(panel$ydata), panel$mns)
      if (length(panel$mns) >= panel$nmin) {
         mdens         <- density(panel$mns, bw.norm(panel$mns))
         panel$d.mdens <- data.frame(xgrid = mdens$x, dgrid = mdens$y)
         mdens.y       <- approx(mdens$x, mdens$y, xout = panel$mns)$y
      }
      else {
         panel$d.mdens <- dnorm(xgrid, mu, sigma / sqrt(n))
         mdens.y       <- dnorm(panel$mns, mu, sigma / sqrt(n))
      }
      sgn            <- sign(rbinom(length(panel$mns), 1, 0.5) - 0.5)
      panel$d.mdensd <- data.frame(x = panel$mns, d = mdens.y,
                                   r = runif(length(mdens.y), 0, 1), sgn = sgn)
      
      rp.control.put(panel$panelname, panel)
      if (panel$pplot) {
         rp.tkrreplot(panel, plotdata)
         rp.tkrreplot(panel, plotmean)
      }
      else
         sample.draw(panel)
      panel
   }
   
   bw.norm <- function(x) sd(x) * (4/(3 * length(x)))^0.2

   if (is.na(hscale)) hscale <- 1
   if (is.na(vscale)) vscale <- hscale
   
   display.sample        <- c(TRUE, rep(FALSE, 3))
   display.mean          <- rep(FALSE, 4)
   names(display.sample) <- c("data", "population", "mean", "+/- 2 st.dev.")
   names(display.mean)   <- c("sample mean", "accumulate", "+/- 2 se", "distribution")
   pars                  <- c(mu, sigma, n)
   names(pars)           <- c("mu", "sigma", "n")
   
   y           <- rnorm(n, mu, sigma)
   dmax.data   <- dnorm(mu, mu, sigma)
   dmax.mean   <- dnorm(mu, mu, sigma / sqrt(n))
   xgrid       <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
   dens        <- density(y)
   dens.y      <- approx(dens$x, dens$y, xout = y)$y
   d.dens      <- data.frame(xgrid = dens$x, dgrid = dens$y)
   sgn         <- sign(rbinom(length(y), 1, 0.5) - 0.5)
   d.densd     <- data.frame(x = y, d = dens.y,
                             r = runif(length(dens.y), 0, 1), sgn = sgn)
   mns         <- mean(y)
   mdens.y     <- dnorm(mns, mu, sigma / sqrt(n))
   sgn         <- sign(rbinom(length(mns), 1, 0.5) - 0.5)
   d.mdensd    <- data.frame(x = mns, d = mdens.y,
                             r = runif(length(mdens.y), 0, 1), sgn = sgn)
   d.mdens     <- dnorm(xgrid, mu, sigma / sqrt(n))

   panel <- rp.control(pars = pars, ydata = y, mns = mns, y = y,
                       nmin = 10, stdev = pars['sigma'],
                       d.dens = d.dens, d.densd = d.densd, 
                       d.mdens = d.mdens, d.mdensd = d.mdensd,
                       df.dens = d.dens, df.densd = d.densd,
                       dmax.mean = dmax.mean, dmax.data = dmax.data, dmax = dmax.data,
                       plot.mean = FALSE, zoom = FALSE,
                       display.sample = display.sample, display.mean = display.mean,
                       roptions = display.sample, pplot = panel.plot)
   Sys.sleep(pause)
   rp.grid(panel, "controls", row = 0, column = 0)
   Sys.sleep(pause)
   rp.grid(panel, "plots",    row = 0, column = 1, background = "white")
   Sys.sleep(pause)
   
   if (panel.plot) {
      rp.tkrplot(panel, plotdata, sample.draw.data,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "plots", row = 0, column = 0, background = "white")
      Sys.sleep(pause)
      rp.tkrplot(panel, plotmean, sample.draw.mean,
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

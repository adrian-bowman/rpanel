rp.sample <- function(mu = 0, sigma = 1, n = 25,
                      ggplot = TRUE, panel = TRUE, nbins = 30,
                      display, display.sample, display.mean, nsim = 50,
                      hscale = NA, vscale = hscale, pause = 0.01) {

   if (ggplot & !requireNamespace('ggplot2', quietly = TRUE)) {
      ggplot <- FALSE
      message('the ggplot package is not available - reverting to standard graphics.')
   }
   
   if (!ggplot)
      return(rp.sample.old(mu = 0, sigma = 1, n = 25,
                    panel.plot = TRUE, hscale = NA, vscale = hscale))
   
   sample.draw <- function(panel) {
      
      mu       <- panel$pars["mu"]
      n        <- panel$pars["n"]
      col.pars <- 'darkblue'
      col.dens <- 'grey75'
      
      panel <- within(panel, {
         
         if (plot.mean & !any(display.mean)) {
            plt <- ggplot2::ggplot() +
                   ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white',
                                                                           colour = 'white'))
            print(plt)
            return(panel)
         }

         y        <- if (plot.mean) mns else ydata
         dmax     <- if (plot.mean) dmax.mean else dmax.data
         mu       <- pars['mu']
         stdev    <- if (plot.mean) pars['sigma'] / sqrt(pars['n'])
                               else pars['sigma']
         df.dens  <- if (plot.mean) d.mdens  else d.dens
         df.densd <- if (plot.mean) d.mdensd else d.densd
         orig     <- if (display == 'violin') 0.7 * dmax else 0
         scl      <- if (display == 'violin') 0.5 else 1
         
         plt <- ggplot2::ggplot(data.frame(x = y), ggplot2::aes(x))
         
         if ((!plot.mean && display.sample['data']) |
             (plot.mean && display.mean['sample mean'])) {
            
            # Mark observations outside the scale of the histogram
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

            if (length(y) >= nmin) {
               if (display == 'histogram') {
                  brks <- seq(mu - 3 * stdev, mu + 3 * stdev, length = nbins)
                     plt  <- plt +
                        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                                breaks = brks, col = 'grey50', fill = col.dens)
               }
               if (display %in% c('density', 'violin'))
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
            if ((length(y) < nmin) | ((display != 'histogram') & (length(y) <= nmax))) {
               dsgn <- if (display == 'violin') df.densd$sgn else 1
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
            txt  <- if (plot.mean) 'standard error scale' else 'standard deviation scale'
            plt  <- plt +
               ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                                 y = ypos, col = col.pars) +
               ggplot2::annotate('segment', x = tpos, col = col.pars,
                                 y = ypos, yend = ypos - 0.04 * dmax) +
               ggplot2::annotate('text',    x = tpos,
                                 y = ypos - 0.12 * dmax, label = as.character(-3:3),
                                 col = col.pars) +
               ggplot2::annotate('text', x = mu, y = ypos + 0.12 * dmax,
                                 label = txt, col = col.pars)
         }
         
         # Set axes ranges, allowing for zoom in
         sb  <- if (plot.mean & zoom) 3.5 * stdev else 3 * pars['sigma']
         plt <- plt +
            ggplot2::xlim(mu - sb, mu + sb) +
            ggplot2::scale_y_continuous(expand = ggplot2::expansion(0, 0),
                                        limits = c(0, 2 * dmax)) +
            ggplot2::ylab('density')
         if (display == 'violin')
            plt <- plt +
               ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                              axis.text.y  = ggplot2::element_blank(),
                              axis.ticks.y = ggplot2::element_blank(),
                              panel.grid.major.y = ggplot2::element_blank(),
                              panel.grid.minor.y = ggplot2::element_blank())
         
         if (panel.interactive) print(plt) else panel$plt <- plt
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
      mu              <- panel$pars["mu"]
      sigma           <- panel$pars["sigma"]
      n               <- panel$pars["n"]
      panel$dmax.data <- dnorm(mu, mu, sigma)
      panel$dmax.mean <- dnorm(mu, mu, sigma / sqrt(n))
      panel$ydata     <- rnorm(n, mu, sigma)
      xgrid           <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
      if (length(panel$ydata) >= panel$nmin) {
         dens            <- density(panel$ydata, bw = bw.norm(panel$ydata))
         panel$d.dens    <- data.frame(xgrid = dens$x, dgrid = dens$y)
         dens.y          <- approx(dens$x, dens$y,xout = panel$ydata)$y
      }
      else {
         panel$d.dens    <- data.frame(xgrid = xgrid, dgrid = 0)
         dens.y          <- dnorm(panel$ydata, mu, sigma)
      }
      sgn             <- sign(rbinom(length(panel$ydata), 1, 0.5) - 0.5)
      panel$d.densd   <- data.frame(x = panel$ydata, d = dens.y,
                                    r = runif(length(dens.y), 0, 1), sgn = sgn)
      panel$mns       <- if (!panel$display.mean["accumulate"]) panel$mns <- mean(panel$ydata)
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
      
      if (panel.interactive) {
         rp.control.put(panel$panelname, panel)
         rp.tkrreplot(panel, plotdata)
         rp.tkrreplot(panel, plotmean)
         panel
      }
      else {
         result <- list()
         panel$plot.mean <- FALSE
         result$data     <- sample.draw(panel)$plt
         panel$plot.mean <- TRUE
         result$mean     <- sample.draw(panel)$plt
         result
      }
   }
   
   panel.interactive <- panel
   if (is.na(hscale)) hscale <- 1
   if (is.na(vscale)) vscale <- hscale
   
   if (missing(display)) display <- 'histogram'
   if (missing(display.sample)) display.sample <- logical(0)
   if (!('data'          %in% names(display.sample)))
      display.sample <- c(display.sample, 'data' = TRUE)
   if (!('population'    %in% names(display.sample)))
      display.sample <- c(display.sample, 'population' = FALSE)
   if (!('mean'          %in% names(display.sample)))
      display.sample <- c(display.sample, 'mean' = FALSE)
   if (!('+/- 2 st.dev.' %in% names(display.sample)))
      display.sample <- c(display.sample, '+/- 2 st.dev.' = FALSE)
   if (missing(display.mean)) display.mean <- logical(0)
   if (!('sample mean'          %in% names(display.mean)))
      display.mean <- c(display.mean, 'sample mean' = FALSE)
   if (!('accumulate'    %in% names(display.mean)))
      display.mean <- c(display.mean, 'accumulate' = FALSE)
   if (!('+/- 2 se'          %in% names(display.mean)))
      display.mean <- c(display.mean, '+/- 2 se' = FALSE)
   if (!('distribution' %in% names(display.mean)))
      display.mean <- c(display.mean, 'distribution' = FALSE)
   if (!(display %in% c('histogram', 'density', 'violin'))) {
      display <- 'histogram'
      message('display not recognised - using histogram.')
   }

   pars        <- c(mu = mu, sigma = sigma, n = n)
   y           <- rnorm(n,  mu, sigma)
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

   if (panel.interactive) {
      panel <- rp.control(pars = pars, ydata = y, mns = mns, y = y,
                          nmin = 10, nmax = 5000, stdev = pars['sigma'],
                          d.dens = d.dens, d.densd = d.densd, 
                          d.mdens = d.mdens, d.mdensd = d.mdensd,
                          df.dens = d.dens, df.densd = d.densd,
                          dmax.mean = dmax.mean, dmax.data = dmax.data,
                          plot.mean = FALSE, zoom = FALSE,
                          display.sample = display.sample, display.mean = display.mean,
                          roptions = display.sample, panel.interactive = panel.interactive)
      Sys.sleep(pause)
      rp.grid(panel, "controls", row = 0, column = 0)
      Sys.sleep(pause)
      rp.grid(panel, "plots",    row = 0, column = 1, background = "white")
      Sys.sleep(pause)
   
      rp.tkrplot(panel, plotdata, sample.draw.data,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "plots", row = 0, column = 0, background = "white")
      Sys.sleep(pause)
      rp.tkrplot(panel, plotmean, sample.draw.mean,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "plots", row = 1, column = 0, background = "white")
      rp.textentry(panel, pars, sample.changepars, width = 10, title = 'Parameters',
                  c("mean", "st.dev.", "sample size"), c("mu", "sigma", "n"),
                  grid = "controls", row = 0, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.radiogroup(panel, display, c('histogram', 'density', 'violin'), title = 'Display',
                  action = sample.redraw,
                  grid = "controls", row = 1, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.button(panel, sample.new, "Sample", repeatinterval = 1, repeatdelay = 1,
                grid = "controls", row = 2, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.checkbox(panel, display.sample, sample.redraw, names(display.sample),
                  title = "Sample",
                  grid = "controls", row = 3, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.checkbox(panel, display.mean, sample.redraw, names(display.mean),
                  title = "Sample mean",
                  grid = "controls", row = 4, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.checkbox(panel, zoom, sample.redraw, labels = "zoom in",
                  grid = "controls", row = 5, column = 0, sticky = "ew")
      return(invisible())
   }
   else {
      ymat <- matrix(rnorm(n * (nsim - 1), mu,  sigma), ncol = nsim - 1)
      mns  <- apply(ymat, 2, mean)
      display.mean['accumulate'] <- TRUE
      pnl <- list(pars = pars, ydata = y, mns = mns, y = y,
                  nmin = 10, nmax = 5000, stdev = pars['sigma'],
                  d.dens = d.dens, d.densd = d.densd, 
                  d.mdens = d.mdens, d.mdensd = d.mdensd,
                  df.dens = d.dens, df.densd = d.densd,
                  dmax.mean = dmax.mean, dmax.data = dmax.data,
                  plot.mean = FALSE, zoom = FALSE,
                  display.sample = display.sample, display.mean = display.mean,
                  roptions = display.sample, panel.interactive = panel.interactive)
      result <- sample.new(pnl)
      return(result)
   }
   
}

bw.norm <- function(x) sd(x) * (4/(3 * length(x)))^0.2

rp.sample <- function(n = 25, mu = 5, sigma = 0.4, p = 0.7,
                      distribution  = 'normal', shape = 0,
                      panel = TRUE, nbins = 20, nbins.mean = 20,
                      display, display.sample, display.mean, nsim = 50,
                      show.out.of.range = TRUE,
                      hscale = NA, vscale = hscale, pause = 0.01) {

   ggplot     <- TRUE
   shape0     <- (abs(shape) < 2 * .Machine$double.eps)
   sn.present <- requireNamespace('sn', quietly = TRUE)
   if (!shape0 & !sn.present)
      message('the sn package is not available so the shape parameter has been reset to 0.')
   if (ggplot & !requireNamespace('ggplot2', quietly = TRUE)) {
      ggplot <- FALSE
      message('the ggplot package is not available - reverting to standard graphics.')
   }
   
   if (!ggplot)
      return(rp.sample.old(mu = 0, sigma = 1, n = 25,
                    panel.plot = TRUE, hscale = NA, vscale = hscale))
   
   sample.draw <- function(panel) {
      
      mu       <- panel$pars["mu"]
      n        <- panel$samplesize
      col.pars <- 'darkblue'
      col.dens <- 'grey75'
      
      panel <- within(panel, {
         
         if (plot.mean & !any(display.mean)) {
            plt <- ggplot2::ggplot() +
                   ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white',
                                                                           colour = 'white'))
            if (panel.interactive) print(plt)
            else panel$plt <- plt
            return(panel)
         }

         if (plot.mean) {
            y <- if (display.mean['t-statistic']) tstats else mns
         } else
            y <- ydata         
         mu       <- pars['mu']
         stdev    <- if (plot.mean) pars['sigma'] / sqrt(n) else pars['sigma']
         m.cur    <- if (plot.mean & display.mean['t-statistic']) 0 else as.vector(mu)
         s.cur    <- if (plot.mean & display.mean['t-statistic']) 1 else as.vector(stdev)
         dmax     <- if (plot.mean) dmax.mean else dmax.data
         d.cur    <- if (plot.mean & display.mean['t-statistic']) 0.4 else as.vector(dmax)
         orig     <- if (display == 'violin') 0.7 * d.cur else 0
         scl      <- if (display == 'violin') 0.5 else 1
         if (plot.mean) {
            df.dens  <- if (display.mean['t-statistic']) d.tdens  else d.mdens
            df.densd <- if (display.mean['t-statistic']) d.tdensd else d.mdensd
         }
         else {
            df.dens  <- d.dens
            df.densd <- d.densd
         }

         plt <- ggplot2::ggplot(data.frame(x = y), ggplot2::aes(x))
         
         # Show the data or means or t-statistics
         if ((!plot.mean && display.sample['data']) |
             (plot.mean && (display.mean['sample mean'] | display.mean['t-statistic']))) {
            if (length(y) >= nmin) {
               if (display == 'histogram') {
                  nb   <- if (plot.mean) nbins.mean else nbins
                  # brks <- seq(mu - 3 * stdev, mu + 3 * stdev, length = nb + 1)
                  # plt <- plt +
                  #    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                  #                            breaks = brks, col = 'grey50', fill = col.dens)
                  low   <- which(y < m.cur - 3 * s.cur)
                  high  <- which(y > m.cur + 3 * s.cur)
                  nlow  <- length(low)
                  nhigh <- length(high)
                  wdth  <- 6 * s.cur / nb
                  nblo  <- ceiling((m.cur - min(y)) / wdth)
                  nbhi  <- ceiling((max(y) - m.cur) / wdth)
                  lo.b  <- min(m.cur - 3 * s.cur, m.cur - (nblo + 1) * wdth)
                  hi.b  <- max(m.cur + 3 * s.cur, m.cur + (nbhi + 1) * wdth)
                  brks  <- seq(lo.b, hi.b, by = wdth)
                  hst   <- hist(y, breaks = brks, plot = FALSE)
                  ind   <- (hst$breaks[-length(hst$breaks)] < m.cur - 3 * s.cur) |
                           (hst$breaks[-1] > m.cur + 3 * s.cur)
                  dfrm  <- data.frame(x = hst$breaks[-1][!ind] - wdth / 2,
                                      y = pmin(hst$density[!ind], 1.9 * d.cur))
                  trnc  <- which(hst$density[!ind] > 1.9 * d.cur)
                  plt <- plt + ggplot2::geom_col(ggplot2::aes(x, y),
                                        width = wdth, col = 'black', fill = 'grey', data = dfrm) +
                               ggplot2::geom_text(ggplot2::aes(x, y + d.cur / 20, label = '+'),
                                                  data = dfrm[trnc, ])
                  if (show.out.of.range) {
                     if (nlow > 0) {
                        plural <- if (nlow > 1) 's' else ''
                        plt <- plt + ggplot2::annotate('text', angle = 90,
                              x = m.cur - 3 * s.cur, y = 0.75 * d.cur,
                              label = paste(nlow, ' observation', plural, ' lower', sep = ''))
                     }
                     if (nhigh > 0) {
                        plural <- if (nhigh > 1) 's' else ''
                        plt <- plt + ggplot2::annotate('text', angle = 90,
                              x = m.cur + 3 * s.cur, y = 0.75 * d.cur,
                              label = paste(nhigh, ' observation', plural, ' higher', sep = ''))
                     }
                  }
               }
               if (display %in% c('density', 'violin'))
                  plt <- plt +
                     ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0, ymin = orig,
                                                     ymax = pmin(orig + scl * dgrid, 2 * d.cur)),
                                          data = df.dens, col = col.dens, fill = col.dens)
               if (display == 'violin')
                  plt <- plt +
                     ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0, ymax = orig,
                                                       ymin = pmax(orig - scl * dgrid, 0)),
                                          data = df.dens, col = col.dens, fill = col.dens)
            }
            if ((length(y) < nmin) | ((display != 'histogram') & (length(y) <= nmax))) {
               dsgn <- if (display == 'violin') df.densd$sgn else 1
               sz   <- if (length(y) >= nmin) 0.2 else 2
               dft  <- if (plot.mean & display.mean['t-statistic'])
                          data.frame(x = tstats, d = dt(tstats, n - 1), r = df.densd$r)
                       else df.densd
               plt <- plt +
                   ggplot2::geom_point(ggplot2::aes(x, orig + dsgn * r * scl * d),
                                       data = dft, size = sz)
            }
         }

         # Show the distribution density curve
         if ((!plot.mean && display.sample['population']) |
             ( plot.mean && display.mean['distribution'])) {
            xgrid <- seq(mu - 3 * stdev, mu + 3 * stdev, length = 200)
            if (!plot.mean)
               pgrid <- if (!sn.present) dnorm(xgrid, mu, stdev)
                        else sn::dsn(xgrid, sn.xi, sn.omega, sn.shape)
            else {
               if (display.mean['t-statistic']) {
                  xgrid <- seq(-3.5, 3.5, length = 200)
                  pgrid <- dt(xgrid, n - 1)
               }
               else
                  pgrid <- dnorm(xgrid, mu, stdev)
            }
            d.pop <- data.frame(xgrid, pgrid)
            plt <- plt +
               ggplot2::geom_line(ggplot2::aes(xgrid, orig + scl * pgrid),
                                  data = d.pop, linewidth = 1, col = col.pars)
            # Show the standard normal density to compare with the t density
            # if (plot.mean & display.mean['t-statistic'])
            #    plt <- plt +
            #       ggplot2::geom_line(ggplot2::aes(xgrid, orig + scl * dnorm(xgrid)),
            #                          data = d.pop, linewidth = 1, linetype = 2,
            #                          col = col.pars)
            if (display == 'violin')
               plt <- plt +
               ggplot2::geom_line(ggplot2::aes(xgrid, orig - scl * pgrid),
                                  data = d.pop, linewidth = 1, col = col.pars)
         }
            
         # Show the mean value
         if (!(plot.mean & display.mean['t-statistic'])) {
            if (display.sample['mean'] & (!plot.mean | any(display.mean))) {
               bottomm <- if (display == 'density') 0 else orig -0.65 * d.cur
               plt <- plt + ggplot2::geom_segment(x = mu, y = 1.3 * d.cur, yend = bottomm,
                                                col = col.pars) +
                  ggplot2::annotate('text', x = mu, y = 1.4 * d.cur,
                                 label = 'mean', col = col.pars)
            }
         }
         
         # Show the sd or se scale
         if ((!plot.mean && display.sample['st.dev. scale']) |
             ( plot.mean && display.mean['se scale'])) {
            ypos <- 1.75 * d.cur
            tpos <- if (plot.mean & display.mean['t-statistic']) -3:3 else mu + (-3:3) * stdev
            if (plot.mean) {
               txt  <- if (display.mean['t-statistic']) 't-statistic' else 'standard error'
            } else txt <- 'standard deviation'
            txt <- paste(txt, 'scale')
            plt  <- plt +
               ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                                 y = ypos, col = col.pars) +
               ggplot2::annotate('segment', x = tpos, col = col.pars,
                                 y = ypos, yend = ypos - 0.04 * d.cur) +
               ggplot2::annotate('text',    x = tpos,
                                 y = ypos - 0.12 * d.cur, label = as.character(-3:3),
                                 col = col.pars) +
               ggplot2::annotate('text', x = m.cur, y = ypos + 0.12 * d.cur,
                                 label = txt, col = col.pars)
         }
         
         # Set axes ranges, allowing for zoom in
         ym <- if (plot.mean & display.mean['t-statistic']) 0.8 else 2 * d.cur
         plt <- plt +
            ggplot2::scale_y_continuous(expand = ggplot2::expansion(0, 0), limits = c(0, ym)) +
            ggplot2::ylab('density')
         if (plot.mean & display.mean['t-statistic'])
            plt <- plt + ggplot2::xlim(-3.5, 3.5)
         else {
            sb  <- if (plot.mean & display.mean['zoom']) 3.5 * stdev else 3 * pars['sigma']
         # if (plot.mean & display.mean['t-statistic'])
         #    plt <- plt +
         #       ggplot2::theme(axis.title.x = ggplot2::element_blank(),
         #                      axis.text.x  = ggplot2::element_blank(),
         #                      axis.ticks.x = ggplot2::element_blank(),
         #                      panel.grid.major.x = ggplot2::element_blank(),
         #                      panel.grid.minor.x = ggplot2::element_blank())
         # else
            plt <- plt + ggplot2::xlim(mu - sb, mu + sb)
         }
            
         # Remove y axis information for the violin plot
         if (display == 'violin')
            plt <- plt +
               ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                              axis.text.y  = ggplot2::element_blank(),
                              axis.ticks.y = ggplot2::element_blank(),
                              panel.grid.major.y = ggplot2::element_blank(),
                              panel.grid.minor.y = ggplot2::element_blank())
         
         # Add title
         ttl <- 'Sample'
         if (plot.mean) {
            ttl <- if (display.mean['t-statistic']) 't-statistic' else paste(ttl, 'mean')
            if (length(y) > 1) ttl <- paste(ttl, 's', sep = '')
         }
         plt <- plt + ggplot2::ggtitle(ttl)
         
         if (panel.interactive) print(plt) else panel$plt <- plt
      })
      
      panel
   }
   
   sample.draw.data <- function(panel) {
      panel$plot.mean <- FALSE
      rp.control.put(panel$panelname, panel)
      panel <- sample.draw(panel)
      panel
   }
   
   sample.draw.mean <- function(panel) {
      panel$plot.mean <- TRUE
      rp.control.put(panel$panelname, panel)
      panel <- sample.draw(panel)
      panel
   }
   
   sample.redraw <- function(panel) {
      rp.tkrreplot(panel, plotdata)
      rp.tkrreplot(panel, plotmean)
      panel
   }
   
   sample.changepars <- function(panel) {
      panel$mns <- NULL
      panel$samplesize <- as.numeric(panel$samplesize)
      # rp.control.put(panel$panelname, panel)
      panel <- sample.new(panel)
      panel
   }

   sample.new <- function(panel) {
      mu              <- panel$pars["mu"]
      sigma           <- panel$pars["sigma"]
      n               <- panel$samplesize
      panel$ydata     <- if (!panel$sn.present) rnorm(n, mu, sigma)
                         else sn::rsn(n, panel$sn.xi, panel$sn.omega, panel$sn.shape)
      panel$dmax.mean <- dnorm(mu, mu, sigma / sqrt(n))
      xgrid           <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
      if (length(panel$ydata) >= panel$nmin) {
         dens            <- density(panel$ydata, bw = bw.norm(panel$ydata))
         panel$d.dens    <- data.frame(xgrid = dens$x, dgrid = dens$y)
         dens.y          <- approx(dens$x, dens$y,xout = panel$ydata)$y
      }
      else {
         panel$d.dens    <- data.frame(xgrid = xgrid, dgrid = 0)
         dens.y          <- if (!sn.present) dnorm(panel$ydata, mu, sigma)
                            else sn::dsn(panel$ydata, panel$sn.xi, panel$sn.omega, panel$sn.shape)
      }
      sgn             <- sign(rbinom(length(panel$ydata), 1, 0.5) - 0.5)
      panel$d.densd   <- data.frame(x = panel$ydata, d = dens.y,
                                    r = runif(length(dens.y), 0, 1), sgn = sgn)
      
      mn              <- mean(panel$ydata)
      tstat           <- (mn - mu) / (sd(panel$ydata) / sqrt(n))
      panel$mns       <- if (panel$display.mean["accumulate"]) c(mn, panel$mns)
                         else mn
      panel$tstats    <- if (panel$display.mean["accumulate"]) c(tstat, panel$tstats)
                         else tstat
      if (length(panel$mns) >= panel$nmin) {
         mdens         <- density(panel$mns, bw.norm(panel$mns))
         panel$d.mdens <- data.frame(xgrid = mdens$x, dgrid = mdens$y)
         mdens.y       <- approx(mdens$x, mdens$y, xout = panel$mns)$y
         tdens         <- density(panel$tstats, bw.norm(panel$tstats))
         panel$d.tdens <- data.frame(xgrid = tdens$x, dgrid = tdens$y)
         tdens.y       <- approx(tdens$x, tdens$y, xout = panel$tstats)$y
      }
      else {
         panel$d.mdens <- dnorm(xgrid, mu, sigma / sqrt(n))
         mdens.y       <- dnorm(panel$mns, mu, sigma / sqrt(n))
         tdens.y       <- dt(panel$tstats, n - 1)
      }
      sgn            <- sign(rbinom(length(panel$mns), 1, 0.5) - 0.5)
      r              <- runif(length(mdens.y), 0, 1)
      panel$d.mdensd <- data.frame(x = panel$mns,   d = mdens.y, r = r, sgn = sgn)
      panel$d.tdensd <- data.frame(x = panel$tstats, d = tdens.y, r = r, sgn = sgn)
      
      if (panel.interactive) {
         rp.control.put(panel$panelname, panel)
         rp.tkrreplot(panel, plotdata)
         rp.tkrreplot(panel, plotmean)
         panel
      }
      else {
         result <- list()
         panel$plot.mean <- FALSE
         result$sample   <- sample.draw(panel)$plt
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
   if (!('st.dev. scale' %in% names(display.sample)))
      display.sample <- c(display.sample, 'st.dev. scale' = FALSE)
   if (missing(display.mean)) display.mean <- logical(0)
   if (!('sample mean'          %in% names(display.mean)))
      display.mean <- c(display.mean, 'sample mean' = FALSE)
   if (!('accumulate' %in% names(display.mean)))
      display.mean <- c(display.mean, 'accumulate' = FALSE)
   if (!('se scale' %in% names(display.mean)))
      display.mean <- c(display.mean, 'se scale' = FALSE)
   if (!('distribution' %in% names(display.mean)))
      display.mean <- c(display.mean, 'distribution' = FALSE)
   if (!('zoom' %in% names(display.mean)))
      display.mean <- c(display.mean, 'zoom' = FALSE)
   if (!('t-statistic' %in% names(display.mean)))
      display.mean <- c(display.mean, 't-statistic' = FALSE)
   if (!(display %in% c('histogram', 'density', 'violin'))) {
      display <- 'histogram'
      message('display not recognised - using histogram.')
   }
   display.sample <- display.sample[c('data', 'population', 'mean', 'st.dev. scale')]
   display.mean   <- display.mean[c('sample mean', 'accumulate', 'se scale',
                                    'zoom', 't-statistic', 'distribution')]
   normal <- (distribution == 'normal')
   if (!normal & ((mu < 0) | (mu > 1)))
      stop('when the distribution is bonimial, the mean value must lie between 0 and 1.')

   pars      <- c(mu = mu, sigma = sigma)
   sn.delta  <- shape / sqrt(1 + shape^2)
   sn.omega  <- sigma / sqrt(1 - 2 * sn.delta^2 / pi)
   sn.xi     <- mu - sn.omega * sn.delta * sqrt(2 / pi)
   sn.mode   <- sn.xi + sn.omega *
                   (sn.delta * sqrt(2 / pi) - (1 - pi / 4) * ((sqrt(2 / pi) * sn.delta)^3) /
                   (1 - sn.delta^2 * 2 / pi) - exp(-2 * pi / abs(shape)) * sign(shape) / 2)
   
   # Generate an initial set of data
   if (normal) {
      y         <- if (!sn.present) rnorm(n, mu, sigma)
                   else sn::rsn(n, sn.xi, sn.omega, shape)
      stdev     <- sigma
      dmax.data <- if (!sn.present) dnorm(mu, mu, sigma)
                   else sn::dsn(sn.mode, sn.xi, sn.omega, shape)
      xgrid     <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 200)
      dens      <- density(y)
      dens.y    <- approx(dens$x, dens$y, xout = y)$y
      d.dens    <- data.frame(xgrid = dens$x, dgrid = dens$y)
      sgn       <- sign(rbinom(length(y), 1, 0.5) - 0.5)
      d.densd   <- data.frame(x = y, d = dens.y,
                              r = runif(length(dens.y), 0, 1), sgn = sgn)
   }
   else {
      y         <- rbinom(n, 1, mu)
      stdev     <- sqrty(mu * (1 - mu))
      d.dens    <- NA
      d.densd   <- NA
      dmax.data <- max(mu, 1 - mu)
   }
   dmax.mean <- dnorm(mu, mu, stdev / sqrt(n))
   
   # Compute the distributional information on the means
   mns       <- mean(y)
   tstats    <- (mns - mu) / (sd(y) / sqrt(n))
   mdens.y   <- dnorm(mns, mu, stdev / sqrt(n))
   tdens.y   <- if (normal) dt(tstats, n - 1) else dnorm(tstats)
   sgn       <- sign(rbinom(length(mns), 1, 0.5) - 0.5)
   r         <- runif(length(mdens.y), 0, 1)
   d.mdensd  <- data.frame(x = mns,    d = mdens.y, r = r, sgn = sgn)
   d.tdensd  <- data.frame(x = tstats, d = tdens.y, r = r, sgn = sgn)
   d.mdens   <- dnorm(xgrid, mu, stdev / sqrt(n))
   xgrid     <- seq(-3, 3, length = 200)
   d.tdens   <- if (normal) dt(xgrid, n - 1) else dnorm(xgrid)
   
   if (panel.interactive) {
      panel <- rp.control(pars = pars, samplesize = n, sn.present = sn.present,
                          sn.xi = sn.xi, sn.omega = sn.omega, sn.shape = shape,
                          sn.mode = sn.mode, ydata = y, mns = mns, tstats = tstats, 
                          y = y, nmin = 10, nmax = 5000, stdev = stdev,
                          d.dens  = d.dens,  d.densd  = d.densd, 
                          d.mdens = d.mdens, d.mdensd = d.mdensd,
                          d.tdens = d.tdens, d.tdensd = d.tdensd,
                          df.dens = d.dens,  df.densd = d.densd,
                          dmax.mean = dmax.mean, dmax.data = dmax.data,
                          plot.mean = FALSE, zoom = FALSE,
                          nbins = nbins, nbins.mean = nbins.mean,
                          display.sample = display.sample, display.mean = display.mean,
                          roptions = display.sample, panel.interactive = panel.interactive)
      Sys.sleep(pause)
      rp.grid(panel, "sample",       row = 0, column = 1)
      Sys.sleep(pause)
      rp.grid(panel, "datacontrols", row = 1, column = 0)
      Sys.sleep(pause)
      rp.grid(panel, "dataplot",     row = 1, column = 1, background = "white")
      Sys.sleep(pause)
      rp.grid(panel, "meancontrols", row = 2, column = 0)
      Sys.sleep(pause)
      rp.grid(panel, "meanplot",     row = 2, column = 1, background = "white")
      Sys.sleep(pause)
   
      rp.tkrplot(panel, plotdata, sample.draw.data,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "dataplot", row = 0, column = 0, background = "white")
      Sys.sleep(pause)
      rp.tkrplot(panel, plotmean, sample.draw.mean,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "meanplot", row = 0, column = 0, background = "white")
      Sys.sleep(pause)
      rp.button(panel, sample.new, "New sample", repeatinterval = 1, repeatdelay = 1,
                grid = 'sample', row = 0, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.textentry(panel, samplesize, sample.changepars, 'sample size', width = 10,
                   grid = 'sample', row = 0, column = 1, sticky = "ew")
      Sys.sleep(pause)
      rp.text(panel, paste('  mean:', signif(mu, 5)),
              grid = 'sample', row = 0, column = 2, sticky = "ew")
      Sys.sleep(pause)
      rp.text(panel, paste('  st.dev:', signif(sigma, 5)),
              grid = 'sample', row = 0, column = 3, sticky = "ew")
      if (!shape0) {
         Sys.sleep(pause)
         rp.text(panel, paste('  shape:', signif(shape, 5)),
                 grid = 'sample', row = 0, column = 4, sticky = "ew")
      }
      # rp.textentry(panel, pars, sample.changepars, width = 10, title = 'Parameters',
      #              c("mean", "st.dev.", "sample size"), c("mu", "sigma", "n"),
      #              grid = "controls", row = 0, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.menu(panel, display, list(list('Display', 'histogram', 'density', 'violin')),
                    action = sample.redraw)
      Sys.sleep(pause)
      rp.checkbox(panel, display.sample, sample.redraw, names(display.sample),
                  title = "Sample",
                  grid = "datacontrols", row = 0, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.slider(panel, nbins, 10, 100, sample.redraw, resolution = 1,
                grid = 'datacontrols', row = 1, column = 0, sticky = 'ew')
      Sys.sleep(pause)
      rp.checkbox(panel, display.mean, sample.redraw, names(display.mean),
                  title = "Sample mean",
                  grid = "meancontrols", row = 0, column = 0, sticky = "ew")
      Sys.sleep(pause)
      rp.slider(panel, nbins.mean, 10, 100, sample.redraw, resolution = 1,
                grid = 'meancontrols', row = 1, column = 0, sticky = 'ew')
      return(invisible(panel))
   }
   else {
      if (display.mean['accumulate']) {
         ymat   <- if (!sn.present) rnorm(n * (nsim - 1), mu, sigma)
                   else sn::rsn(n * (nsim - 1), sn.xi, sn.omega, shape)
         ymat   <- matrix(ymat, ncol = nsim - 1)
         mns    <- apply(ymat, 2, mean)
         sds    <- apply(ymat, 2, sd)
         tstats <- (mns - mu) / (sds / sqrt(n))
      }
      else
         mns <- NULL
      pnl <- list(pars = pars, samplesize = n, sn.present = sn.present,
                  sn.xi = sn.xi, sn.omega = sn.omega, sn.shape = shape,
                  sn.mode = sn.mode, 
                  ydata = y, mns = mns, tstats = tstats, y = y,
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

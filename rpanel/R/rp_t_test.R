#     A function for a confidence interval and hypothesis test
#     on two samples of data

rp.t_test <- function(x, y = NULL, mu = NULL, display = 'density',
                     uncertainty = 'none', se.scale = TRUE,
                     zoom = FALSE, col = '#86B875', refcol = '#E495A5', xlab, ylab, ...) {
   
   if (!requireNamespace('ggplot2'))
      stop('the ggplot2 package is not available.')
   
   if (!(display %in% c('density', 'histogram'))) {
      warning("display not recognised - reverting to 'density'.")
      display <- 'density'
   }
   if (!(uncertainty %in% c('none', 'sample mean', 'reference')))
         uncertainty <- 'sample mean'
   ttest.args <- list(...)
   paired     <- if ('paired' %in% names(ttest.args)) ttest.args$paired else FALSE
   reference  <- !is.null(mu)
   if (!reference) mu <- 0
   horizontal <- TRUE
   
   ttest  <- t.test(x, y, mu = mu, ...)
   method <- if      (is.null(y)) 'One'
             else if (!paired)    'Two'
             else                 'Paired'
   height <- c('data' = 0.25, 'uncertainty' = 0.15, 'mean' = 0.5)
   # if (missing(col)) {
   #    col <- if (method == 'Two')
   #       # col = c('#1FC8DEFF', '#F1CA3AFF', '#C1F334FF', '#BE2102FF'),
   #       col = c(grey(0.5), grey(0.5), '#86B875', '#E495A5')
   #       else col = '#86B875'
   # }
   # if (missing(col))    col    <- '#86B875'
   # if (missing(refcol)) refcol <- '#E495A5'
   clr <- c(estimate  = '#B3CDE3', estline = '#0093FF',
            reference = '#FBB4AE', refline = '#FF7F00',
            points    = 'grey50',  notch   = 'black',
            density   = 'grey75')
   if (missing(xlab))   xlab <- deparse(substitute(x))
   ylab <- if ((method == 'Two') & missing(ylab)) deparse(substitute(y)) else NA

   plot.args <- list(se.scale = se.scale, col = clr, horizontal = horizontal,
                     height = height, display = display, zoom = zoom,
                     uncertainty = uncertainty, reference = reference,
                     xlab = xlab, ylab = ylab, nmin = 10)

   # Formula case to be added
   
   
   if (method == 'Paired') x <- x - y
   plt <- if (method %in% c('One', 'Paired'))
               rp.onesample(x, ttest, ttest.args, plot.args)
          else rp.twosample(x, y, ttest, ttest.args, plot.args)
   
   invisible(plt)
}


rp.onesample <- function(x, ttest, ttest.args, plot.args) {
   
   display     <- plot.args$display
   violin      <- (display == 'violin')
   uncertainty <- plot.args$uncertainty
   col.dens    <- plot.args$col['density']
   mu          <- ttest$null.value
   mn          <- ttest$estimate
   se          <- ttest$stderr
   se.scale    <- plot.args$se.scale
   if (uncertainty != 'none') se.scale <- TRUE
   reference   <- plot.args$reference
   hst         <- hist(x, plot = FALSE)
   dmax        <- max(hst$density)
   
   # Set up the plot
   
   dens    <- density(x, bw = bw.norm(x))
   xlimits <- range(mean(x) - 4 * se, mean(x) + 4 * se)
   if (!plot.args$zoom) xlimits <- range(xlimits, dens$x)
   if (plot.args$reference) xlimits <- range(xlimits, mu - 4 * se, mu + 4 * se)
   ticks <- (if (violin) c(0, -1.5, -2) else c(0.5, -0.5, -1)) * dmax
   ylabs <- c('data\n', 'uncertainty\nof the mean', 'se scale')
   tind  <- 1
   if (uncertainty != 'none') tind <- c(tind, 2)
   if (se.scale) tind <- c(tind, 3)
   cntr    <- if (uncertainty == 'reference') mu else ttest$estimate
   dfrm  <- data.frame(x, gp = 1)
   plt <- ggplot2::ggplot(dfrm, ggplot2::aes(x, gp)) +
      ggplot2::theme(
         # axis.text.y        = ggplot2::element_text(angle = 90, vjust = 0.5),
         axis.ticks.y       = ggplot2::element_blank(),
         axis.title.y       = ggplot2::element_blank(),
         panel.grid.major.y = ggplot2::element_blank(),
         panel.grid.minor.y = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = ticks[tind],
                                  labels = ylabs[tind])
      # ggplot2::xlim(xlimits[1], xlimits[2])
      
   
   # Plot the data
   
   if (!plot.args$zoom) {
      if (display == 'histogram') {
         plt <- plt +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                    breaks = hst$breaks, col = col.dens, fill = col.dens)
      }
      else {
         orig <- 0
         sgn  <- if (violin) sign(rbinom(length(x), 1, 0.5) - 0.5)
                 else 1
         if (length(x) >= plot.args$nmin) {
            d.dens <- data.frame(xgrid = dens$x, dgrid = dens$y)
            dmax   <- max(dens$y)
            dens.y <- approx(dens$x, dens$y, xout = x)$y
            dr.lo  <- if (violin) orig - d.dens$dgrid else 0
            plt <- plt +
               ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                                 ymin = dr.lo,
                                                 ymax = orig + dgrid),
                                    data = d.dens, col = NA, fill = col.dens, alpha = 0.25)
         }
         else {
            dens.y <- dnorm(x, mean(x), sd(x))
            dmax   <- dnorm(0, mean(x), sd(x))
         }
         d.densd <- data.frame(x = x, d = dens.y, sgn = sgn, r = runif(length(dens.y), 0, 1))
         sz  <- if (length(x) >= plot.args$nmin) 0.2 else 1
         plt <- plt +
            ggplot2::geom_point(ggplot2::aes(x, orig + sgn * r * d),
                                data = d.densd, size = sz)
      }
   }
   
   # Plot the uncertainty
   
   u.orig  <- if (violin) -2.3 * dmax else -1.3 * dmax
   ucol    <- if (uncertainty == 'reference') plot.args$col['reference']
              else plot.args$col['estimate']
   if (uncertainty != 'none') {
      urng    <- range(cntr - 4 * se, cntr + 4 * se)
      ngrid   <- 100
      xgrid   <- seq(urng[1], urng[2], length = ngrid)
      dens    <- dt((xgrid - cntr) / se, length(x) - 1) / dt(0, length(x) - 1)
      dgrd    <- data.frame(x = xgrid, y = 1, dens)
      height  <- dmax
      dstn.lo <- if (violin) u.orig - height * dens else u.orig
      plt <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                           ymin = dstn.lo, ymax = u.orig + height * dens),
                              data = dgrd, col = NA, fill = ucol)
   }
   
   # Plot the sample mean
   
   linestart <- if (violin) -3.4 * dmax else -1.3 * dmax
   lineend   <- if (plot.args$zoom) u.orig + 1.1 * dmax else 1.25 * dmax
   plt <- plt +
      ggplot2::annotate('segment', x = ttest$estimate, xend = ttest$estimate,
                        y = linestart, yend = lineend, col = plot.args$col['estline']) +
      ggplot2::annotate('text', x = ttest$estimate, y = lineend + 0.1 * dmax,
                        hjust = hjst(ttest$estimate, xlimits, 0.25),
                        label = 'sample mean', col = plot.args$col['estline'])
   
   # Plot reference if requested
   
   if (plot.args$reference | (uncertainty == 'reference')) {
      rlineend <- if (plot.args$zoom) u.orig + 1.05 * dmax else 1.20 * dmax
      plt  <- plt +
         ggplot2::annotate('segment', x = mu, xend = mu,
                           y = linestart, yend =  rlineend, col = plot.args$col['refline']) +
         ggplot2::annotate('text', x = mu, y = rlineend + 0.1 * dmax, label = 'reference',
                           hjust = hjst(mu, xlimits, 0.25), col = plot.args$col['refline'])
   }
   
   # Plot the uncertainty axis
   
   if (se.scale) {
      sedist <- if (uncertainty == 'reference') (mn - mu) / se
                else if (reference) (mu - mn) / se
                else NULL
      plt <- rp.add_sescale(plt, cntr, u.orig - 0.05, u.orig, se, plot.args$col['estline'], sedist)
   }
   
   print(plt)
   plt
}

rp.add_data_density <- function(plt, display, x, ypos, yht, hst, dens, scl, col) {
   
   violin <- (display == 'violin')
   if (!violin) ypos <- ypos - 0.5 * yht
   if (display == 'histogram') {
      # plt <- plt +
      #    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
      #                            breaks = hst$breaks, col = col.dens, fill = col)
      ngp <- length(hst$density)
      dfrm.h <- data.frame(x   = rep(mean(x), ngp), y = rep(ypos, ngp),
                           xlo = rev(rev(hst$breaks)[-1]), xhi = hst$breaks[-1],
                           ylo = rep(ypos, length(hst$breaks) - 1), yhi = ypos + hst$density * scl)
      plt <- plt +
         ggplot2::geom_rect(ggplot2::aes(xmin = xlo, xmax = xhi, ymin = ylo, ymax = yhi),
                            col = 'grey50', fill = col, data = dfrm.h)
   }
   else {
      sgn     <- if (violin) sign(rbinom(length(x), 1, 0.5) - 0.5) else 1
      d.dens  <- data.frame(xgrid = dens$x, dgrid = dens$y)
      dens.y  <- approx(dens$x, dens$y, xout = x)$y
      d.densd <- data.frame(x = x, d = dens.y, sgn = sgn, r = runif(length(dens.y), 0, 1))
      dr.lo   <- if (violin) ypos - d.dens$dgrid * scl else ypos
      plt <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                           ymin = dr.lo, ymax = ypos + dgrid * scl),
                              data = d.dens, col = NA, fill = col) +
         ggplot2::geom_point(ggplot2::aes(x, ypos + sgn * r * d * scl), data = d.densd)
   }

   plt
}

rp.add_sescale <- function(plt, xpos, ylo, yhi, se, col, sedist) {
   if (!is.null(sedist))
       sedist <- 2 * (ceiling(abs(sedist) / 2)) * sign(sedist)
   serange <- range(-4, 4, sedist)
   if (max(abs(serange)) <= 8) {
      tpos    <- xpos + (serange[1]:serange[2]) * se
      tlab    <- seq(serange[1], serange[2], by = 2)
   }
   else{
      tlab <- pretty(serange)
      tpos <- xpos + tlab * se
   }
   tscl    <- 0.10 * (yhi - ylo)
   drtpos  <- diff(range(tpos))
   plt  <- plt +
      ggplot2::annotate('rect', xmin = min(tpos) - drtpos / 10,
                        xmax = max(tpos) + drtpos / 10,
                        ymin = ylo, ymax = yhi,
                        # alpha = 0.7,
                        fill = 'white') +
      ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                        y = ylo, yend = ylo, col = col) +
      ggplot2::annotate('segment', x = tpos, xend = tpos,
                        y = ylo, yend = ylo + tscl, col = col) +
      ggplot2::annotate('text', x = xpos + tlab * se,
                        y = (ylo + yhi) / 2, col = col,
                        label = as.character(tlab)) +
      ggplot2::annotate('segment', x = tpos, xend = tpos,
                        y = yhi, yend = yhi - tscl, col = col) +
      ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                        y = yhi, col = col)
   plt
}

rp.add_uncertainty <- function(plt, display, xpos, ypos, yht, se, degf, col, dmax,
                               distribution = 't', ngrid = 100) {
   xgrid <- seq(xpos - 4 * se, xpos + 4 * se, length = ngrid)
   dgrid  <- data.frame(xgrid, ymin = ypos,
                        dens = if (distribution == 't') dt((xgrid - xpos) / se, degf)
                               else dnorm(xgrid, xpos, se))
   if (missing(dmax)) dmax <- max(dgrid$dens) 
   scl  <- yht / dmax
   if (display == 'violin') {
      ypos       <- ypos + yht / 2
      scl        <- scl / 2
      dgrid$ymin <- ypos - dgrid$dens * scl
   }
   plt <- plt +
      ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                        ymin = ymin, ymax = ypos + dens * scl),
                           data = dgrid, col = NA, fill = col)
   plt
}

rp.twosample <- function(x, y, ttest, ttest.args, plot.args) {

   # Ensure that x has values and y is a factor
   
   xlab       <- plot.args$xlab
   ylab       <- plot.args$ylab
   horizontal <- plot.args$horizontal
   xttl <- if (!is.numeric(y)) xlab else 'value'
   if (!is.numeric(x))
      stop('x must be numeric.')
   if (is.numeric(y)) {
      fac  <- factor(c(rep(xlab, length(x)),
                       rep(ylab, length(y))), levels = c(xlab, ylab))
      x    <- c(x, y)
      y    <- fac
   }
   else if (is.character(y) | is.logical(y)) 
      y <- factor(y)
   else
      if (!is.factor(y)) stop('y must be character, logical or a factor.')
   if (length(x) != length(y))
      stop('x and y have different lengths.')
   if (nlevels(y) != 2)
      stop('y should have only two levels.')
   
   # Setup

   display     <- plot.args$display
   violin      <- (display == 'violin')
   zoom        <- plot.args$zoom
   mns         <- ttest$estimate
   se          <- ttest$stderr
   mu          <- ttest$null.value
   uncertainty <- plot.args$uncertainty
   reference   <- plot.args$reference
   se.scale    <- ifelse (uncertainty != 'none', TRUE, plot.args$se.scale)
   height      <- plot.args$height
   clr         <- plot.args$col
   ht          <- c(main = 0.7, axis = 0.5, margin = 0.2)
   dpos        <- 2 * ht['margin'] + ht['main']
   fn          <- function(x) density(x, bw = bw.norm(x))
   dens        <- tapply(x, y, fn)
   if (display == 'histogram') {
      hst  <- tapply(x, y, hist, plot = FALSE)
      fn   <- function(x) max(x$density)
      dmax <- max(sapply(hst, fn))
   }
   else {
      if (length(x) >= plot.args$nmin)
         dmax <- max(dens[[1]]$y, dens[[2]]$y)
      else {
         fn   <- function(x) dnorm(0, 0, sd(x))
         dmax <- max(tapply(x, y, fn))
      }
   }
   dscl <- ht['main'] / dmax
   if (violin) dscl <- 0.5 * scl
   slo     <- 2 * ht['margin'] + ht['main'] + 0.05 * ht['axis']
   shi     <- slo + 0.3 * ht['axis']
   apos    <- c(ht['margin'] + 0.5 * ht['main'],
                ht['margin'] * 3 + ht['main'] * 1.5 + ht['axis'], dpos)
   alab    <- c(xlab, ylab, 'difference\nscale')
   if (se.scale) {
      apos <- c(apos, shi)
      alab <- c(alab, 'se scale')
   }
   ylimits <- c(0, 4 * ht['margin'] + 2 * ht['main'] + ht['axis'])
   xlimits <- range(dens[[1]]$x, dens[[2]]$x,
                    mns[1] + mu - 4 * se, mns[1] + mu + 4 * se, mns - 4 * se, mns + 4 * se)

   # Plot setup
   
   dfrm <- data.frame(x, as.numeric(y))
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y)) +
      ggplot2::theme(
         panel.grid.major.y = ggplot2::element_blank(),
         panel.grid.minor.y = ggplot2::element_blank(),
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = apos, labels = alab)
   if (!horizontal) plt <- plt + ggplot2::coord_flip() +
                                ggplot2::theme(axis.title.x = ggplot2::element_blank())
   else            plt <- plt + ggplot2::theme(axis.title.y = ggplot2::element_blank())

   # Plot the data

   if (!plot.args$zoom) {
      for (i in 1:2) {
         plt <- rp.add_data_density(plt, display, x[y == levels(y)[i]], apos[i],
                                    ht['main'], hst[[i]], dens[[i]], dscl, 'grey85')
      }
   }
   
   # Add the difference scale
   
   tpos <- pretty(x - mns[1])
   tlab <- as.character(tpos)
   tpos <- tpos + mns[1]
   tscl <- if (plot.args$zoom) 0.03 * ht['axis'] else 0.05 * ht['axis']
   if (violin) yp <- yp + 0.5 * ht['axis']
   plt <- plt +
      ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                        y = dpos, col = 'grey25') +
      ggplot2::annotate('segment', x = tpos, xend = tpos,
                        y = dpos, yend = dpos - tscl, col = 'grey25') +
      ggplot2::annotate('text', x = tpos, y = dpos - 2.5 * tscl,
                        label = tlab, col = 'grey25')
   
   # Plot the uncertainty axis
   
   if (se.scale) {
      # if (violin) spos <- spos + 0.5 * ht['axis']
      if (uncertainty == 'reference') {
         xpos   <- mns[1] + mu
         sclr   <- clr['refline']
         sedist <- (mns[2] - mns[1] - mu) / se
      }
      else {
         xpos   <- mns[2]
         sclr   <- clr['estline']
         sedist <- if (reference) (mu + mns[1] - mns[2]) / se else NULL
      }
      plt <- rp.add_sescale(plt, xpos, slo, shi, se, sclr, sedist)
   }
   
   # Add the uncertainty distribution
   
   if (uncertainty != 'none') {
      ucol <- if (uncertainty == 'sample mean') clr['estimate']
      else clr['reference']
      # plt  <- rp.add_uncertainty(plt, display, cntr, 2 * ht['margin'] + ht['main'], ht['axis'],
      #                            se, ttest$parameter, col = ucol)
      
      xpos  <- if (uncertainty == 'sample mean') mns[2] else mns[1] + mu
      ulo   <- shi
      uhi   <- 2 * ht['margin'] + ht['main'] + ht['axis']
      degf  <- ttest$parameter
      distribution <- 't'
      ngrid <- 100
      xgrid <- seq(xpos - 4 * se, xpos + 4 * se, length = ngrid)
      dgrid <- data.frame(xgrid, ymin = rep(ulo, length(xgrid)),
                          dens = if (distribution == 't') dt((xgrid - xpos) / se, degf)
                          else dnorm(xgrid, xpos, se))
      uscl  <- (uhi - ulo) / max(dgrid$dens)
      ucntr <- ulo
      if (violin) {
         ucntr <- (ulo + uhi) / 2
         uscl  <- uscl / 2
         dgrid$ymin <- ucntr - dgrid$dens * uscl
      }
      plt <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                           ymin = ymin, ymax = ucntr + dens * uscl),
                              data = dgrid, col = NA, fill = ucol)
   }
   
   # Plot the sample means, difference and reference, as required
   
   value     <- c(mns, mns[2])
   linestart <- c(0.5 * ht['margin'], 2.75 * ht['margin'] + ht['main'] + ht['axis'],
                  if (se.scale) shi else dpos)
   lineend   <- c(1.25 * ht['margin'] +     ht['main'],
                  3.5  * ht['margin'] + 2 * ht['main'] + ht['axis'],
                  linestart[2] - 0.5 * ht['margin'])
   gclr      <- c(rep('black', 2), clr['estline'])
   group     <- c(rep('sample mean', 2), 'sample mean difference')
   if (plot.args$reference | (uncertainty == 'reference')) {
      value     <- c(value,     mns[1] + mu)
      linestart <- c(linestart, linestart[3])
      lineend   <- c(lineend,   lineend[3])
      # linestart <- c(linestart, lineend[1] + 0.25 * ht['margin'])
      # lineend   <- c(lineend,   linestart[2] - 0.5 * ht['margin'])
      gclr      <- c(gclr,      clr['refline'])
      group     <- c(group,     'reference')
   }
   names(gclr) <- group
   dfrm.lines <- data.frame(value, linestart, lineend, gclr, group)
   n.lines <- nrow(dfrm.lines)
   plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = value, y = linestart, yend = lineend,
                                                 col = group), data = dfrm.lines) +
                ggplot2::scale_color_manual(name = '', breaks = group, values = gclr) +
                ggplot2::theme(legend.position = 'top')

   # Print and return
   
   print(plt)
   plt
}

hjst <- function(x, xlimits, edge) {
   hjst <- (x - xlimits[1]) / diff(xlimits)
   if (hjst < edge)     hjst <- 2 * hjst
   if (hjst > 1 - edge) hjst <- 1 - 2 * (1 - hjst)
   hjst
}

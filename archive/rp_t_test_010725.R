#     A function for a confidence interval and hypothesis test
#     on two samples of data

rp.t_test <- function(x, y = NULL, mu = NULL, display = 'density',
                     uncertainty = 'sample mean', scale = is.null(y), zoom = FALSE,
                     horizontal = TRUE,
                     col = '#86B875', refcol = '#E495A5',
                     xlab, ylab, ...) {
   
   if (!requireNamespace("ggplot2"))
      stop("the ggplot2 package is not available.")
   
   if (!(uncertainty %in% c('none', 'sample mean', 'reference')))
         uncertainty <- 'sample mean'
   ttest.args <- list(...)
   paired     <- if ('paired' %in% names(ttest.args)) ttest.args$paired else FALSE
   reference  <- !is.null(mu)
   if (!reference) mu <- 0
   
   ttest  <- t.test(x, y, mu = mu, ...)
   method <- if      (is.null(y)) "One"
             else if (!paired)    "Two"
             else                 "Paired"
   height <- c('data' = 0.25, 'uncertainty' = 0.15, 'mean' = 0.5)
   # if (missing(col)) {
   #    col <- if (method == 'Two')
   #       # col = c("#1FC8DEFF", "#F1CA3AFF", "#C1F334FF", "#BE2102FF"),
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

   plot.args <- list(scale = scale, col = clr, horizontal = horizontal,
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
   se          <- ttest$stderr
   hst         <- hist(x, plot = FALSE)
   dmax        <- max(hst$density)
   
   # Set up the plot
   
   dens    <- density(x, bw = bw.norm(x))
   xlimits <- range(mean(x) - 4 * se, mean(x) + 4 * se)
   if (!plot.args$zoom) xlimits <- range(xlimits, dens$x)
   if (plot.args$reference) xlimits <- range(xlimits, mu - 4 * se, mu + 4 * se)
   ticks <- if (violin) c(0, -1.5 * dmax) else c(0.5, -0.5) * dmax
   tind  <- if (uncertainty == 'none') 1 else 1:2
   dfrm  <- data.frame(x, gp = 1)
   plt <- ggplot2::ggplot(dfrm, ggplot2::aes(x, gp)) +
      ggplot2::theme(axis.text.y        = ggplot2::element_text(angle = 90, vjust = 0.5),
                     axis.ticks.y       = ggplot2::element_blank(),
                     axis.title.y       = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = ticks[tind],
                                  labels = c('data\n', 'uncertainty\nof the mean')[tind]) +
      ggplot2::xlim(xlimits[1], xlimits[2])
      
   
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
   
   if (uncertainty != 'none') {
      cntr    <- if (uncertainty == 'sample mean') ttest$estimate else mu
      ucol    <- if (uncertainty == 'sample mean') plot.args$col['estimate']
                 else plot.args$col['reference']
      clab    <- if (uncertainty == 'sample mean') 'sample mean uncertainty'
                 else 'reference uncertainty'
      urng    <- range(cntr - 4 * se, cntr + 4 * se)
      ngrid   <- 100
      xgrid   <- seq(urng[1], urng[2], length = ngrid)
      dens    <- dt((xgrid - cntr) / se, length(x) - 1) / dt(0, length(x) - 1)
      dgrd    <- data.frame(x = xgrid, y = 1, dens)
      u.orig  <- if (violin) -2.3 * dmax else -1.3 * dmax
      height  <- dmax
      dstn.lo <- if (violin) u.orig - height * dens else u.orig
      plt <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                           ymin = dstn.lo, ymax = u.orig + height * dens),
                              data = dgrd, col = NA, fill = ucol)
   }
   
   # Plot the sample mean
   
   linestart <- if (violin) -3.4 * dmax else -1.4 * dmax
   lineend   <- if (plot.args$zoom) u.orig + 1.1 * dmax else 1.25 * dmax
   plt <- plt +
      ggplot2::annotate("segment", x = ttest$estimate, xend = ttest$estimate,
                        y = linestart, yend = lineend, col = plot.args$col['estline']) +
      ggplot2::annotate('text', x = ttest$estimate, y = lineend + 0.1 * dmax,
                        hjust = hjst(ttest$estimate, xlimits, 0.25),
                        label = 'sample mean', col = plot.args$col['estline'])
   
   # Plot reference if requested
   
   if (plot.args$reference | (uncertainty == 'reference'))
      plt  <- plt +
         ggplot2::annotate("segment", x = mu, xend = mu,
                           y = linestart, yend =  1.05 * dmax,
                           col = plot.args$col['refline']) +
         ggplot2::annotate('text', x = mu, y = 1.15 * dmax, label = 'reference',
                           hjust = hjst(mu, xlimits, 0.25), col = plot.args$col['refline'])
   
   # Plot the uncertainty axis
   
   if ((uncertainty != 'none') & plot.args$scale) {
      tpos  <- cntr - (-4:4) * se
      mscl  <- if (violin) dmax else 0.5 * dmax
      tscl  <- if (plot.args$zoom) 0.03 else 0.05
      selab <- if (diff(range(tpos)) < 0.25 * diff(xlimits)) 'se scale' else
                     'standard error scale'
      plt  <- plt +
         ggplot2::annotate("segment", x = min(tpos), xend = max(tpos),
                           y = u.orig, yend = u.orig, col = 'grey25') +
         ggplot2::annotate("segment", x = tpos, xend = tpos,
                           y = u.orig, yend = u.orig - tscl * mscl, col = 'grey25') +
         ggplot2::annotate("text", x = cntr - c(-4, -2, 0, 2, 4) * se, col = 'grey25',
                           y = u.orig - 3 * tscl * mscl,
                           label = as.character(seq(-4, 4, by = 2))) +
         ggplot2::annotate("text",    x = cntr, col = 'grey25',
                           y = u.orig + 3 * tscl * mscl, label = selab)
   }
   
   # Plot the sample mean and +/- 2 se's
   # Current thinking is not to do this
   # notchx    <- cntr - notch * se
   # notchy0   <- 0.5 * height * dnorm(notchx, cntr, se) / dnorm(cntr, cntr, se)
   # notchx    <- rep(notchx, 2)
   # notchy    <- c(1 + notchy0, 1 - notchy0)
   # notchht   <- 0.25 * height
   # notchyend <- c(1 + notchy0 + notchht, 1 - notchy0  - notchht)
   # dnotch    <- data.frame(notchx, notchy, notchyend)
   # plt   <- plt +
   #    ggplot2::geom_segment(ggplot2::aes(x = notchx, xend = notchx,
   #                                       y = notchy, yend = notchyend),
   #                          col = notchcol, data = dnotch)
   
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
      dfrm.h <- data.frame(x = mean(x), y = ypos,
                           xlo = rev(rev(hst$breaks)[-1]), xhi = hst$breaks[-1],
                           ylo = ypos, yhi = ypos + hst$density * scl)
      plt <- plt +
         ggplot2::geom_rect(ggplot2::aes(xmin = xlo, xmax = xhi, ymin = ylo, ymax = yhi),
                            col = 'black', fill = col, data = dfrm.h)
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

rp.add_sescale <- function(plt, xpos, ypos, se, col, zoom, xlimits, yht) {
   tpos  <- xpos - (-4:4) * se
   tscl  <- if (zoom) 0.03 * yht else 0.05 * yht
   selab <- if (diff(range(tpos)) < 0.25 * diff(xlimits)) 'se scale'
            else 'standard error scale'
   plt  <- plt +
      ggplot2::annotate("segment", x = min(tpos), xend = max(tpos),
                        y = ypos, yend = ypos, col = col) +
      ggplot2::annotate("segment", x = tpos, xend = tpos,
                        y = ypos, yend = ypos - tscl, col = col) +
      ggplot2::annotate("text", x = xpos - c(-4, -2, 0, 2, 4) * se,
                        y = ypos - 3 * tscl, col = col,
                        label = as.character(seq(-4, 4, by = 2))) +
      ggplot2::annotate("text", x = xpos, col = col,
                        y = ypos + 3 * tscl, label = selab)
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
   mns         <- ttest$estimate
   mu          <- ttest$null.value
   uncertainty <- plot.args$uncertainty
   height      <- plot.args$height
   col         <- plot.args$col['estimate']
   col.dens    <- plot.args$col['density']
   ht.main     <- 0.7
   ht.axis     <- 0.5
   ht.margin   <- 0.2  
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
   scl <- ht.main / dmax
   if (violin) scl <- 0.5 * scl
         
   se      <- ttest$stderr
   # margin  <- if (display == 'violin') 0.7 * dmax else 0.35 * dmax
   # violadj <- if (display == 'violin') dmax else 0
   # orig    <- c(0, dmax + violadj + margin, -dmax - violadj - 3 * margin)
   ypos  <- c(ht.margin + 0.5 * ht.main,
              ht.margin * 3 + ht.main * 1.5 + ht.axis,
              ht.margin * 2 + ht.main + ht.axis * 0.5)
   # ytind   <- if (uncertainty == 'none') 1:2 else 1:3
   ylimits <- c(0, 4 * ht.margin + 2 * ht.main + ht.axis)
   xlimits <- range(dens[[1]]$x, dens[[2]]$x,
                    mns[1] + mu - 4 * se, mns[1] + mu + 4 * se, mns - 4 * se, mns + 4 * se)
   # xlimits <- range(mns[2] - 4 * se, mns[2] + 4 * se)
   # if (!plot.args$zoom)
   #    xlimits <- range(xlimits, dens[[1]]$x, dens[[2]]$x)
   # if (plot.args$reference)
   #    xlimits <- range(xlimits, mns[1] + mu - 4 * se, mns[1] + mu + 4 * se)
   # if (uncertainty == 'reference')
   #    xlimits <- range(xlimits, mns[1] + mu - 4 * se, mns[1] + mu + 4 * se)
   # ylimits <- if (plot.args$zoom) c(orig[3] - 0.25 * violadj - 1.5 * margin,
   #                                  orig[1] - violadj - 1.15 * margin)
   #            else c(orig[3] - 0.25 * violadj - 1.5 * margin,
   #                   orig[2] + dmax    + 1.0 * margin)
         
   # Plot setup
   
   dfrm <- data.frame(x, as.numeric(y))
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y)) +
      ggplot2::theme(
         # Assignment of multiple colours in axis.text.y is not officially supported
         # and generates a warning.
         # axis.text.y        = ggplot2::element_text(colour = c(rep('black', 2),
         #                                                       plot.args$col['estline'])),
         panel.grid.major.y = ggplot2::element_blank(),
         panel.grid.minor.y = ggplot2::element_blank(),
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = ypos,
                                  # expand = ggplot2::expansion(),
                                  # limits = ylimits,
                                  labels = c(xlab, ylab, 'difference\nin means'))
   if (!horizontal) plt <- plt + ggplot2::coord_flip() +
                                ggplot2::theme(axis.title.x = ggplot2::element_blank())
   else            plt <- plt + ggplot2::theme(axis.title.y = ggplot2::element_blank())

   # Plot the data

   if (!plot.args$zoom) {
      for (i in 1:2) {
         plt <- rp.add_data_density(plt, display, x[y == levels(y)[i]], ypos[i], ht.main, hst[[i]],
                                    dens[[i]], scl, 'grey85')
      }
   }
   
   # Add the uncertainty
   
   if (uncertainty != 'none') {
      cntr <- if (uncertainty == 'sample mean') mns[2] else mns[1] + mu
      ucol <- if (uncertainty == 'sample mean') plot.args$col['estimate']
              else plot.args$col['reference']
      plt  <- rp.add_uncertainty(plt, display, cntr, 2 * ht.margin + ht.main, ht.axis,
                                 se, ttest$parameter, col = ucol)
   }
   
   # Add the difference scale
   
   tpos <- pretty(x - mns[1])
   tlab <- as.character(tpos)
   tpos <- tpos + mns[1]
   tscl <- if (plot.args$zoom) 0.03 * ht.axis else 0.05 * ht.axis
   yp   <- 2 * ht.margin + ht.main
   if (violin) yp <- yp + 0.5 * ht.axis
   plt <- plt +
      ggplot2::annotate("segment", x = min(tpos), xend = max(tpos),
                        y = yp, yend = yp, col = 'grey25') +
      ggplot2::annotate("segment", x = tpos, xend = tpos,
                        y = yp, yend = yp - tscl, col = 'grey25') +
      ggplot2::annotate("text", x = tpos, y = yp - 2.5 * tscl,
                        label = tlab, col = 'grey25')
   
   # Plot the sample means
   
   linestart <- c(0.5 * ht.margin, 2.75 * ht.margin + ht.main + ht.axis)
   lineend   <- c(1.25 * ht.margin + ht.main, 3.5 * ht.margin + 2 * ht.main + ht.axis)

   if (!plot.args$zoom) {
      linelab <- c(linestart[1] - 0.5 * ht.margin, lineend[2] + 0.5 * ht.margin)
      angle   <- ifelse(horizontal, 0, 90)
      plt <- plt +
         ggplot2::annotate("segment", x = mns, xend = mns,
                           y = linestart, yend = lineend, col = 'grey25') +
         ggplot2::annotate('text', x = mns, y = linelab,
                           label = paste('sample mean (', c(xlab, ylab), ')', sep = ''),
                           col = 'grey25', angle = angle) +
         ggplot2::scale_color_manual(values = c('sample mean' = 'black'))
   }

   # Plot the sample mean difference
   
   if (!plot.args$zoom)
      # plt  <- plt +
      #    ggplot2::annotate("segment", x = mns[2], xend = mns[2],
      #                   y = ly, yend = ly - 0.4 * margin, col = plot.args$col['estimate'])
      plt <- plt +
         ggplot2::annotate("segment", x = mns[2], xend = mns[2],
                           y    = lineend[1] + 0.25 * ht.margin,
                           yend = linestart[2] - 0.5 * ht.margin,
                           col = plot.args$col['estline'])
         # ggplot2::annotate('text', x = mns[2], y = lineend[1] + 0.25 * ht.margin,
         #                   hjust = hjst(mns[2], xlimits, 0.1),
         #                   label = 'sample mean difference', col = plot.args$col['estline'])

   # Plot the reference, if requested
   
   if (plot.args$reference | (uncertainty == 'reference'))
      plt  <- plt +
         ggplot2::annotate("segment", x = mns[1] + mu, xend = mns[1] + mu,
                           y    = lineend[1] + 0.25 * ht.margin,
                           yend = linestart[2] - 0.5 * ht.margin,
                           col = plot.args$col['refline']) +
         ggplot2::annotate('text', x = mns[1] + mu, y = linestart[2] - 0.25 * ht.margin,
                           hjust = hjst(mns[1] + mu, xlimits, 0.1),
                           label = 'reference', col = plot.args$col['refline'])
   
   # Plot the uncertainty axis
   
   if ((uncertainty != 'none') & plot.args$scale) {
      ypos <- 2 * ht.margin + ht.main + 0.3 * ht.axis
      # if (violin) ypos <- ypos + 0.5 * ht.axis
      xpos <- ifelse(uncertainty == 'reference', mns[1], mns[2])
      clr  <- ifelse(uncertainty == 'reference', plot.args$col['refline'], plot.args$col['estline'])
      plt <- rp.add_sescale(plt, xpos, ypos, se, clr, plot.args$zoom, xlimits, ht.axis)
      
      # tpos  <- cntr + (-4:4) * se
      # dscl  <- if (display == 'violin') dmax else 0.5 * dmax
      # tscl  <- if (plot.args$zoom) 0.05 else 0.15
      # acol  <- 'grey25'
      # selab <- if (diff(range(tpos)) < 0.25 * diff(xlimits)) 'se scale' else
      #           'standard error scale'
      # plt  <- plt +
      #    ggplot2::annotate("segment", x = min(tpos), xend = max(tpos),
      #                      y = orig[3], yend = orig[3], col = acol) +
      #    ggplot2::annotate("segment", x = tpos, xend = tpos,
      #                         y = orig[3], yend = orig[3] - tscl * dscl,
      #                      col = acol) +
      #    ggplot2::annotate("text", x = cntr - c(-4, -2, 0, 2, 4) * se,
      #                      y = orig[3] - 2 * tscl * dscl,
      #                      vjust = 'top', label = as.character(seq(-4, 4, by = 2)),
      #                      col = acol) +
      #    ggplot2::annotate("text",  x = cntr, y = orig[3] + 2 * tscl * dscl,
      #                      col = acol, vjust = 'bottom', label = selab)
   }

   print(plt)
   plt
}

hjst <- function(x, xlimits, edge) {
   hjst <- (x - xlimits[1]) / diff(xlimits)
   if (hjst < edge)     hjst <- 2 * hjst
   if (hjst > 1 - edge) hjst <- 1 - 2 * (1 - hjst)
   hjst
}

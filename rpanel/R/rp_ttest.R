#     A function for a confidence interval and hypothesis test
#     on two samples of data

rp.ttest <- function(x, y = NULL, mu = NULL, display = 'density',
                     uncertainty = 'sample mean', scale = TRUE, zoom = FALSE,
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
   if (missing(col))    col    <- '#86B875'
   if (missing(refcol)) refcol <- '#E495A5'
   if (missing(xlab))   xlab <- deparse(substitute(x))
   ylab <- if ((method == 'Two') & missing(ylab)) deparse(substitute(y)) else NA

   plot.args <- list(scale = scale, col = col, refcol = refcol,
                     height = height, display = display, zoom = zoom,
                     uncertainty = uncertainty, reference = reference,
                     xlab = xlab, ylab = ylab, nmin = 10, col.dens = 'grey75')

   if (method == 'Paired') x <- x - y
   plt <- if (method %in% c('One', 'Paired'))
               rp.onesample(x, ttest, ttest.args, plot.args)
          else rp.twosample(x, y, ttest, ttest.args, plot.args)
   
   invisible(plt)
}


rp.onesample <- function(x, ttest, ttest.args, plot.args) {
   
   display     <- plot.args$display
   uncertainty <- plot.args$uncertainty
   col.dens    <- plot.args$col.dens
   mu          <- ttest$null.value
   se          <- ttest$stderr
   hst         <- hist(x, plot = FALSE)
   dmax        <- max(hst$density)
   
   # Set up the plot
   
   dens  <- density(x, bw = bw.norm(x))
   rng   <- range(mean(x) - 4 * se, mean(x) + 4 * se)
   if (!plot.args$zoom) rng <- range(rng, dens$x)
   if (plot.args$reference) rng <- range(rng, mu - 4 * se, mu + 4 * se)
   ticks <- if (display == 'violin') c(0, -1.5 * dmax) else c(0.5, -0.5) * dmax
   tind  <- if (uncertainty == 'none') 1 else 1:2
   dfrm  <- data.frame(x, gp = 1)
   plt <- ggplot2::ggplot(dfrm, ggplot2::aes(x, gp)) +
      ggplot2::theme(
                     axis.text.y        = ggplot2::element_text(angle = 90, vjust = 0.5),
                     axis.ticks.y       = ggplot2::element_blank(),
                     axis.title.y       = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = ticks[tind],
                                  labels = c('data\n', 'uncertainty\nof the mean')[tind]) +
      ggplot2::xlim(rng[1], rng[2])
      
   
   # Plot the data
   
   if (!plot.args$zoom) {
      if (display == 'histogram') {
         plt <- plt +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                    breaks = hst$breaks, col = col.dens, fill = col.dens)
      }
      else {
         orig <- 0
         sgn  <- if (display == 'violin') sign(rbinom(length(x), 1, 0.5) - 0.5)
                 else 1
         if (length(x) >= plot.args$nmin) {
            d.dens <- data.frame(xgrid = dens$x, dgrid = dens$y)
            dmax   <- max(dens$y)
            dens.y <- approx(dens$x, dens$y, xout = x)$y
            dr.lo  <- if (display == 'violin') orig - d.dens$dgrid else 0
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
   
   u.alpha <- if (uncertainty == 'none') 0 else 1
   cntr    <- if (uncertainty == 'sample mean') ttest$estimate else mu
   ucol    <- if (uncertainty == 'sample mean') plot.args$col
              else plot.args$refcol
   clab    <- if (uncertainty == 'sample mean') 'sample mean uncertainty'
              else 'reference uncertainty'
   rng     <- range(cntr - 4 * se, cntr + 4 * se)
   ngrid   <- 100
   xgrid   <- seq(rng[1], rng[2], length = ngrid)
   dens    <- dt((xgrid - cntr) / se, length(x) - 1) / dt(0, length(x) - 1)
   dgrd    <- data.frame(x = xgrid, y = 1, dens)
   u.orig  <- if (display == 'violin') -2.3 * dmax else -1.3 * dmax
   height  <- dmax
   dstn.lo <- if (display == 'violin') u.orig - height * dens else u.orig
   plt <- plt +
      ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                        ymin = dstn.lo, ymax = u.orig + height * dens),
                           data = dgrd, col = NA, fill = ucol, alpha = u.alpha)
      # ggplot2::scale_fill_manual(values = ucol, labels = clab,
      #                            guide  = ggplot2::guide_legend(title = NULL,
      #                                                        position = "top"))

   # Plot the sample mean
   
   linestart <- if (display == 'violin') -3.4 * dmax else -1.4 * dmax
   lineend   <- if (plot.args$zoom) u.orig + 1.1 * dmax else 1.25 * dmax
   plt <- plt +
      ggplot2::annotate("segment", x = ttest$estimate, xend = ttest$estimate,
                        y = linestart, yend = lineend, col = plot.args$col) +
      ggplot2::annotate('text', x = ttest$estimate, y = lineend + 0.1 * dmax,
                        label = 'sample mean', col = plot.args$col)
   
   # Plot reference if requested
   
   if (plot.args$reference | (uncertainty == 'reference')) {
      plt  <- plt +
         ggplot2::annotate("segment", x = mu, xend = mu,
                           y = linestart, yend =  1.05 * dmax,
                           col = plot.args$refcol) +
         ggplot2::annotate('text', x = mu, y = 1.15 * dmax, label = 'reference',
                           col = plot.args$refcol)
   }
   
   # Plot the uncertainty axis
   
   if ((uncertainty != 'none') & plot.args$scale) {
      tpos <- cntr - (-4:4) * se
      mscl <- if (display == 'violin') dmax else 0.5 * dmax
      plt  <- plt +
         ggplot2::annotate("segment", x = rng[1], xend = rng[2],
                           y = u.orig, yend = u.orig, col = 'grey25') +
         ggplot2::annotate("segment", x = tpos, xend = tpos,
                           y = u.orig, yend = u.orig - 0.05 * mscl, col = 'grey25') +
         ggplot2::annotate("text", x = cntr - c(-4, -2, 0, 2, 4) * se, col = 'grey25',
                           y = u.orig - 0.15 * mscl,
                           label = as.character(seq(-4, 4, by = 2))) +
         ggplot2::annotate("text",    x = cntr, col = 'grey25',
                           y = u.orig + 0.15 * mscl, label = 'standard error scale')
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

rp.twosample <- function(x, y, ttest, ttest.args, plot.args) {

   # Ensure that x has values and y is a factor
   
   xlab <- plot.args$xlab
   ylab <- plot.args$ylab
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
   
   # formula case
   
   
   # Setup

   display     <- plot.args$display
   mns         <- ttest$estimate
   mu          <- ttest$null.value
   uncertainty <- plot.args$uncertainty
   height      <- plot.args$height
   col         <- plot.args$col
   col.dens    <- plot.args$col.dens
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
         
   se      <- ttest$stderr
   margin  <- if (display == 'violin') 0.7 * dmax else 0.35 * dmax
   violadj <- if (display == 'violin') dmax else 0
   orig    <- c(0, dmax + violadj + margin, -dmax - violadj - 3 * margin)
   yticks  <- orig + 0.5 * (dmax - violadj)
   ytind   <- if (uncertainty == 'none') 1:2 else 1:3
   # xlimits <- range(dens[[1]]$x, dens[[2]]$x,
   #                  mns[1] + mu - 4 * se, mns[1] + mu + 4 * se, mns - 4 * se, mns + 4 * se)
   xlimits <- range(mns[2] - 4 * se, mns[2] + 4 * se)
   if (!plot.args$zoom)
      xlimits <- range(xlimits, dens[[1]]$x, dens[[2]]$x)
   if (plot.args$reference)
      xlimits <- range(xlimits, mns[1] + mu - 4 * se, mns[1] + mu + 4 * se)
   if (uncertainty == 'reference')
      xlimits <- range(xlimits, mns[1] + mu - 4 * se, mns[1] + mu + 4 * se)
   # xlimits <- if (plot.args$zoom) range(mns[2] - 4 * se, mns[2] + 4 * se,
   #                                      mns[1] + mu - 4 * se, mns[1] + mu + 4 * se)
   #            else rng
   ylimits <- if (plot.args$zoom) c(orig[3] - 0.25 * violadj - 1.5 * margin,
                                    orig[1] - violadj - 1.15 * margin)
              else c(orig[3] - 0.25 * violadj - 1.5 * margin,
                     orig[2] + dmax    + 1.0 * margin)
         
   # Set up the plot
   
   dfrm <- data.frame(x, y = 0, gp = y)
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y)) +
      ggplot2::theme(
            axis.text.y        = ggplot2::element_text(angle = 90, vjust = 0.5),
            axis.ticks.y       = ggplot2::element_blank(),
            axis.title.y       = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = yticks[ytind], expand = ggplot2::expansion(),
                                  limits = ylimits,
                                  labels = c(paste(xlab, '\n'), paste(ylab, '\n'),
                                             'uncertainty\nof the mean')[ytind])
   if (!plot.args$zoom) plt <- plt + 
      ggplot2::scale_x_continuous(xttl, position = 'top', limits = xlimits,
                                  sec.axis = ggplot2::sec_axis(transform = ~ . - mns[1],
                                                               name = "mean difference"))
   else plt <- plt +
      ggplot2::scale_x_continuous(position = 'top', limits = xlimits,
                                  sec.axis = ggplot2::sec_axis(transform = ~ . - mns[1],
                                                               name = "mean difference")) +
      ggplot2::theme(axis.text.x.top  = ggplot2::element_blank(),
                     axis.ticks.x.top = ggplot2::element_blank(),
                     axis.title.x.top = ggplot2::element_blank())
      
   # Plot the data

   for (i in 1:2) {
      if (display == 'histogram') {
         dfrm.h <- data.frame(x = mns[1], y = 0,
                              xlo = rev(rev(hst[[i]]$breaks)[-1]), xhi = hst[[i]]$breaks[-1],
                              ylo = orig[i], yhi = orig[i] + hst[[i]]$density)
         plt <- plt +
            ggplot2::geom_rect(ggplot2::aes(xmin = xlo, xmax = xhi, ymin = ylo, ymax = yhi),
                               col = col.dens, fill = col.dens, data = dfrm.h)
      }
      else {
         xi <- x[y == levels(y)[i]]
         if (length(xi) >= plot.args$nmin) {
            d.dens <- data.frame(xgrid = dens[[i]]$x, dgrid = orig[i] + dens[[i]]$y,
                         lo = if (display == 'violin') orig[i] - dens[[i]]$y else orig[i])
            dens.y <- approx(dens[[i]]$x, dens[[i]]$y, xout = xi)$y
            plt <- plt +
               ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0, ymin = lo, ymax = dgrid),
                                    data = d.dens, col = NA, fill = col.dens, alpha = 0.25)
         }
         else
            dens.y <- dnorm(xi, mean(xi), sd(xi))
         sgn  <- if (display == 'violin') sign(rbinom(length(xi), 1, 0.5) - 0.5) else 1
         d.densd <- data.frame(x = xi, y = orig[i] + sgn * runif(length(dens.y), 0, 1) * dens.y)
         sz  <- if (length(x) >= plot.args$nmin) 0.2 else 1
         plt <- plt +
            ggplot2::geom_point(ggplot2::aes(x, y), data = d.densd, size = sz)
      }
   }
   
   # Plot the uncertainty
   
   if (uncertainty != 'none') {
      cntr    <- if (uncertainty == 'sample mean') mns[2] else mns[1] + mu
      ucol    <- if (uncertainty == 'sample mean') plot.args$col else plot.args$refcol
      clab    <- if (uncertainty == 'sample mean') 'sample mean uncertainty'
                 else 'reference uncertainty'
      ngrid   <- 100
      xgrid   <- seq(cntr - 4 * se, cntr + 4 * se, length = ngrid)
      dens    <- dt((xgrid - cntr) / se, length(x) - 1) / dt(0, length(x) - 1)
      dgrd    <- data.frame(x = xgrid, y = 1, dens = dens)
      plt <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                           ymin = orig[3] - violadj * dens,
                                           ymax = orig[3] + dmax * dens),
                              data = dgrd, col = NA, fill = ucol)
   }
   
   # Plot the sample means
   
   linestart <- orig[1:2] + dmax + 0.25 * margin
   lineend   <- orig[1] - violadj - 0.25 * margin
   plt <- plt +
      ggplot2::annotate("segment", x = mns, xend = mns,
                        y = linestart, yend = lineend, col = 'grey25') +
      ggplot2::annotate('text', x = mns, y = linestart + 0.25 * margin,
                        label = paste('sample mean (', c(xlab, ylab), ')', sep = ''),
                        col = 'grey25') +
      ggplot2::annotate('rect', xmin = -Inf, xmax = Inf,
                        ymin = lineend - 0.5 * margin, ymax = lineend,
                        col = NA, fill = 'white') +
      ggplot2::annotate("segment", x = mns[1], xend = mns[1],
                        y = lineend - 0.5 * margin, yend = -Inf, col = 'white')
      # ggplot2::annotate("segment", x = mns[2], xend = mns[1],
      #                   y    = orig[1] - violadj + 0.25 * margin,
      #                   yend = orig[3] - violadj - 1.5  * margin, col = 'white') +
      
      # ggplot2::annotate('text', x = mns, y = orig[1] - violadj + 0.5 * margin,
      #                   label = paste('sample mean (', c(xlab, ylab), ')', sep = ''),
      #                   col = 'grey25')
      
   # Plot the sample mean difference
   
   ly <- orig[1] - violadj - 0.75 * margin
   plt  <- plt +
      ggplot2::annotate("segment", x = mns[2], xend = mns[2],
                        y = ly, yend = ly - 0.4 * margin, col = plot.args$col) +
      ggplot2::annotate("segment", x = mns[2], xend = mns[2],
                        y = ly - 1.1 * margin, yend = -Inf, col = plot.args$col) +
      ggplot2::annotate('text', x = mns[2], y = ly - 0.75 * margin,
                        label = 'sample mean difference', col = plot.args$col)

   # Plot the reference, if requested
   
   if (plot.args$reference | (uncertainty == 'reference')) {
      plt  <- plt +
         ggplot2::annotate("segment", x = mns[1] + mu, xend = mns[1] + mu,
                           y = ly - 1.6 * margin, yend = -Inf,
                           col = plot.args$refcol) +
         ggplot2::annotate('text', x = mns[1] + mu, y = ly - 1.25 * margin,
                           label = 'reference', col = plot.args$refcol)
   }
   
   # Plot the uncertainty axis
   
   if ((uncertainty != 'none') & plot.args$scale) {
      tpos <- cntr + (-4:4) * se
      dscl <- if (display == 'violin') dmax else 0.5 * dmax
      acol <- 'grey25'
      plt  <- plt +
         ggplot2::annotate("segment", x = min(tpos), xend = max(tpos),
                           y = orig[3], yend = orig[3], col = acol) +
         ggplot2::annotate("segment", x = tpos, xend = tpos,
                              y = orig[3], yend = orig[3] - 0.1 * dscl,
                           col = acol) +
         ggplot2::annotate("text", x = cntr - c(-4, -2, 0, 2, 4) * se,
                           y = orig[3] - 0.15 * dscl,
                           vjust = 'top', label = as.character(seq(-4, 4, by = 2)),
                           col = acol) +
         ggplot2::annotate("text",  x = cntr, y = orig[3] + 0.15 * dscl,
                           col = acol, vjust = 'bottom',
                           label = 'standard error scale')
   }
   
   print(plt)
   plt
}

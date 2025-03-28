#     A function for a confidence interval and hypothesis test
#     on two samples of data

rp.ttest <- function(x, y = NULL, mu = NULL, display = 'density',
                     uncertainty = 'sample mean',
                     scale = TRUE, col = '#86B875', refcol = '#E495A5',
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
   if (missing(col)) {
      col <- if (method == 'Two')
         # col = c("#1FC8DEFF", "#F1CA3AFF", "#C1F334FF", "#BE2102FF"),
         col = c(grey(0.5), grey(0.5), '#86B875', '#E495A5')
         else col = '#86B875'
   }
   if (missing(refcol)) refcol <- '#E495A5'
   if (missing(xlab)) xlab <- deparse(substitute(x))
   ylab <- if ((method == 'Two') & missing(ylab)) deparse(substitute(y)) else NA

   plot.args <- list(scale = scale, col = col, refcol = refcol,
                     height = height, display = display,
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
   rng   <- range(dens$x, mu - 4 * se, mu + 4 * se, mean(x) - 4 * se, mean(x) + 4 * se)
   ticks <- if (display == 'violin') c(0, -1.5 * dmax) else c(0.5, -0.5) * dmax
   tind  <- if (uncertainty == 'none') 1 else 1:2
   dfrm  <- data.frame(x, gp = 1)
   plt <- ggplot2::ggplot(dfrm, ggplot2::aes(x, gp)) +
      ggplot2::theme(
                     # axis.text.y        = ggplot2::element_blank(),
                     axis.text.y        = element_text(angle = 90),
                     axis.ticks.y       = ggplot2::element_blank(),
                     axis.title.y       = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = ticks[tind],
                                  labels = c('data\n', 'uncertainty\nof the mean')[tind])
      ggplot2::xlim(rng[1], rng[2])
      
   
   # Plot the data
   
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
   plt <- plt +
      ggplot2::annotate("segment", x = ttest$estimate, xend = ttest$estimate,
                        y = linestart, yend =  1.25 * dmax, col = plot.args$col) +
      ggplot2::annotate('text', x = ttest$estimate, y = 1.35 * dmax,
                        label = 'sample mean', col = plot.args$col)
   
   # Plot reference if requested
   
   if (plot.args$reference | (uncertainty == 'reference')) {
      plt  <- plt +
         ggplot2::annotate("segment", x = mu, xend = mu,
                           y = linestart, yend =  1.05 * dmax, col = plot.args$refcol) +
         ggplot2::annotate('text', x = mu, y = 1.15 * dmax, label = 'reference',
                           col = plot.args$refcol)
         # ggplot2::scale_colour_manual(values = plot.args$refcol,
         #                              labels = 'reference',
         #                              guide  = ggplot2::guide_legend(title = NULL,
         #                                                             position = "top"))
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
         ggplot2::annotate("text",    x = cntr - c(-4, -2, 0, 2, 4) * se, col = 'grey25',
                           y = u.orig - 0.15 * mscl, label = as.character(seq(-4, 4, by = 2))) +
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

   if (!requireNamespace('ggplot2', quietly = TRUE))
      stop("the ggplot2 package is not available.")
   
   mns         <- ttest$estimate
   reference   <- ttest$null.value
   uncertainty <- plot.args$uncertainty
   height      <- plot.args$height
   col         <- plot.args$col
   xlab        <- plot.args$xlab
   ylab        <- plot.args$ylab

   # Ensure that x has values and y is a factor
   
   if (!is.numeric(x))
      stop('x must be numeric.')
   if (is.numeric(y)) {
      fac  <- factor(c(rep(xlab, length(x)),
                       rep(ylab, length(y))))
      x    <- c(x, y)
      y    <- fac
      xlab <- 'value'
      ylab <- 'groups'
   }
   else if (is.character(y) | is.logical(y))
      y <- factor(y)
   else
      stop('y must be character, logical or a factor.')
   if (length(x) != length(y))
      stop('x and y have different lengths.')
   if (nlevels(y) != 2)
      stop('y should have only two levels.')
   
   # formula case


   # Plot the data
   
   dfrm <- data.frame(x, y)
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y)) +
           ggplot2::ylab('groups')

   if (requireNamespace('ggforce', quietly = TRUE))
      plt <- plt +
           ggforce::geom_sina(jitter_y = FALSE,
                              maxwidth = 2 * plot.args$height['data'])
   else
      plt <- plt + ggplot2::geom_jitter(height = 0.2, width = 0)
   plt <- plt +
      ggplot2::geom_violin(scale = 'width', width = 0.5,
                           col = NA, fill = 'grey75', alpha = 0.25)
   
   # Add the difference axis
   drng.min <- min(ttest$estimate, ttest$estimate - 4 * ttest$stderr)
   drng.max <- max(ttest$estimate, ttest$estimate + 4 * ttest$stderr)
   ticks <- pretty(c(drng.min, drng.max) - mns[1])
   plt <- plt +
      ggplot2::annotate('segment', x    = min(ticks) + mns[1],
                                   xend = max(ticks) + mns[1], y = 1.5) +
      ggplot2::annotate('segment', x = ticks + mns[1], y = 1.5, yend = 1.47) +
      ggplot2::annotate('text', x = ticks + mns[1], y = 1.46,
                        label = as.character(ticks), vjust = 'top')
      

   # If a reference is provided, plot this or the sample mean
   
   # Plot the uncertainty
   
   mns    <- tapply(x, y, mean, na.rm = TRUE)
   dfrm <- data.frame(xpos = c(mns, mns[2], mns[1] + reference),
                      ypos = c(1, 2, 1.5, 1.5), colcode = factor(1:4),
                      labels = c(levels(y), 'difference', 'reference'))
   dfrm <- dfrm[!is.na(dfrm$xpos), ]
   plt  <- plt +
      ggplot2::annotate('segment', x = mns, xend = mns,
                        y = c(0.6, 1.5), yend = c(1.5, 2.4),
                        col = col[2:3], linewidth = 1)
   cntr  <- if (uncertainty == 'sample mean') diff(mns) else reference
   cntr  <- c(mns, cntr + mns[1])
   n     <- c(tapply(x, y, length), length(x))
   sds   <- tapply(x, y, function(z) sd(z))
   se    <- tapply(x, y, function(z) sd(z) / sqrt(length(z)))
   sp    <- sqrt((sds[1]^2 * (n[1] - 1) + sds[2]^2 * (n[2] - 1)) / (n[1] + n[2] - 2))
   se    <- c(se, sp * sqrt((1 / n[1] + 1 / n[2])))
   fcol  <- if (uncertainty == 'sample mean') col[3] else col[4]
   clab  <- if (uncertainty == 'sample mean') 'difference' else 'reference'
   labels  <- c(levels(y), clab)
   colcode <- factor(1:3)
   fn <- function(cntr, se, n) {
      rng   <- range(cntr - 4 * se, cntr + 4 * se)
      ngrid <- 100
      xgrid <- seq(rng[1], rng[2], length = ngrid)
      dens  <- dt((xgrid - cntr) / se, n - 1)
      list(xgrid, dens)
   }
   mat  <- mapply(fn, cntr, se, n)
   hunc <- height['uncertainty']
   dgrd <- data.frame(xgrid1 = mat[[1]],
                      xgrid2 = mat[[3]],
                      xgrid3 = mat[[5]],
                      ymin1  = 1   - hunc * mat[[2]] / dt(0, n[3] - 1),
                      ymin2  = 2   - hunc * mat[[4]] / dt(0, n[3] - 1),
                      ymin3  = 1.5 - hunc * mat[[6]] / dt(0, n[3] - 1),
                      ymax1  = 1   + hunc * mat[[2]] / dt(0, n[3] - 1),
                      ymax2  = 2   + hunc * mat[[4]] / dt(0, n[3] - 1),
                      ymax3  = 1.5 + hunc * mat[[6]] / dt(0, n[3] - 1))
   plt   <- plt +
      ggplot2::geom_ribbon(ggplot2::aes(x = xgrid3, y = 1.5,
                                        ymin = ymin3, ymax = ymax3),
                           fill = fcol, alpha = 0.7, data = dgrd)
   if (!is.null(reference) & !is.na(reference)) {
      label <- if (uncertainty == 'sample mean') 'reference' else 'difference'
      clr   <- if (uncertainty == 'sample mean') col[4] else col[3]
      cntr  <- if (uncertainty == 'sample mean') reference else diff(mns)
      cntr  <- cntr + mns[1]
   }
      
   # Plot the uncertainty axis
   if (plot.args$scale & (uncertainty != 'none')) {
      scale.fn <- function(plt, cntr, se, col, ypos, ht) {
         rng  <- range(cntr - 4 * se, cntr + 4 * se)
         tpos <- cntr - (-4:4) * se
         plt +
            ggplot2::annotate("segment", x = min(tpos), xend = max(tpos),
                              y = ypos, yend = ypos, col = col) +
            ggplot2::annotate("segment", x = tpos, xend = tpos, col = col,
                              y = ypos, yend = ypos - 0.2 * ht) +
            ggplot2::annotate("text",    x = cntr - c(-4, -2, 0, 2, 4) * se,
                              y = ypos - 0.5 * ht,
                              label = as.character(seq(-4, 4, by = 2)),
                              col = col)
      }
      htu  <- height['uncertainty']
      plt  <- scale.fn(plt, mns[1], se[1], col[1], 1 - 1.2 * htu, htu)
      plt  <- scale.fn(plt, mns[2], se[2], col[2], 2 - 1.2 * htu, htu)
      cntr <- if (uncertainty == 'sample mean') diff(mns) else reference
      plt  <- scale.fn(plt, cntr + mns[1], se[3], fcol, 3 - 1.2 * htu, htu)
      plt
   }

   print(plt)
   plt
}

#     A function for a confidence interval and hypothesis test
#     on two samples of data

rp.t_test <- function(x, y = NULL, panel = TRUE, mu = NULL, data.display = 'density',
                     display = c(se.scale = FALSE, distribution = FALSE, ci = FALSE,
                                 pvalue = FALSE),
                     position = 'none',  candidate, zoom = FALSE,
                     clr, xlab, ylab, vlab, hscale = 1, vscale = hscale, ...) {
   
   if (!requireNamespace('ggplot2'))
      stop('the ggplot2 package is not available.')
   
   # Formula input for x
   if ('formula' %in% class(x)) {
      model <- lm(x, model = TRUE, ...)
      trms  <- attr(model$terms, 'term.labels')
      if (length(trms) > 1) stop('there should only be one predictor variable.')
      var.types    <- attr(model$terms, 'dataClasses')
      response.ind <- attr(model$terms, 'response')
      x            <- model$model[ , response.ind]
      if (class(x) == 'Pair') {
         x <- apply(x, 1, diff)
         if (missing(vlab)) vlab <- 'Difference'
      }
      if (length(trms) > 0) {
         factor.ind   <- which(var.types == 'factor')
         if (length(factor.ind) != 1) stop('the predictor variable should be a single factor.')
         y <- model$model[ , trms[1]]
         if (nlevels(y) != 2) stop('the predictor variable should have two levels.')
      }
      if (missing(vlab)) vlab <- names(model$model)[response.ind]
   }
   
   # x numeric and y character or a factor and set up labels
   if (is.character(y)) y <- factor(y)
   if (is.numeric(x) & is.factor(y)) {
      if (nlevels(y) > 2) stop('y is a factor with more than two levels.')
      if (missing(xlab)) xlab <- levels(y)[1]
      if (missing(ylab)) ylab <- levels(y)[2]
      if (missing(vlab)) vlab <- deparse(substitute(x))
      v <- x
      g <- y
      x <- v[g == levels(y)[1]]
      y <- v[g == levels(y)[2]]
   }
   else if (!is.null(y)) {
      if (missing(xlab)) xlab <- deparse(substitute(x))
      if (missing(ylab)) ylab <- deparse(substitute(y))
      if (missing(vlab)) vlab <- 'Value'
   }
   else {
      if (missing(vlab)) vlab <- deparse(substitute(x))
      xlab <- NA
      ylab <- NA
   }
   
   if (!(data.display %in% c('density', 'histogram'))) {
      warning("display not recognised - reverting to 'density'.")
      data.display <- 'density'
   }
   if (!(position %in% c('none', 'candidate', 'sample mean', 'reference')))
      position <- 'none'
   reference  <- !is.null(mu)
   if (!reference) mu <- 0
   horizontal <- TRUE
   height <- c('data' = 0.25, 'uncertainty' = 0.15, 'mean' = 0.5)
   
   ttest.args <- list(...)
   paired     <- if ('paired' %in% names(ttest.args)) ttest.args$paired else FALSE
   method     <- if      (is.null(y)) 'One'
                 else if (!paired)    'Two'
                 else                 'Paired'
   if (method == 'Paired') {
      x <- x - y
      y <- NULL
   }
   
   clr    <- c(estimate  = '#B3CDE3', estline = '#0093FF',
               reference = '#FBB4AE', refline = '#FF7F00',
               points    = 'grey50',  notch   = 'black',
               density   = 'grey75')
   bgdcol <- "grey85"
   seed   <- round(runif(1) * 100000)

   ttest  <- t.test(x, y, mu = mu, ...)
   print(ttest)

   if (missing(candidate)) {
      candidate <- ifelse(method == 'Two', ttest$estimate[2], ttest$estimate[1])
      candidate <- candidate + 4 * ttest$stderr * runif(1, -1, 1)
   }
   
   if (panel) {
      pnl <- rp.control(x = x, y = y, col = clr,
                  height = height, display = display, zoom = zoom,
                  data.display = data.display, seed = seed,
                  candidate = candidate,
                  conf = attr(ttest$conf.int, 'conf.level'), static = !panel,
                  position = position, reference = reference,
                  xlab = xlab, ylab = ylab, vlab = vlab, nmin = 10, bgdcol = bgdcol,
                  distribution = 't', horizontal = horizontal, ttest = ttest)
      rp.grid(pnl, "controls", row = 0, column = 0, background = bgdcol)
      rp.grid(pnl, "picture",  row = 0, column = 1, background = "white")
      if (method %in% c('One', 'Paired')) {
         rp.tkrplot(pnl, plot, rp.onesample,
                    hscale = hscale, vscale = vscale, 
                    grid = "picture", row = 0, column = 0, background = "white")
         rp.radiogroup(pnl, position, c('none', 'candidate', 'sample mean', 'reference'),
                       initval = position, action = rp.onesample.redraw,
                       grid = "controls", row = 0, column = 0)
         rp.slider(pnl, candidate, ttest$estimate -6 * ttest$stderr,
                   ttest$estimate + 6 * ttest$stderr, action = rp.onesample.redraw,
                   grid = "controls", row = 1, column = 0)
rp.checkbox(pnl, display, rp.onesample.redraw,
                     c('se.scale', 'distribution', 'ci', 'pvalue'),
                     title = 'display',
                     grid = "controls", row = 2, column = 0)
      }
   }
   else{
      pnl <- list(x = x, y = y, col = clr, horizontal = horizontal,
                  height = height, display = display, zoom = zoom,
                  data.display = data.display, seed = seed,
                  candidate = candidate,
                  conf = attr(ttest$conf.int, 'conf.level'), static = !panel,
                  position = position, reference = reference,
                  xlab = xlab, ylab = ylab, vlab = vlab, nmin = 10, bgdcol = bgdcol,
                  distribution = 't', ttest = ttest)
      plt <- if (method %in% c('One', 'Paired')) rp.onesample(pnl)
             else rp.twosample(pnl)
      invisible(plt)
   }
}


rp.onesample <- function(lst) {
   
   x           <- lst$x
   display     <- lst$display
   position    <- lst$position
   col.dens    <- lst$col['density']
   mu          <- lst$ttest$null.value
   estimate    <- lst$ttest$estimate
   se          <- lst$ttest$stderr
   se.scale    <- lst$display['se.scale']
   ci          <- ifelse(position == 'sample mean', lst$display['ci'], FALSE)
   pvalue      <- ifelse(position == 'reference',   lst$display['pvalue'], FALSE)
   reference   <- lst$reference
   hst         <- hist(x, plot = FALSE)
   dmax        <- max(hst$density)
   set.seed(lst$seed)
   
   # Set up the plot
   
   dens    <- density(x, bw = bw.norm(x))
   xlimits <- range(estimate)
   if (!lst$zoom) xlimits <- range(xlimits, dens$x)
   if (position == 'sample mean') xlimits <- range(estimate - 4 * se, estimate + 4 * se)
   if (position == 'reference')   xlimits <- range(xlimits, mu - 4 * se, mu + 4 * se)
   # Ensure there is space to place arrows beyond
   if (pvalue) {
      estlocs <- estimate
      if (lst$ttest$alternative == 'two.sided') {
         xlimits <- range(xlimits, mu - (estimate - mu))
         estlocs <- c(estlocs, mu - (estimate - mu))
      }
      if (any(abs(estlocs - xlimits[1]) < 0.1 * diff(xlimits)))
         xlimits[1] <- xlimits[1] - 0.1 * diff(xlimits)
      if (any(abs(estlocs - xlimits[2]) < 0.1 * diff(xlimits)))
         xlimits[2] <- xlimits[2] + 0.1 * diff(xlimits)
   }
   lst$xlimits <- xlimits
   
   # Find the tick points and labels for the se scale
   sedist <- if (lst$position == 'reference') (estimate - mu) / se
             else if (lst$reference) (mu - estimate) / se
             else NULL
   if (!is.null(sedist))
      sedist <- 2 * (ceiling(abs(sedist) / 2)) * sign(sedist)
   serange <- range(-4, 4, sedist)
   if (!is.null(sedist) & lst$display['pvalue'] & lst$ttest$alternative == 'two.sided')
      serange <- range(serange, -sedist)
   if (max(abs(serange)) <= 8) {
      tpos    <- (serange[1]:serange[2]) * se
      tlab    <- seq(serange[1], serange[2], by = 2)
   }
   else{
      tlab <- pretty(serange)
      tpos <- tlab * se
   }
   lst$tpos <- tpos
   lst$tlab <- tlab
   
   dfrm <- data.frame(x)
   plt <- ggplot2::ggplot(dfrm, ggplot2::aes(x)) +
          ggplot2::theme(
            axis.ticks.y       = ggplot2::element_blank(),
            axis.title.y       = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank()) +
          ggplot2::xlab(lst$vlab)

   # Plot the data
   
   if (!lst$zoom) {
      if (lst$data.display == 'histogram') {
         plt <- plt +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                    breaks = hst$breaks, col = col.dens, fill = col.dens)
      }
      else {
         if (length(x) >= lst$nmin) {
            d.dens <- data.frame(xgrid = dens$x, dgrid = dens$y)
            dmax   <- max(dens$y)
            dens.y <- approx(dens$x, dens$y, xout = x)$y
            plt <- plt +
               ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                                 ymin = 0, ymax = dgrid),
                                    data = d.dens, col = NA, fill = col.dens, alpha = 0.25)
         }
         else {
            dens.y <- dnorm(x, mean(x), sd(x))
            dmax   <- dnorm(0, mean(x), sd(x))
         }
         d.densd <- data.frame(x = x, d = dens.y, r = runif(length(dens.y), 0, 1))
         sz  <- if (length(x) >= lst$nmin) 0.2 else 1
         plt <- plt +
            ggplot2::geom_point(ggplot2::aes(x, r * d), data = d.densd, size = sz)
      }
   }
   else
      dmax <- 1
   lst$dmax <- dmax

   # Locate the centre of the uncertainty scale
   cntr <- switch(position, 'none' = NA, 'candidate' = lst$candidate,
                  'sample mean' = estimate, 'reference' = mu)
   
   # Plot the uncertainty
   if (position != 'none')
      plt <- rp.add_uncertainty(plt, cntr, -dmax, 0.8 * dmax, -1.25 * dmax, lst)
                                    # xlimits, ttest$estimate, se, ttest$parameter, dmax,
                                    # ucol, ulcol, plot.args$col, ci, ttest$conf.int,
                                    # pvalue, conf = attr(ttest$conf.int, 'conf.level'),
                                    # p.value = lst$ttest$p.value,
                                    # distribution = 't', alternative = ttest$alternative,
                                    # ngrid = 100, plot.args$zoom)
   
   # Plot the sample mean
   
   linestart <- -dmax
   lineend   <- if (lst$zoom) 0.1 * dmax else 1.2 * dmax
   plt <- plt +
      ggplot2::annotate('segment', x = estimate, xend = estimate,
                        y = linestart, yend = lineend, col = lst$col['estline']) +
      ggplot2::annotate('text', x = estimate, y = lineend + 0.05 * dmax,
                        hjust = hjst(estimate, xlimits, 0.25),
                        label = 'sample mean', col = lst$col['estline'])
   
   # Plot reference if requested
   
   if (lst$reference | (lst$position == 'reference')) {
      rlineend <- if (lst$zoom) 0.05 * dmax else 1.05 * dmax
      plt  <- plt +
         ggplot2::annotate('segment', x = mu, xend = mu,
                           y = linestart, yend =  rlineend, col = lst$col['refline']) +
         ggplot2::annotate('text', x = mu, y = rlineend + 0.05 * dmax, label = 'reference',
                           hjust = hjst(mu, xlimits, 0.25), col = lst$col['refline'])
   }
   
   # Plot the uncertainty axis
   
   if (se.scale) {
      plt  <- rp.add_sescale(plt, cntr, -1.1 * dmax, -dmax, lst)
   }
   
   # Add the y-axis labels
   
   ticks <- c(0.5, -0.5, -1, -1.25) * dmax
   ylabs <- c('data\n', 't-distribution', 'se scale',
              paste(round(attr(lst$ttest$conf.int, 'conf.level') * 100),
                    '%\nconfidence\ninterval', sep = ''))
   if (position == 'reference')
      ylabs[4] <- paste('p-value\n', signif(lst$ttest$p.value, 2), sep = '')
   tind  <- 1
   if (lst$display['distribution']) tind <- c(tind, 2)
   if (se.scale) tind <- c(tind, 3)
   if (ci | pvalue) tind <- c(tind, 4)
   plt <- plt +
      ggplot2::scale_y_continuous(breaks = ticks[tind], labels = ylabs[tind],
                                  limits = c(-1.3 * dmax, 1.3 * dmax))

   if (lst$static) return(plt) else print(plt)
   lst
}

rp.onesample.redraw <- function(panel) {
   rp.tkrreplot(panel, plot)
   panel
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

rp.add_sescale <- function(plt, xpos, ylo, yhi, lst) {
   tpos   <- xpos + lst$tpos
   tlab   <- lst$tlab
   se     <- lst$ttest$stderr
   sclr   <- ifelse(lst$position == 'reference', lst$col['refline'],
                    lst$col['estline'])
   tscl   <- 0.10 * (yhi - ylo)
   drtpos <- diff(range(tpos))
   plt  <- plt +
      ggplot2::annotate('rect', xmin = min(tpos) - drtpos / 10,
                        xmax = max(tpos) + drtpos / 10,
                        ymin = ylo, ymax = yhi,
                        fill = 'white') +
      ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                        y = ylo, yend = ylo, col = sclr) +
      ggplot2::annotate('segment', x = tpos, xend = tpos,
                        y = ylo, yend = ylo + tscl, col = sclr) +
      ggplot2::annotate('text', x = xpos + tlab * se,
                        y = (ylo + yhi) / 2, col = sclr,
                        label = as.character(tlab)) +
      ggplot2::annotate('segment', x = tpos, xend = tpos,
                        y = yhi, yend = yhi - tscl, col = sclr) +
      ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                        y = yhi, col = sclr)
   plt
}

rp.add_uncertainty <- function(plt, xpos, ypos, yht, cipos, lst) {
                               # xlimits,
                               # estimate, se, degf, dmax,
                               # ucol, ulcol, clr, ci, conf.int,
                               # pvalue, conf = 0.95, p.value,
                               # distribution = 't', alternative = 'two.sided',
                               # ngrid = 100, zoom) {
   estimate <- lst$ttest$estimate
   se       <- lst$ttest$stderr
   dmax     <- lst$dmax
   xlimits  <- lst$xlimits
   ucol     <- switch(lst$position, 'none' = NA, 'candidate' = 'grey50',
                      'reference'   = lst$col['reference'],
                      'sample mean' = lst$col['estimate'])
   ulcol    <- switch(lst$position, 'none' = NA, 'candidate' = 'grey50',
                      'reference'   = lst$col['refline'],
                      'sample mean' = lst$col['estline'])
   if (lst$display['distribution']) {
      ngrid <- 100
      xgrid <- seq(xpos - 4 * se, xpos + 4 * se, length = ngrid)
      d.fn  <- ifelse (lst$distribution == 't',
                       function(x) dt((x - xpos) /se, df = lst$ttest$parameter),
                       function(x) dnorm(x, xpos, se))
      dgrid <- data.frame(xgrid, ymin = ypos, dens = d.fn(xgrid))
      scl   <- yht / dmax
      plt <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, ymin = ymin, ymax = ypos + dens * scl),
                              data = dgrid, col = NA, fill = ucol)
   }
   # Add notches to the uncertainty distribution
   if (lst$display['ci'] | lst$display['pvalue']) {
      q.fn       <- ifelse (lst$distribution == 't',
                            function(x) xpos + se * qt(x, df = lst$ttest$parameter),
                            function(x) qnorm(x, xpos, se))
      notch.p    <- c(0.5 - lst$conf / 2, 0.5 + lst$conf / 2)
      notch.x    <- q.fn(notch.p)
      notch.dfrm <- data.frame(x = notch.x, y = ypos + d.fn(notch.x) * scl)
      plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = x, y = y, yend = ypos),
                                               col = 'white', data = notch.dfrm)
   }
   # Add the confidence interval
   if (lst$display['ci'] & !lst$position == 'reference')
      plt <- plt + ggplot2::annotate('segment',
                                     x = lst$ttest$conf.int[1], xend = lst$ttest$conf.int[2],
                                     y = cipos, col = ulcol)
   # Add the p-value calculation
   if (lst$display['pvalue']) {
      xstart  <- estimate
      xlimits <- xpos + range(lst$tpos)
      xstop   <- switch(lst$ttest$alternative, greater = xlimits[2], less = xlimits[1],
                            two.sided = ifelse(estimate > xpos, xlimits[2], xlimits[1]))
      p       <- ifelse(lst$ttest$alternative == 'two.sided',
                        lst$ttest$p.value / 2, lst$ttest$p.value)
      p       <- as.character(signif(p, 2))
      hjust   <- ifelse (xstop > xstart, 'left', 'right')
      plt <- plt +
         ggplot2::annotate('segment', y = cipos,
                           x = xstart, xend = xstop, col = ulcol,
                           arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))) +
         ggplot2::annotate('text', x = xstart, y = cipos + 0.08 * dmax,
                           label = p, hjust = hjust, col = ulcol) +
         ggplot2::annotate('text', x = xpos, y = cipos - 0.05 * dmax,
                           label = 'probability', col = ulcol)
      if (lst$ttest$alternative == 'two.sided') {
         xstart    <- xpos - (estimate - xpos)
         xstop     <- ifelse(xstop > mean(xlimits), xlimits[1], xlimits[2])
         hjust     <- ifelse(hjust == 'left', 'right', 'left')
         linestart <- -dmax
         lineend   <- if (lst$zoom) 0.1 * dmax else 1.2 * dmax
         plt <- plt +
            ggplot2::annotate('segment', y = cipos,
                              x = xstart, xend = xstop, col = ulcol,
                              arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))) +
            ggplot2::annotate('text', x = xstart, y = cipos + 0.08 * dmax,
                              label = p, hjust = hjust, col = ulcol) +
            ggplot2::annotate('segment', x = xstart, y = linestart, yend = lineend,
                              col = lst$col['estline'], linetype = 'dashed')
      }
   }
   plt
}

rp.twosample <- function(lst) {

   # Ensure that x has values and y is a factor
   
   xlab       <- plot.args$xlab
   ylab       <- plot.args$ylab
   vlab       <- plot.args$vlab
   horizontal <- plot.args$horizontal
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

   display     <- lst$display
   violin      <- (display == 'violin')
   zoom        <- lst$zoom
   mns         <- lst$ttest$estimate
   se          <- lst$ttest$stderr
   mu          <- lst$ttest$null.value
   position    <- lst$position
   reference   <- lst$reference
   se.scale    <- ifelse (position != 'none', TRUE, plot.args$se.scale)
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
   # ylimits <- c(0, 4 * ht['margin'] + 2 * ht['main'] + ht['axis'])
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
      ggplot2::scale_y_continuous(breaks = apos, labels = alab) +
      ggplot2::xlab(vlab)
   if (!horizontal) plt <- plt + ggplot2::coord_flip() +
                                ggplot2::theme(axis.title.x = ggplot2::element_blank())
   else             plt <- plt + ggplot2::theme(axis.title.y = ggplot2::element_blank())

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
      if (position == 'reference') {
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
   
   if (position != 'none') {
      ucol <- if (position == 'sample mean') clr['estimate']
      else clr['reference']
      # plt  <- rp.add_uncertainty(plt, display, cntr, 2 * ht['margin'] + ht['main'], ht['axis'],
      #                            se, lst$ttest$parameter, col = ucol)
      
      xpos  <- if (position == 'sample mean') mns[2] else mns[1] + mu
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
   if (plot.args$reference | (position == 'reference')) {
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
   if (lst$static) return(plt) else print(plt)
   lst
}

hjst <- function(x, xlimits, edge) {
   hjst <- (x - xlimits[1]) / diff(xlimits)
   if (hjst < edge)     hjst <- 2 * hjst
   if (hjst > 1 - edge) hjst <- 1 - 2 * (1 - hjst)
   hjst
}

#     A function for a confidence interval and hypothesis test
#     on two samples of data

rp.ttest <- function(x, y = NULL, mu = NULL,
                     uncertainty = 'sample mean',
                     scale = FALSE, col = '#86B875', refcol = '#E495A5',
                     height, refheight = 1.5 * height, seed,
                     ...) {
   
   if (!requireNamespace("ggplot2"))
      stop("the ggplot2 package is not available.")
   if (!requireNamespace("ggforce")) {
      cat("The ggforce package is not available - reverting to 'jitter' for points.")
   }
   
   if (!missing(seed)) set.seed(seed)
   if (!(uncertainty %in% c('none', 'sample mean', 'reference')))
         uncertainty <- 'sample mean'
   ttest.args <- list(...)
   reference  <- !is.null(mu)
   if (!reference) mu <- 0
   
   ttest <- t.test(x, y, mu = mu, ...)
   method <- if      (grepl("One",    ttest$method)) "One"
             else if (grepl("Two",    ttest$method)) "Two"
             else if (grepl("Paired", ttest$method)) "Paired"
   if (missing(height)) {
    height <- if (method == 'Two')
                 c('data' = 0.38, 'uncertainty' = 0.2, 'mean' = 0.28)
               else 0.2
   }
   if (missing(col)) {
      col <- if (method == 'Two')
         # col = c("#1FC8DEFF", "#F1CA3AFF", "#C1F334FF", "#BE2102FF"),
         col = c(grey(0.5), grey(0.5), '#86B875', '#E495A5')
         else col = '#86B875'
   }
   if (missing(refcol)) refcol <- '#E495A5'
   
   plot.args <- list(scale = scale, col = col, refcol = refcol,
                     height = height, refheight = refheight,
                     uncertainty = uncertainty, reference = reference)

   if (method == 'Paired') x <- y - x
   plt <- if (method %in% c('One', 'Paired'))
               rp.onesample(x, ttest, ttest.args, plot.args)
          else rp.twosample(x, y, ttest, ttest.args, plot.args)
   
   invisible(plt)
}


rp.onesample <- function(x, ttest, ttest.args, plot.args) {
   
   reference   <- ttest$null.value
   uncertainty <- plot.args$uncertainty
   
   # Set up the plot
   
   dfrm <- data.frame(x, gp = 1)
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, gp)) +
      ggplot2::theme(axis.text.y        = ggplot2::element_blank(),
                     axis.ticks.y       = ggplot2::element_blank(),
                     axis.title.y       = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank())
   
   # Plot the data
   
   plt <- plt + ggforce::geom_sina(jitter_y = FALSE, maxwidth = 1)

   # If a reference is provided, plot this or the sample mean
   
   if (plot.args$reference | uncertainty == 'reference') {
      xpos <- if (uncertainty == 'sample mean') reference
              else ttest$estimate
      xcol <- if (uncertainty == 'sample mean') plot.args$refcol
              else plot.args$col
      clab <- if (uncertainty == 'sample mean') 'reference' else 'sample mean'
      plt  <- plt +
         # ggplot2::annotate("segment", x = reference, xend = reference,
         #                              y = 1 - refheight, yend = 1 + refheight,
         #                              col = refcol) +
         ggplot2::annotate("segment", x = xpos, xend = xpos,
                           y    = 1 - plot.args$refheight,
                           yend = 1 + plot.args$refheight, col = xcol) +
         # ggplot2::geom_segment(ggplot2::aes(x = xpos, xend = xpos,
         #                                    y =    1 - plot.args$refheight,
         #                                    yend = 1 + plot.args$refheight,
         #                                    col = xcol)) +
         ggplot2::scale_colour_manual(values = xcol,
                                      labels = clab,
                                      guide  = ggplot2::guide_legend(title = NULL,
                                                                     position = "right"))
   }
   
   # Plot the uncertainty
   
   if (uncertainty != 'none') {
      cntr   <- if (uncertainty == 'sample mean') ttest$estimate else reference
      ucol   <- if (uncertainty == 'sample mean') plot.args$col
                else plot.args$refcol
      clab   <- if (uncertainty == 'sample mean') 'sample mean uncertainty'
                else 'reference uncertainty'
      se     <- ttest$stderr
      rng    <- range(cntr - 4 * se, cntr + 4 * se)
      ngrid  <- 100
      xgrid  <- seq(rng[1], rng[2], length = ngrid)
      dens   <- dt((xgrid - cntr) / se, length(x) - 1) / dt(0, length(x) - 1)
      dgrd   <- data.frame(x = xgrid, y = 1, dens)
      height <- plot.args$height
      # The geom_line calls tidy up the edges of the filled area
      plt   <- plt +
         ggplot2::geom_line(ggplot2::aes(x, y = 1 + 0.5 * dens * height),
                            col = ucol, alpha = 0.7, data = dgrd) +
         ggplot2::geom_line(ggplot2::aes(x, y = 1 - 0.5 * dens * height),
                            col = ucol, alpha = 0.7, data = dgrd) +
         ggplot2::geom_tile(ggplot2::aes(x, y, height = dens * height, fill = ucol),
                            alpha = 0.7, data = dgrd) +
         ggplot2::scale_fill_manual(values = ucol, labels = clab,
                                    guide  = ggplot2::guide_legend(title = NULL,
                                                                position = "right"))
      # Plot the uncertainty axis
      if (plot.args$scale) {
         ypos <- 1 - height
         tpos <- cntr - (-4:4) * se
         plt  <- plt +
            ggplot2::annotate("segment", x = rng[1], xend = rng[2],
                              y = ypos, yend = ypos, col = ucol) +
            ggplot2::annotate("segment", x = tpos, xend = tpos, col = ucol,
                              y = ypos, yend = ypos - 0.1 * height) +
            ggplot2::annotate("text",    x = cntr - c(-4, -2, 0, 2, 4) * se,
                              y = ypos - 0.3 * plot.args$height,
                              label = as.character(seq(-4, 4, by = 2)),
                              col = ucol)
      }
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
# rp.twosample <- function(x, y, xlab, ylab, data_plot = "sina", uncertainty,
#                                                   reference = 0, violin = TRUE,
#                          scale = FALSE, seed) {
   
   if (!requireNamespace("ggplot2"))
      stop("the ggplot2 package is not available.")
   if (!requireNamespace('ggforce')) {
      cat("The ggforce package is not available - reverting to 'jitter' for points.")
      data_plot <- 'jitter'
   }
   
   reference   <- ttest$null.value
   uncertainty <- plot.args$uncertainty
   height      <- plot.args$height
   col         <- plot.args$col
   violin      <- TRUE
   
   # Ensure that x has values and y is a factor
   
   if (is.character(y)) y <- factor(y)
   if (is.factor(y)) {
      if (nlevels(y) != 2)
         stop('y should have only two levels.')
      if (missing(xlab)) xlab <- deparse(substitute(x))
      if (missing(ylab)) ylab <- deparse(substitute(y))
   }
   if (is.numeric(y)) {
      fac  <- factor(c(rep(deparse(substitute(x)), length(x)),
                       rep(deparse(substitute(y)), length(y))))
      x    <- c(x, y)
      y    <- fac
      xlab <- 'value'
      ylab <- 'groups'
   }
   
   # formula case


   # Plot the data
   
   dfrm <- data.frame(x, y)
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y)) +
           ggforce::geom_sina(jitter_y = FALSE,
                              maxwidth = 2 * plot.args$height['data'])

   # Adjust the axes
   
   plt  <- plt +
      ggplot2::ylab('groups')
   # plt  <- plt +
   #    ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = 0.33)) +
   #    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
   #                   panel.grid.minor.x = ggplot2::element_blank()) +
   #    ggplot2::geom_hline(yintercept = 1.5, col = 'white', linetype = 'dashed',
   #                        linewidth = 0.3) +
   #    ggplot2::scale_x_continuous(
   #           sec.axis = ggplot2::sec_axis(~ . - mean(x[y == levels(y)[1]]),
   #              name = paste("Difference (",
   #              levels(y)[2], ' - ', levels(y)[1], ")", sep = ''))) +
   #    ggplot2::scale_y_discrete(limits = c(levels(y)[1], '', levels(y)[2]))
   # Separating white bar when difference i9s at the top
   # plt  <- plt +
   #    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 2.47, ymax = 2.53),
   #                       fill = "white")

   # If a reference is provided, plot this or the sample mean
   
   # if (reference) {
   #    xpos <- if (uncertainty == 'sample mean') reference   else mean(x)
   #    xcol <- if (uncertainty == 'sample mean') refcol      else col
   #    clab <- if (uncertainty == 'sample mean') 'reference' else 'sample mean'
   #    plt  <- plt +
   #       ggplot2::geom_segment(aes(x = xpos, xend = xpos,
   #                                 y = 1 - height['mean'], yend = 1 + height['mean'],
   #                                 col = xcol, linewidth = 1)) +
   #       ggplot2::scale_colour_manual(values = xcol,
   #                                    labels = clab,
   #                                    guide  = guide_legend(title = NULL,
   #                                                          position = "bottom"))
   # }
   
   # Plot the uncertainty
   
   mns    <- tapply(x, y, mean, na.rm = TRUE)
   # if (uncertainty == 'none') {
      # Plot the sample means
      dfrm <- data.frame(xpos = c(mns, mns[2], mns[1] + reference),
                         ypos = c(1:3, 3), colcode = factor(1:4),
                         labels = c(levels(y), 'difference', 'reference'))
      dfrm <- dfrm[!is.na(dfrm$xpos), ]
      plt  <- plt +
         ggplot2::geom_segment(ggplot2::aes(x = xpos, xend = xpos,
                                   y = ypos - height['mean'],
                                   yend = ypos + height['mean'],
                                   col = colcode), linewidth = 1, data = dfrm) +
         ggplot2::scale_colour_manual(values = col,
                                      labels = labels,
                                      guide  = ggplot2::guide_legend(title = 'Sample means:',
                                                            position = "right"))
   # }
   # else {
      print(plt)
      stop()
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
                         ymin1 = 1 - hunc * mat[[2]] / dt(0, n[3] - 1),
                         ymin2 = 2 - hunc * mat[[4]] / dt(0, n[3] - 1),
                         ymin3 = 3 - hunc * mat[[6]] / dt(0, n[3] - 1),
                         ymax1 = 1 + hunc * mat[[2]] / dt(0, n[3] - 1),
                         ymax2 = 2 + hunc * mat[[4]] / dt(0, n[3] - 1),
                         ymax3 = 3 + hunc * mat[[6]] / dt(0, n[3] - 1))
      if (!violin) {
         dgrd$ymin1 <- 1
         dgrd$ymin2 <- 2
         dgrd$ymin3 <- 3
      }
      plt   <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid1,  y = 1,
                                           ymin = ymin1, ymax = ymax1,
                                           fill = colcode[1]),
                              alpha = 0.7, data = dgrd) +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid2, y = 2,
                                           ymin = ymin2, ymax = ymax2,
                                           fill = colcode[2]),
                              alpha = 0.7, data = dgrd) +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid3, y = 3,
                                           ymin = ymin3, ymax = ymax3,
                                           fill = colcode[3]),
                              alpha = 0.7, data = dgrd) +
         ggplot2::scale_fill_manual(values = c(col[1:2], fcol),
                                    labels = labels,
                                    guide  = ggplot2::guide_legend(title = 'Uncertainty of means:',
                                                          position = "right"))
      if (!is.null(reference) & !is.na(reference)) {
         label <- if (uncertainty == 'sample mean') 'reference' else 'difference'
         clr   <- if (uncertainty == 'sample mean') col[4] else col[3]
         cntr  <- if (uncertainty == 'sample mean') reference else diff(mns)
         cntr  <- cntr + mns[1]
         dfrm  <- data.frame(x = cntr, xend = cntr,
                             y = 3 - height['mean'], yend = 3 + height['mean'])
         plt   <- plt +
            ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend, 
                                      col = clr), linewidth = 1, data = dfrm) +
            ggplot2::scale_colour_manual(values = clr,
                                         labels = label,
                                         guide  = ggplot2::guide_legend(title = 'Sample means:',
                                                               position = "right"))
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
   # }
      
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

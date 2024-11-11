#     A function for a confidence interval and hypothesis test
#     on one sample of data

rp.twosample <- function(x, y, xlab, ylab, data_plot = "sina", uncertainty,
                         reference = 0, violin = TRUE,
                         # cols = c("#1FC8DEFF", "#F1CA3AFF", "#C1F334FF", "#BE2102FF"),
                         cols = c(grey(0.5), grey(0.5), "#18DFBEFF", "#DD3D08FF"),
                         height = c('data' = 0.38, 'uncertainty' = 0.2, 'mean' = 0.38),
                         scale = FALSE, seed) {
   
   if (!requireNamespace("ggplot2"))
      stop("the ggplot2 package is not available.")
   if (data_plot == 'sina' & !requireNamespace('ggforce')) {
      cat("The ggforce package is not available - reverting to 'jitter' for points.")
      data_plot <- 'jitter'
   }
   
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

   # Check there is sufficient data
   
   ind <- is.na(x) | is.na(y)
   x <- x[!ind]
   y <- y[!ind]
   if (length(x) <= 1)
      stop(paste("x has", length(x), "non-missing observations."))
   if (length(y) <= 1)
      stop(paste("y has", length(y), "non-missing observations."))
   
   # Set default arguments
   
   if (!missing(seed)) set.seed(seed)
   if (missing(uncertainty)) uncertainty <- 'none'
   else {
      if (!(uncertainty %in% c('none', 'sample mean', 'reference'))) {
         cat("The value of 'uncertainty' is invalid and has been set to 'none'.\n")
         uncertainty <- 'none'
      }
   }
   if (is.null(reference)) reference <- NA
   if (is.na(reference) & (uncertainty == 'reference')) {
      warning("'reference' is missing and has been set to 0.")
      reference <- 0
   }

   # Set up the plot
   
   dfrm <- data.frame(x, y)
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y)) +
           ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                          panel.grid.minor.x = ggplot2::element_blank()) +
           ggplot2::geom_hline(yintercept = 1.5, col = 'white', linetype = 'dashed',
                               linewidth = 0.3) +
           ggplot2::scale_x_continuous(
                      sec.axis = sec_axis(~ . - mean(x[y == levels(y)[1]]),
                           name = paste("sample mean difference (",
                                        levels(y)[2], ' - ', levels(y)[1],
                                        ")", sep = ''))) +
           ggplot2::scale_y_discrete(limits = as.character(1:3),
                                     labels = c(levels(y), '')) +
           ggplot2::geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 2.47, ymax = 2.53),
                              fill = "white")
   
   # Plot the data
   
   if (data_plot == "sina")
      plt <- plt + ggforce::geom_sina(jitter_y = FALSE, maxwidth = 2 * height['data'])
   else
      plt <- plt + ggplot2::geom_jitter(width = 0, height = 2 * height['data'])
   plt  <- plt +
      ylab('groups') +
      theme(axis.title.y = element_text(hjust = 0.33))
   
   # If a reference is provided, plot this or the sample mean
   
   # if (!reference.missing) {
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
   
   mns    <- tapply(x, y, mean)
   if (uncertainty == 'none') {
      # Plot the sample means
      dfrm <- data.frame(xpos = c(mns, mns[2], mns[1] + reference),
                         ypos = c(1:3, 3), colcode = factor(1:4),
                         labels = c(levels(y), 'difference', 'reference'))
      dfrm <- dfrm[!is.na(dfrm$xpos), ]
      plt  <- plt +
         ggplot2::geom_segment(aes(x = xpos, xend = xpos,
                                   y = ypos - height['mean'],
                                   yend = ypos + height['mean'],
                                   col = colcode), linewidth = 1, data = dfrm) +
         ggplot2::scale_colour_manual(values = cols,
                                      labels = labels,
                                      guide  = guide_legend(title = 'Sample means:',
                                                            position = "bottom"))
   }
   else {
      cntr  <- if (uncertainty == 'sample mean') diff(mns) else reference
      cntr  <- c(mns, cntr + mns[1])
      n     <- c(tapply(x, y, length), length(x))
      sds   <- tapply(x, y, function(z) sd(z))
      se    <- tapply(x, y, function(z) sd(z) / sqrt(length(z)))
      sp    <- sqrt((sds[1]^2 * (n[1] - 1) + sds[2]^2 * (n[2] - 1)) / (n[1] + n[2] - 2))
      se    <- c(se, sp * sqrt((1 / n[1] + 1 / n[2])))
      fcol  <- if (uncertainty == 'sample mean') cols[3] else cols[4]
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
         ggplot2::scale_fill_manual(values = c(cols[1:2], fcol),
                                    labels = labels,
                                    guide  = guide_legend(title = 'Uncertainty of means:',
                                                          position = "bottom"))
      if (!is.null(reference) & !is.na(reference)) {
         label <- if (uncertainty == 'sample mean') 'reference' else 'difference'
         clr   <- if (uncertainty == 'sample mean') cols[4] else cols[3]
         cntr  <- if (uncertainty == 'sample mean') reference else diff(mns)
         cntr  <- cntr + mns[1]
         dfrm  <- data.frame(x = cntr, xend = cntr,
                             y = 3 - height['mean'], yend = 3 + height['mean'])
         plt   <- plt +
            ggplot2::geom_segment(aes(x = x, xend = xend, y = y, yend = yend, 
                                      col = clr), linewidth = 1, data = dfrm) +
            ggplot2::scale_colour_manual(values = clr,
                                         labels = label,
                                         guide  = guide_legend(title = 'Sample means:',
                                                               position = "bottom"))
      }
      
      # Plot the uncertainty axis
      if (scale & (uncertainty != 'none')) {
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
         plt  <- scale.fn(plt, mns[1], se[1], cols[1], 1 - 1.2 * htu, htu)
         plt  <- scale.fn(plt, mns[2], se[2], cols[2], 2 - 1.2 * htu, htu)
         cntr <- if (uncertainty == 'sample mean') diff(mns) else reference
         plt  <- scale.fn(plt, cntr + mns[1], se[3], fcol, 3 - 1.2 * htu, htu)
         plt
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

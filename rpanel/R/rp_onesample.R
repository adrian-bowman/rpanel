#     A function for a confidence interval and hypothesis test
#     on one sample of data

rp.onesample <- function(x, xlab, data_plot = "sina", reference, uncertainty,
                         scale = FALSE, col = '#86B875', refcol = '#E495A5',
                         height = 0.2, refheight = 1.5 * height, seed) {
   
   # Checks which will generate failure
   
   x <- x[!is.na(x)]
   if (length(x) <= 1)
      stop(paste("x has", length(x), "non-missing observations."))
   if (!requireNamespace("ggplot2"))
      stop("the ggplot2 package is not available.")
   if (data_plot == "sina" & !requireNamespace("ggforce")) {
      cat("The ggforce package is not available - reverting to 'jitter' for points.")
   }
   
   # Set default arguments
   
   if (missing(xlab)) xlab <- deparse(substitute(x))
   if (!missing(seed)) set.seed(seed)
   reference.missing   <- missing(reference)
   uncertainty.default <- if (reference.missing) 'sample mean' else 'reference'
   if (missing(uncertainty))
      uncertainty <- uncertainty.default
   else {
      if (!(uncertainty %in% c('sample mean', 'reference'))) {
         cat("The value of 'uncertainty' is invalid and has been set to the default.\n")
         uncertainty <- uncertainty.default
      }
   }

   # Set up the plot
   
   dfrm <- data.frame(x, gp = 1)
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, gp)) +
           ggplot2::theme(axis.text.y        = ggplot2::element_blank(),
                          axis.ticks.y       = ggplot2::element_blank(),
                          axis.title.y       = ggplot2::element_blank(),
                          panel.grid.major.y = ggplot2::element_blank(),
                          panel.grid.minor.y = ggplot2::element_blank())
   
   # Plot the data
   
   if (data_plot == "sina")
      plt <- plt + ggforce::geom_sina(jitter_y = FALSE, maxwidth = 1)
   else
      plt <- plt + ggplot2::geom_jitter(width = 0, height = 0.25)

   # If a reference is provided, plot this or the sample mean
   
   if (!reference.missing) {
      xpos <- if (uncertainty == 'sample mean') reference   else mean(x)
      xcol <- if (uncertainty == 'sample mean') refcol      else col
      clab <- if (uncertainty == 'sample mean') 'reference' else 'sample mean'
      plt  <- plt +
         # ggplot2::annotate("segment", x = reference, xend = reference,
         #                              y = 1 - refheight, yend = 1 + refheight,
         #                              col = refcol) +
         ggplot2::geom_segment(aes(x = xpos, xend = xpos,
                                   y = 1 - refheight, yend = 1 + refheight,
                                   col = xcol)) +
         ggplot2::scale_colour_manual(values = xcol,
                                      labels = clab,
                                      guide  = guide_legend(title = NULL,
                                                            position = "bottom"))
   }
   
   # Plot the uncertainty
   
   cntr  <- if (uncertainty == 'sample mean') mean(x) else reference
   ucol  <- if (uncertainty == 'sample mean') col else refcol
   clab  <- if (uncertainty == 'sample mean') 'sample mean uncertainty' else
                                              'reference uncertainty'
   se    <- sd(x) / sqrt(length(x))
   rng   <- range(cntr - 4 * se, cntr + 4 * se)
   ngrid <- 100
   xgrid <- seq(rng[1], rng[2], length = ngrid)
   dens  <- dt((xgrid - cntr) / se, length(x) - 1) / dt(0, length(x) - 1)
   dgrd  <- data.frame(x = xgrid, y = 1, dens)
   # The geom_line calls tidy up the edges of the filled area
   plt   <- plt +
      ggplot2::geom_line(ggplot2::aes(x, y = 1 + 0.5 * dens * height),
                         col = ucol, alpha = 0.7, data = dgrd) +
      ggplot2::geom_line(ggplot2::aes(x, y = 1 - 0.5 * dens * height),
                         col = ucol, alpha = 0.7, data = dgrd) +
      ggplot2::geom_tile(ggplot2::aes(x, y, height = dens * height, fill = ucol),
                         alpha = 0.7, data = dgrd) +
      ggplot2::scale_fill_manual(values = ucol, labels = clab,
                                 guide  = guide_legend(title = NULL,
                                                       position = "bottom"))

   # Plot the uncertainty axis
   if (scale) {
      ypos <- 1 - height
      tpos <- cntr - (-4:4) * se
      plt  <- plt +
         ggplot2::annotate("segment", x = rng[1], xend = rng[2],
                                      y = ypos, yend = ypos, col = ucol) +
         ggplot2::annotate("segment", x = tpos, xend = tpos, col = ucol,
                                      y = ypos, yend = ypos - 0.1 * height) +
         ggplot2::annotate("text",    x = cntr - c(-4, -2, 0, 2, 4) * se,
                                      y = ypos - 0.3 * height,
                                      label = as.character(seq(-4, 4, by = 2)),
                                      col = ucol)
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

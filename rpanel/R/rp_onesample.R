#     A function for a confidence interval and hypothesis test
#     on one sample of data

rp.onesample <- function(x, xlab, data_plot = "jitter", reference) {
   
   # Checks which will generate failure
   x <- x[!is.na(x)]
   if (length(x) <= 1)
      stop(paste("x has", length(x), "non-missing observations."))
   if (!requireNamespace("ggplot2"))
      stop("the ggplot2 package is not available.")
   if (data_plot == "sina" & !requireNamespace("ggforce"))
      stop("the ggforce package is not available.")
   
   if (missing(xlab)) xlab <- deparse(substitute(x))
   dfrm <- data.frame(x, gp = 1)
   
   # Plot the data
   plt <- ggplot2::ggplot(dfrm, ggplot2::aes(x, gp))
   if (data_plot == "sina")
      plt <- plt + ggforce::geom_sina(jitter_y = FALSE)
   else
      plt <- plt + ggplot2::geom_jitter(width = 0, height = 0.25)
   plt <- plt + ggplot2::theme(axis.text.y        = ggplot2::element_blank(),
                               axis.ticks.y       = ggplot2::element_blank(),
                               axis.title.y       = ggplot2::element_blank(),
                               panel.grid.major.y = ggplot2::element_blank(),
                               panel.grid.minor.y = ggplot2::element_blank())

   # Plot the reference, if provided
   if (!missing(reference))
      plt <- plt + ggplot2::annotate("segment",x = reference, xend = reference,
                                     y = 0.8, yend = 1.2, col = "red") 
   
   # Plot the uncertainty
   mn    <- mean(x)
   se    <- sd(x) / sqrt(length(x))
   rng   <- range(mn - 3 * se, mn + 3 * se)
   ngrid <- 200
   xgrid <- seq(rng[1], rng[2], length = ngrid)
   dens  <- dnorm(xgrid, mn, se) / dnorm(mn, mn, se)
   dgrd  <- data.frame(x = xgrid, y = 1, dens)
   plt   <- plt +
      ggplot2::geom_tile(ggplot2::aes(x, y, height = 0.25 * dens),
                           fill = "lightgreen", data = dgrd) +
      ggplot2::scale_alpha(range = c(0, 1), guide = "none")
   
   # Plot the sample mean and +/- 2 se's
   notchx    <- c(mn - 2 * se, mn, mn + 2 * se)
   notchy0   <- 0.125 * dnorm(notchx, mn, se) / dnorm(mn, mn, se)
   notchx    <- rep(notchx, 2)
   notchy    <- c(1 + notchy0, 1 - notchy0)
   notchyend <- c(1 + notchy0 + 0.04, 1 - notchy0 - 0.04)
   dnotch    <- data.frame(notchx, notchy, notchyend)
   plt   <- plt +
      ggplot2::geom_segment(ggplot2::aes(x = notchx, xend = notchx,
                                         y = notchy, yend = notchyend),
                            col = "darkgreen", data = dnotch)
      
   print(plt)
   plt
}

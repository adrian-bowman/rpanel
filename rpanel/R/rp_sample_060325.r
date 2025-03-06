sample.draw.ggplot <- function(panel) {
   
   mu       <- panel$pars["mu"]
   sigma    <- panel$pars["sigma"]
   n        <- panel$pars["n"]
   col.pars <- "green3"
   
   with(panel, {
      plt  <- ggplot2::ggplot(data.frame(x = y), aes(x))
      brks <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 20)
      
      if (display.sample["data"]) {
         plt  <- plt +
            geom_histogram(aes(y = ..density..),
                           breaks = brks, col = 'grey90', fill = 'grey50') +
            xlim(mu - 3 * sigma, mu + 3 * sigma) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 1 / sigma)) +
            theme(axis.title.y = element_blank(),
                  axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())
         
         # Mark observations outside the scale
         # ind <- which(hst$density > usr[4])
         # if (length(ind) > 0)
         #    segments(hst$breaks[ind], usr[4], hst$breaks[ind + 1], usr[4], col = "red", lwd = 3)
         # ind <- any(hst$density > 0 & hst$breaks[-1] < usr[1])
         # if (ind) segments(usr[1], 0, usr[1], usr[4], col = "red", lwd = 3)
         # ind <- any(hst$density > 0 & hst$breaks[-length(hst$breaks)] > usr[2])
         # if (ind) segments(usr[2], 0, usr[2], usr[4], col = "red", lwd = 3)
      }
      if (display.sample["population"])
         plt <- plt + geom_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                                    col = 'blue')
      if (display.sample["mean"])
         plt <- plt + geom_segment(x = mu, y = 0.9 / sigma, yend = 0,
                                   col = col.pars)
      if (display.sample["+/- 2 st.dev."])
         plt <- plt + geom_segment(x = mu - 2 * sigma, xend = mu + 2 * sigma,
                                   y = 0.8 / sigma, col = col.pars)
      print(plt)
   })
   
   panel
}
      
samplemean.draw.ggplot <- function(panel) {
   
   mu       <- panel$pars["mu"]
   sigma    <- panel$pars["sigma"]
   n        <- panel$pars["n"]
   col.pars <- "green3"
   
   with(panel, {
      if (any(display.mean)) {
         brks <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 20)
         plt <- ggplot2::ggplot(data.frame(x = mns), aes(x)) +
            xlim(mu - 3 * sigma, mu + 3 * sigma) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, sqrt(n) / sigma)) +
            theme(axis.title.y = element_blank(),
                  axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())
            
         # hst <- hist(mns, plot = FALSE,
         #             breaks = seq(mu - 3 * sigma, mu + 3 * sigma, length = 50))
         # plot(mu + c(-3, 3) * sigma, c(0, sqrt(n) / sigma),
         #      type = "n", axes = FALSE, xlab = "Sample mean", ylab = "")
         # usr <- par("usr")
         # rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
         # grid(col = "white", lty = 1)
         # axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
         #      col.axis = grey(0.6), cex.axis = 0.8)
         
         if (display.mean["sample mean"]) {
            plt <- plt + geom_histogram(aes(y = ..density..),
                            breaks = brks, col = 'grey90', fill = 'grey50')
         }
         if (display.sample["mean"])
            plt <- plt + geom_segment(y = 0.9 * sqrt(n) / sigma, yend = 0,
                                      x = mu, col = col.pars)
         if (display.mean["+/- 2 se"])
            plt <- plt + geom_segment(x    = mu - 2 * sigma / sqrt(n),
                                      xend = mu + 2 * sigma / sqrt(n),
                                      y = 0.8 * sqrt(n) / sigma, col = col.pars)
         if (display.mean["distribution"])
            plt <- plt + geom_function(fun = dnorm,
                                       args = list(mean = mu, sd = sigma / sqrt(n)),
                                       col = 'blue')
      }
      else
         plt <- ggplot()
      print(plt)
   })
   
   panel
}

rp.sample <- function(mu = 0, sigma = 1, n = 25, display = 'histogram',
                      panel.plot = TRUE, hscale = NA, vscale = hscale) {

   # Disable the tcltk version and force ggplot graphics
   display <- "tcltk"
   style   <- 'ggplot'
   if (style == 'ggplot' & requireNamespace('ggplot2', quietly = TRUE))
      sample.draw <- sample.draw.ggplot
   else
      sample.draw <- sample.draw.old
   
   if (display == "shiny") {
      if (requireNamespace("shiny", quietly = TRUE)) {
         shiny::runApp(shiny::shinyApp(sample_ui, sample_server))
         return(invisible())
      }
      else
         cat("the shiny package is not available - reverting to tcltk.")
   }
   
   sample.redraw <- function(panel) {
      rp.tkrreplot(panel, plotdata)
      rp.tkrreplot(panel, plotmean)
      panel
   }
   
   sample.changepars <- function(panel) {
      panel$mns <- NULL
      rp.control.put(panel$panelname, panel)
      rp.do(panel, sample.new)
      panel
   }

   sample.new <- function(panel) {
      panel$y <- rnorm(panel$pars["n"], panel$pars["mu"], panel$pars["sigma"])
      if (!panel$display.mean["accumulate"]) panel$mns <- NULL
      panel$mns <- c(mean(panel$y), panel$mns)
      rp.control.put(panel$panelname, panel)
      if (panel$pplot) {
         rp.tkrreplot(panel, plotdata)
         rp.tkrreplot(panel, plotmean)
      }
      else
         sample.draw(panel)
      panel
   }

   if (is.na(hscale)) hscale <- 1
   if (is.na(vscale)) vscale <- hscale
   
   display.sample        <- c(TRUE, rep(FALSE, 3))
   display.mean          <- rep(FALSE, 4)
   names(display.sample) <- c("data", "population", "mean", "+/- 2 st.dev.")
   names(display.mean)   <- c("sample mean", "accumulate", "+/- 2 se", "distribution")
   pars                  <- c(mu, sigma, n)
   names(pars)           <- c("mu", "sigma", "n")
   y                     <- rnorm(n, mu, sigma)
   panel <- rp.control(pars = pars, y = y, mns = mean(y),
                       display.sample = display.sample, display.mean = display.mean,
                       pplot = panel.plot)
   rp.grid(panel, "controls", row = 0, column = 0)
   rp.grid(panel, "plots",    row = 0, column = 1, background = "white")

   if (panel.plot) {
      rp.tkrplot(panel, plotdata, sample.draw.ggplot,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "plots", row = 0, column = 0, background = "white")
      rp.tkrplot(panel, plotmean, samplemean.draw.ggplot,
                 hscale = hscale, vscale = vscale / 2,
                 grid = "plots", row = 1, column = 0, background = "white")
      action.fn <- sample.redraw
   }
   else {
      action.fn <- sample.draw
      rp.do(panel, action.fn)
   }
   rp.textentry(panel, pars, sample.changepars, width = 10,
               c("mean", "st.dev.", "sample size"), c("mu", "sigma", "n"),
               grid = "controls", row = 0, column = 0, sticky = "ew")
   rp.button(panel, sample.new, "Sample",
             grid = "controls", row = 1, column = 0, sticky = "ew")
   rp.checkbox(panel, display.sample, action.fn, names(display.sample),
               title = "Sample",
               grid = "controls", row = 2, column = 0, sticky = "ew")
   rp.checkbox(panel, display.mean,   action.fn, names(display.mean),
               title = "Sample mean",
               grid = "controls", row = 3, column = 0, sticky = "ew")

   invisible()
}

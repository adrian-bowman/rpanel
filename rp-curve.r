rp.curve <- function(curve, covariance, xgrid, x, y, xlab, ylab,
                       hscale = 1, vscale = hscale, panel = TRUE,
                       Speed = 5, ntime = 10, ninterp = 50, ylim = NULL) {

   if (panel & !require(tkrplot)) stop("the tkrplot package is not available.")
   
   data.present <- !missing(x) & !missing(y)
   if (missing(xlab)) xlab <- deparse(substitute(xgrid))
   if (missing(ylab)) ylab <- deparse(substitute(curve))

   ngrid   <- length(xgrid)
   eig     <- eigen(covariance)
   e.vals  <- pmax(eig$values, 0)
   e.vecs  <- eig$vectors
   se.fit  <- sqrt(diag(covariance))
   
   if (is.null(ylim)) {
      d <- c(curve - 3 * se.fit, curve + 3 * se.fit, na.rm = TRUE)
      if (data.present) d <- c(y, d)
      ylim <- range(d, na.rm = TRUE)
   }
   
   rp.curve.draw <- function(pnl) {

      curv <- pnl$curve
      if (pnl$animation) curv <- curv + pnl$e.icurv
      
      # par(mar = c(3, 3, 1, 0) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
      # plot(range(pnl$xgrid), pnl$ylim, ylim = pnl$ylim, type = "n",
      #          xlab = pnl$xlab, ylab = pnl$ylab)
      # if (pnl$data.present) points(pnl$x, pnl$y)
      # lines(pnl$xgrid, curv)
      
      par(mar = c(3, 3, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
      plot(range(pnl$xgrid), pnl$ylim, ylim = pnl$ylim,
      		 xlab = pnl$xlab, ylab = pnl$ylab, type = "n", axes = FALSE)
      usr <- par("usr")
      rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
      grid(col = "white", lty = 1)
      axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
      		 col.axis = grey(0.6))
      axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
      		 col.axis = grey(0.6))
      if (pnl$data.present) points(pnl$x, pnl$y, pch = 16)
      lines(pnl$xgrid, curv, col = "blue", lwd = 2)
      
      pnl
   }
   
   rp.curve.redraw <- function(pnl) {
      rp.tkrreplot(pnl, plot)
      pnl
   }
   
   animate <- function(pnl) {
      pnl$animation  <- !pnl$animation
      if (pnl$animation) {
         pnl$e.sim      <- pnl$e.vecs %*% diag(sqrt(pnl$e.vals)) %*%
                                     rnorm(length(pnl$curve))
         pnl$e.sim.old  <- rep(0, length(pnl$curve))
         pnl$icurv      <- 1
         rp.control.put(pnl$panelname, pnl)      
         rp.timer(pnl, 1, animation.call, function(pnl) pnl$animation)
         pnl$e.icurv    <- rep(0, length(pnl$curve))
         rp.control.put(pnl$panelname, pnl)      
         rp.tkrreplot(pnl, plot)
      }
      pnl
   }
   
   animation.call <- function(pnl) {
   	  Sys.sleep(0.01 + pnl$Speed / 100)
   	  if (pnl$icurv == ntime + 1) {
         pnl$e.sim.old <- pnl$e.sim
         pnl$e.sim     <- pnl$e.vecs %*% diag(sqrt(pnl$e.vals)) %*%
                                  rnorm(length(pnl$curve))
         pnl$icurv     <- 1
   	  }
      wt          <- pnl$icurv / ntime
      wt1         <- sqrt(wt^2 + (1 - wt)^2)
      pnl$e.icurv <- pnl$e.sim.old * (1 - wt) / wt1 + pnl$e.sim * wt / wt1
      pnl$icurv   <- pnl$icurv + 1
      rp.control.put(pnl$panelname, pnl)
      rp.tkrreplot(pnl, plot)
      # Code to create figures from an animation
        # pdf(paste("../figures/curve-new-", pnl$npdf, ".pdf", sep = ""))
        # par(cex.lab = 2)
        # rp.curve.draw(pnl)
        # dev.off()
        # print(pnl$npdf)
        # pnl$npdf <- pnl$npdf + 1
      pnl
   }
   
   if (panel) {
      pnl <- rp.control(x = x, y = y, curve = curve, se.fit = se.fit,
                          e.vecs = e.vecs, e.vals = e.vals,
                          xgrid = xgrid, ngrid = ngrid, data.present = data.present,
                          ylim = ylim, animation = FALSE, npdf = 1,
                          Speed = Speed, ntime = ntime, ninterp = ninterp,
                          xlab = xlab, ylab = ylab)
      rp.tkrplot(pnl, plot, rp.curve.draw,
                 hscale = hscale, vscale = vscale, pos = "right", background = "white")
      rp.button(pnl, animate, "Animate: on/off")
      rp.doublebutton(pnl, Speed, 0.95, log = TRUE, action = rp.curve.redraw)
   }
   else {
      pnl <- list(x = x, y = y, curve = curve, se.fit = se.fit,
                          e.vecs = e.vecs, e.vals = e.vals,
                          xgrid = xgrid, ngrid = ngrid, data.present = data.present,
                          ylim = ylim, animation = FALSE, npdf = 1,
                          Speed = Speed, ntime = ntime, ninterp = ninterp,
                          xlab = xlab, ylab = ylab)
      rp.curve.draw(pnl)
   }
   
   invisible()
}

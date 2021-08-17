#     Multilevel models cartoon

rp.multilevel <- function(mu = 0, sd.levels = c(1, 1), n.levels = rep(5, length(sd.levels)),
                          names = paste("Level", 1:length(sd.levels))) {

   multilevel.sample <- function(panel) {
   	  panel$re <- list()
      for (i in 1:length(sd.levels))
         panel$re[[i]] <- rnorm(cumprod(n.levels)[i], sd = sd.levels[i])
      if (!panel$first) {
      	 rp.control.put(panel$panelname, panel)
      	 rp.do(panel, multilevel.redraw)
      }
      panel$first <- FALSE
      panel
      }
   	
   multilevel.draw <- function(panel) {
      with(panel, {
      	
         xlim <- mu + c(-3, 3) * sqrt(sum(sd.levels^2))
         plot(xlim, 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
         axis(1)
         
         n1      <- n.levels[1]
         vstep   <- 1 / ((n1 + 1) * length(n.levels))
         y       <- mu
         labels  <- character(0)
         vlabels <- numeric(0)
         for (i in 1:maxlevel) {
            nlprev <- prod(n.levels[1:i]) / n.levels[i]        
            if (reffects.showing) {
               y  <- mu + re[[i]]
               vr <- rep((4 - i) / 3, each = n1) - ((1:n1) - 1) * vstep
               vy <- rep(vr, nlprev)
            }
            else {
               y  <- rep(y, each = n.levels[i]) + re[[i]]
               vr <- (n1:1) / n1 - (i - 1) * vstep
               vy <- rep(vr, each = prod(n.levels[1:i]) / n1)
            }
            if      (i == 1) colour <- "black"
            else if (i == 2) colour <- rep(clr, n1)
            else             colour <- rep(rep(clr, each = n.levels[3]), n1)
            rect(rep(xlim[1], n1), vr - 0.5 * vstep,
                 rep(xlim[2], n1), vr + 0.5 * vstep,  col = bclr[i], border = NA)
            text(y, vy, rep(1:(n.levels[i]), nlprev), col = colour, pch = 1)
            labels  <- c(labels, rep(names[i], n1))
            vlabels <- c(vlabels, vr)
         }
         axis(2, vlabels, labels, tick = FALSE, las = 1)
         
         
      })
      panel
   }
   
   multilevel.redraw <- function(panel) {
      rp.tkrreplot(panel, iplot)
      panel
      }
      
   pnl <- rp.control(mu = mu, n.levels = n.levels, sd.levels = sd.levels,
             clr = cm.colors(n.levels[2]), bclr = grey(c(0.9, 0.8, 0.7)),
             names = names,
             maxlevel = 1, reffects.showing = FALSE, first = TRUE)
   rp.do(pnl, multilevel.sample)
   rp.tkrplot(pnl, iplot, multilevel.draw, pos = "right")
   rp.radiogroup(pnl, maxlevel, as.character(1:3), title = "Levels showing",
            action = multilevel.redraw)
   rp.button(pnl, multilevel.sample, "New dataset")
   rp.checkbox(pnl, reffects.showing, multilevel.redraw, label = "Group by level")
   
}

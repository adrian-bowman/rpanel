#   DO in the River Clyde

if (!require(rpanel)) stop("the rpanel package is required.")

# d <- read.table("clyde.dat")
# names(d) <- c("Station", "Day", "Month", "Year", "Depth", "Season", "DO")
# attach(d)

# Treatment <- (Year > 85)
# mdays     <- cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))
# Days      <- Day + mdays[Month]
# ind       <- (Depth==0 & Year > 78 & Year < 92 & Year != 85 & !is.na(DO))
# DO        <- DO[ind]
# Days      <- Days[ind]
# Treatment <- Treatment[ind]
# Station   <- Station[ind]
# save(DO, Days, Treatment, Station, file = "clyde.rda")

load("clyde.rda")

clyde.select <- function(panel) {
   if (panel$analysis == "Day plot" & !panel$slider.added) {
      rp.slider(panel, day.adj, 0, 364, action = clyde.replot, title = "Day adjustment")
      panel$slider.added <- TRUE
   }
   if (panel$analysis == "Day model" & !panel$controls.added) {
      rp.checkbox(panel, model.showing, clyde.replot, title = "Show model")
      rp.slider(panel, intercept, 0, 15,  action = clyde.replot)
      rp.slider(panel, amplitude, 0, 5,   action = clyde.replot)
      rp.slider(panel, phase,     0, 365, action = clyde.replot)
      rp.checkbox(panel, Display, clyde.replot, c("residuals", "fitted model", "groups"))
      rp.doublebutton(panel, station.ind, 2, "Station", clyde.replot, 2, c(0, 24))
      if (require(sm) & require(rgl))
         rp.button(panel, surfaces.plot, "Plot surfaces")
      panel$controls.added <- TRUE
   }
   rp.tkrreplot(panel, plt)
   panel
}
   
surfaces.plot <- function(panel) {
   with(panel, {
      clr <- rep("red", length(do))
      clr[treatment] <- "green"
      scal <- rp.plot3d(days, do, station, size = 2, col = clr)

      for (treat in c(FALSE, TRUE)) {
      ind <- (treatment == treat)
      if (treat) clr <- "green" else clr <- "red"
      surf   <- sm.regression(cbind(days[ind], station[ind]), do[ind], 
                     h = c(20, 1), display = "none")
      ep     <- surf$eval.points
      surf   <- surf$estimate
      ngrid  <- nrow(ep)
      xg     <- numeric(0)
      yg     <- numeric(0)
      zg     <- numeric(0)
      for (i in 1:ngrid) {
         xg <- c(xg, rep(ep[ ,1], c(1, rep(2, ngrid - 2), 1)))
         zg <- c(zg, rep(ep[i,2], 2 * ngrid - 2))
         yg <- c(yg, rep(surf[ , i], c(1, rep(2, ngrid - 2), 1)))
         }
      for (i in 1:ngrid) {
         zg <- c(zg, rep(ep[ ,2], c(1, rep(2, ngrid - 2), 1)))
         xg <- c(xg, rep(ep[i,1], 2 * ngrid - 2))
         yg <- c(yg, rep(surf[i, ], c(1, rep(2, ngrid - 2), 1)))
         }
      sm.surface3d(ep, surf, scal, col = clr)
      }
      })
      panel
   }
   
clyde.plot <- function(panel) {
   with(panel, {
      if (analysis == "Difference") 
            boxplot(split(do[station == station.ind], 
                       factor(treatment[station == station.ind])),
                           names = c("<85", ">85"), ylab = "DO")
      else {
         Days           <- days[station == station.ind]
         DO             <- do[station == station.ind]
         Treatment      <- treatment[station == station.ind]
         Days.adj       <- Days + day.adj
         ind            <- which(Days.adj > 365)
         Days.adj[ind]  <- Days.adj[ind] - 365
         pch            <- rep(1, length(DO))
         clr            <- rep("black", length(DO))
         if (Display["groups"]) {
            pch[Treatment]  <- 3
            clr[!Treatment] <- "red"
            clr[Treatment]  <- "green"
            }
         plot(DO ~ Days.adj, xlab = "Date", ylab = "DO", ylim = range(do),
                       col = clr, pch = pch, axes = FALSE)
         if (Display["groups"]) title(paste("Station", station.ind))
         mdays         <- cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))
         at.adj        <- mdays + 1 + day.adj
         ind           <- which(at.adj > 365)
         at.adj[ind]   <- at.adj[ind] - 365
         axis(1, at = at.adj, labels = c("Jan.1", "", "Mar.1", "", 
            "May.1", "", "Jul.1", "", "Sep.1", "", "Nov.1", ""), cex.axis = 0.7)
         axis(2)
         box()
         ind1 <- 1:(365-day.adj)
         if (day.adj > 0) ind1 <- c((366-day.adj):365, ind1)
         if (model.showing & !Display["groups"]) {
            fv   <- intercept + amplitude * cos(2*pi*(1:365-phase)/365)
            lines(1:365, fv[ind1], col = "blue", lwd = 2)
            if (!Display["fitted model"]) {
               title(paste("DO = ", intercept, " + ", amplitude, 
                  " cos(2pi(day - ", phase, ")/365)", sep = ""), 
                  col.main = "blue")
               if (Display["residuals"]) {
                  fv <- intercept + amplitude * cos(2*pi*(Days-phase)/365)
                  segments(Days.adj, DO, Days.adj, fv, col = "red")
                  }
               }
            }
         if (Display["fitted model"]) {
            if (Display["groups"]) {
               treat <- factor(Treatment)
               model <- lm(DO ~ treat + sin(2*pi*Days/365) + cos(2*pi*Days/365))
               fv    <- predict.lm(model, 
                               data.frame(treat = "TRUE", Days = 1:365))
               lines(1:365, fv[ind1], col = "green", lwd = 2)
               fv    <- predict.lm(model, 
                               data.frame(treat = "FALSE", Days = 1:365))
               lines(1:365, fv[ind1], col = "red", lwd = 2)
               }
            else {
               model <- lm(DO ~ sin(2*pi*Days/365) + cos(2*pi*Days/365))
               fv    <- predict.lm(model, data.frame(Days = 1:365))
               lines(1:365, fv[ind1], col = "green", lwd = 2)
               title(paste("DO = ", round(coef(model)[1], 2), " + ", 
                  round(sqrt((coef(model)[2])^2 + (coef(model)[3])^2), 2),
                  " cos(2pi(day - ", 
                  round(atan(coef(model)[2] / coef(model)[1]) * 365 / (2 * pi), 2),
                  ")/365)", sep = ""), col.main = "green")
               if (Display["residuals"]) {
                  fv <- fitted(model)
                  segments(Days.adj, DO, Days.adj, fv, col = "red")
               }
            }
         }
      }
   })
   panel
}
   
clyde.replot <- function(panel) {
   rp.tkrreplot(panel, plt)
   panel
}

clyde.panel <- rp.control("Clyde data", do = DO, days = Days,
                    treatment = Treatment, station = Station, 
                    intercept = 0, amplitude = 1, phase = 0,
                    slider.added = FALSE, controls.added = FALSE,
                    model.showing = FALSE, analysis = "Difference", first = TRUE,
                    Display = c("residuals" = FALSE, "fitted model" = FALSE, "groups" = FALSE),
                    day.adj = 0, station.ind = 2)
rp.tkrplot(clyde.panel, plt, clyde.plot, pos = "right", hscale = 1.5, vscale = 1.5)
rp.radiogroup(clyde.panel, analysis, c("Difference", "Day plot", "Day model"), action = clyde.select)
rp.do(clyde.panel, clyde.select)
# rp.block(clyde.panel)

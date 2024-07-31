library(rpanel)
if (reinstall) devtools::install("rpanel")

draw <- function(panel) {
   hist(rnorm(40))
   panel
   }
redraw <- function(panel) {
   rp.tkrreplot(panel, plot1)
   panel
   }
pnl <- rp.control()
rp.tkrplot(pnl, plot1, draw)
rp.radiogroup(pnl, flag, as.character(1:3), action = redraw)

pnl <- rp.control()
rp.tkrplot(pnl, plot1, draw, pos = "right")
rp.radiogroup(pnl, flag, as.character(1:3), action = redraw, pos = "top")

pnl <- rp.control()
rp.tkrplot(pnl, plot1, draw, pos = "right")
rp.radiogroup(pnl, flag, as.character(1:3), action = redraw, pos = "top")
rp.checkbox(pnl, flag1, redraw, pos = "top")

data(CofE)
attach(CofE)
draw <- function(panel) {
 with(panel, {
 	clr <- rep(1, length(x))
     if (!any(is.na(coords))) {
        x <- c(x, coords[1])
        y <- c(y, coords[2])
        clr <- c(clr, 2)
        }
     plot(x, y, col = clr)
     model <- lm(y ~ x)
     abline(model, col = "blue", lwd = 3)
     segments(x, y, x, fitted(model), col = "green", lwd = 2)
     })
  panel
  }
drag <- function(panel, x, y) {
   panel$coords  <-  c(x,y)
   rp.tkrreplot(panel, splot)
   panel
   }
pnl <- rp.control(x = Attend, y = Giving, coords = NA)
rp.tkrplot(pnl, splot, draw, drag,drag, pos="right")
rp.checkbox(pnl, var, labels=c("john","ewan","adrian"), pos="top")
rp.checkbox(pnl, var1, labels=c("john","ewan","adrian"), pos="top")
rp.radiogroup(pnl, plot.type, c("histogram", "boxplot", "density estimate"), action = data.plotfn, title = "Plot type", pos="top")
rp.textentry(pnl, pars, plotf, labels = c("df1", "df2", "observed"), initval = c(10, 5, 3), pos="top")

pnl <- rp.control(x = Attend, y = Giving, coords = NA)
rp.checkbox(pnl, var, labels=c("john","ewan","adrian"))
rp.checkbox(pnl, var1, labels=c("john","ewan","adrian"))
rp.radiogroup(pnl, plot.type, c("histogram", "boxplot", "density estimate"), action = draw, title = "Plot type")
rp.textentry(pnl, pars, draw, labels = c("df1", "df2", "observed"), initval = c(10, 5, 3))
rp.tkrplot(pnl, splot, draw, drag,drag, pos="right")

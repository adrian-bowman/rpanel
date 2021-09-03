pkgname <- "rpanel"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "rpanel-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('rpanel')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Clyde")
### * Clyde

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Clyde
### Title: Water quality in the River Clyde
### Aliases: Clyde
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   with(Clyde, {
##D     rp.plot4d(cbind(Doy, DO), Station, location.plot = FALSE)
##D     rp.plot4d(cbind(Station, DO), Doy, location.plot = FALSE)
##D   })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Clyde", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CofE")
### * CofE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CofE
### Title: Giving in the Church of England
### Aliases: CofE
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   with(CofE, {
##D     rp.regression(cbind(Employ, Attend), Giving)
##D   })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CofE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SO2")
### * SO2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SO2
### Title: Sulphur dioxide measurements over Europe
### Aliases: SO2
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   Month     <- SO2$month + (SO2$year - 1990) * 12
##D   Year      <- SO2$year + (SO2$month - 0.5) / 12
##D   Location  <- cbind(SO2$longitude, SO2$latitude)
##D   back      <- I
##D   if (require(maps)) {
##D   	mapxy <- map('world', plot = FALSE,
##D               xlim = range(SO2$longitude), ylim = range(SO2$latitude))
##D       back  <- function() map(mapxy, add = TRUE)
##D   }
##D   rp.plot4d(Location, Year, SO2$logSO2, col.palette = rev(heat.colors(12)),
##D               background.plot = back)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SO2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aircond")
### * aircond

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aircond
### Title: Intervals between the failure of air-conditioning equipment in
###   aircraft
### Aliases: aircond
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    rp.likelihood("sum(log(dexp(data, theta)))", aircond, 0.005, 0.03)
##D    rp.likelihood("sum(log(dgamma(data, theta[1], theta[2])))",
##D         aircond, c(0.3, 0.005), c(3, 0.06))
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aircond", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gullweight")
### * gullweight

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gullweight
### Title: The weights of herring gulls captured at different times of year
### Aliases: gullweight
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    with(gullweight, {
##D      rp.ancova(hab, weight, month)
##D    })
##D    
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gullweight", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("luthor")
### * luthor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: luthor
### Title: Repeated measurements on leutinizing hormone in cows
### Aliases: luthor
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    LH <- luthor[,2:16]
##D    gp     <- factor(luthor[,1])
##D    times  <- c(1:5,(5+(1:10)/2))
##D    rp.rmplot(log(LH), fac = gp, timept = times)
##D    
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("luthor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("poisons")
### * poisons

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: poisons
### Title: Survival times of animals subjected to different poisons and
###   treatment
### Aliases: poisons
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   with(poisons, {
##D     rp.anova(1/stime, treatment, poison)
##D   })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("poisons", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("river")
### * river

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: river
### Title: Temperature and DO threshold in the River Clyde
### Aliases: river
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   rp.logistic(river$Temperature, river$Low)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("river", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rodent")
### * rodent

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rodent
### Title: The mass and speed of quadrupedal rodents
### Aliases: rodent
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    with(rodent, {
##D      rp.regression(log(Mass), log(Speed))
##D    })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rodent", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.ancova")
### * rp.ancova

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.ancova
### Title: Interactive analysis of covariance
### Aliases: rp.ancova
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    with(gullweight, {
##D      rp.ancova(hab, weight, month)
##D    })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.ancova", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.anova")
### * rp.anova

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.anova
### Title: Interactive analysis of variance
### Aliases: rp.anova
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    with(poisons, {
##D      rp.anova(1/stime, treatment, poison)
##D    })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.anova", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.block")
### * rp.block

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.block
### Title: Blocks use of the R console until a panel is closed
### Aliases: rp.block
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D # This function will be called on pressing the button "Simulate".
##D boxp.sim <- function(panel) {
##D   boxplot(rnorm(50))
##D   panel
##D }
##D # Create an rpanel and add the button "Simulate" to it.
##D panel <- rp.control()
##D rp.button(panel, action = boxp.sim, title = "Simulate")
##D rp.block(panel)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.block", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.bubbleplot")
### * rp.bubbleplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.bubbleplot
### Title: Animated scatterplot
### Aliases: rp.bubbleplot
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    rp.bubbleplot(log(gdp), log(co2.emissions), 1960:2007, size = population, 
##D       col = life.expectancy, interpolate = TRUE)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.bubbleplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.button")
### * rp.button

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.button
### Title: Button control for rpanel
### Aliases: rp.button
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    # This function will be called on pressing the button "Simulate".
##D    boxp.sim <- function(panel) {
##D      boxplot(rnorm(50))
##D      panel
##D      }
##D    # Create an rpanel and add the button "Simulate" to it.
##D    panel <- rp.control()
##D    rp.button(panel, action = boxp.sim, title = "Simulate")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.button", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.cartoons")
### * rp.cartoons

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.cartoons
### Title: Access to a collection of rpanel illustrations
### Aliases: rp.cartoons
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    rp.cartoons()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.cartoons", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.checkbox")
### * rp.checkbox

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.checkbox
### Title: A checkbox control for rpanel
### Aliases: rp.checkbox
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    plot.hist <- function(panel) {
##D 	 with(panel, {
##D 		xlim <- range(c(x, mean(x) + c(-3, 3) * sd(x)))
##D 	   if (panel$cbox[3])
##D 	      clr <- "lightblue" else clr <- NULL
##D 	   hist(x, freq = FALSE, col = clr, xlim = xlim)
##D 	   if (panel$cbox[1]) {
##D 	      xgrid <- seq(xlim[1], xlim[2], length = 50)
##D 	      dgrid <- dnorm(xgrid, mean(x), sd(x))
##D 	      lines(xgrid, dgrid, col = "red", lwd = 3)
##D 	      }
##D 	   if (panel$cbox[2])
##D 	      box()
##D 	   })
##D 	 panel
##D 	 }
##D    x <- rnorm(50)
##D    panel <- rp.control(x = x)
##D    rp.checkbox(panel, cbox, plot.hist, 
##D       labels = c("normal density", "box", "shading"), title = "Options")
##D    rp.do(panel, plot.hist)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.checkbox", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.ci")
### * rp.ci

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.ci
### Title: Simulations of normal-based confidence intervals
### Aliases: rp.ci
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   rp.ci()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.ci", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.clearlines")
### * rp.clearlines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.clearlines
### Title: Remove lines from an rpanel image
### Aliases: rp.clearlines
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    panel <- rp.control()
##D    image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
##D    panel <- rp.image(panel, image.file, imagename="gulls.image")
##D    rp.line(panel, imagename=gulls.image, 10, 10, 100, 100, color = "green")
##D    rp.line(panel, imagename=gulls.image, 100, 100, 100, 10, color = "blue")
##D    rp.clearlines(panel, imagename=gulls.image)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.clearlines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.colour.key")
### * rp.colour.key

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.colour.key
### Title: Creates a colour key.
### Aliases: rp.colour.key
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   key.plot <- function(panel) {
##D   	rp.colour.key(topo.colors(12), 0:12)
##D   	panel
##D   }
##D   panel <- rp.control()
##D   rp.tkrplot(panel, key, key.plot, hscale = 0.15)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.colour.key", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.combo")
### * rp.combo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.combo
### Title: A 'combo' for a panel
### Aliases: rp.combo
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    callback <- function(panel) {
##D       print(panel$option)
##D       panel
##D    }
##D    panel <- rp.control()
##D    rp.combo(panel, option, "Pick an option:", 
##D             c("Option1","Option2","Other options"), action=callback)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.combo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.control")
### * rp.control

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.control
### Title: Create or dispose of an rpanel
### Aliases: rp.control rp.control.dispose
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    hist.or.boxp <- function(panel) {
##D      if (panel$plot.type == "histogram")
##D        hist(panel$x)
##D      else
##D        boxplot(panel$x)
##D      panel
##D    }
##D    panel <- rp.control(x=rnorm(50), panelname="panel")
##D    rp.radiogroup(panel, plot.type, c("histogram", "boxplot"),
##D                  title="Plot type", action = hist.or.boxp)  
##D 
##D    # Try also
##D    # panel <- rp.control()
##D    # rp.control.dispose(panel)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.control", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.control.put")
### * rp.control.put

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.control.put
### Title: Updates the panel environment with the current value of the
###   panel list object.
### Aliases: rp.control.put
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   action1 <- function(panel) {
##D     panel$x <- rnorm(1)
##D     rp.control.put(panel$panelname, panel)
##D     rp.do(panel, action2)
##D     panel
##D   }
##D   action2 <- function(panel) {
##D   	print(panel$x)
##D   	panel
##D   }
##D   panel <- rp.control(x = 0)
##D   rp.button(panel, action1, "new x")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.control.put", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.deleteline")
### * rp.deleteline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.deleteline
### Title: Removes a line from an rpanel image
### Aliases: rp.deleteline
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    panel <- rp.control()
##D    image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
##D    panel <- rp.image(panel, image.file, imagename="gulls.image")
##D    rp.line(panel, imagename=gulls.image, 10, 10, 100, 100, color = "green", id="first")
##D    rp.line(panel, imagename=gulls.image, 100, 100, 100, 10, color = "blue", id="second")
##D    rp.deleteline(panel, imagename=gulls.image, id="first")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.deleteline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.do")
### * rp.do

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.do
### Title: Runs a user-written action function
### Aliases: rp.do
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    data.plotfn <- function(panel) {
##D      if (panel$plot.type == "histogram") 
##D        hist(panel$x)
##D      else 
##D        if (panel$plot.type == "boxplot")
##D          boxplot(panel$x)
##D        else 
##D          plot(density(panel$x))
##D      panel
##D    }
##D    panel <- rp.control(x = rnorm(50))
##D    rp.radiogroup(panel, plot.type, 
##D           c("histogram", "boxplot", "density estimate"), 
##D           action = data.plotfn, title = "Plot type", initval="histogram")     
##D    rp.do(panel, data.plotfn)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.do", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.doublebutton")
### * rp.doublebutton

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.doublebutton
### Title: Double-button widget for rpanel
### Aliases: rp.doublebutton
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    density.draw <- function(panel) {
##D      plot(density(panel$x, bw = panel$h))
##D      panel
##D      }
##D    panel <- rp.control(x = rnorm(50))
##D    rp.doublebutton(panel, var = h, step = 0.05, 
##D      title = "Density estimate", action = density.draw,
##D      range = c(0.1, 5), initval=1)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.doublebutton", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.firth")
### * rp.firth

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.firth
### Title: Geostatistical sampling and analysis simulation tool
### Aliases: rp.firth
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   rp.firth()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.firth", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.geosim")
### * rp.geosim

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.geosim
### Title: Interactive visualisation of spatially correlated random fields
### Aliases: rp.geosim
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    rp.geosim()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.geosim", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.grid")
### * rp.grid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.grid
### Title: Define a subsidiary grid within an rpanel
### Aliases: rp.grid
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D panel <- rp.control()
##D rp.grid(panel, pos=list(row=0, column=0, sticky="news"),
##D         background="red", name="g0")
##D rp.grid(panel, pos=list(row=1, column=1, sticky="news", width=100, height=100),
##D         background="navy", name="g1")
##D rp.grid(panel, pos=list(row=2, column=2, sticky="news", width=150, height=200),
##D         background="green", name="g2")
##D rp.button(panel, function(panel) { panel }, "press A",
##D         pos=list(row=1, column=1, sticky=""), parentname="g1")
##D rp.button(panel, function(panel) { panel }, "press B",
##D         pos=list(row=2, column=2, sticky="news"), parentname="g1")
##D rp.button(panel, function(panel) { panel }, "press C",
##D         pos=list("left",width=50, height=150), parentname="g2")
##D rp.grid(panel, pos=list(row=0, column=0, sticky="", width=10, height=10),
##D         background="yellow", parentname="g0")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.grid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.gulls")
### * rp.gulls

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.gulls
### Title: STEPS module: the Birds and the Bees
### Aliases: rp.gulls
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   rp.gulls()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.gulls", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.image")
### * rp.image

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.image
### Title: Placement of an image within a rpanel
### Aliases: rp.image
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    gulls.click <- function(panel, x, y) {
##D      print(c(x, y))
##D      panel
##D      }
##D    panel <- rp.control()
##D    image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
##D    rp.image(panel, image.file, gulls.image, action = gulls.click)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.image", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.likelihood")
### * rp.likelihood

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.likelihood
### Title: Interactive inspection of one- or two-parameter likelihood
###   surfaces
### Aliases: rp.likelihood
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    rp.likelihood("sum(log(dexp(data, theta)))", aircond, 0.005, 0.03)
##D    rp.likelihood("sum(log(dgamma(data, theta[1], theta[2])))",
##D         aircond, c(0.3, 0.005), c(3, 0.06))
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.likelihood", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.line")
### * rp.line

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.line
### Title: Draws a line on an rpanel image
### Aliases: rp.line
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   click.capture <- function(panel,x,y) {
##D     if (is.null(panel$x)) { 
##D       panel$x <- as.numeric(x)
##D       panel$y <- as.numeric(y) 
##D     } 
##D     else { 
##D       rp.line(panel, imagename=gulls.image, panel$x, panel$y,
##D               as.numeric(x), as.numeric(y), width=3, id = "current")
##D       panel$x <- as.numeric(x)
##D       panel$y <- as.numeric(y)
##D     }
##D     panel
##D   }
##D   gulls.panel <- rp.control()
##D   image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
##D   rp.image(gulls.panel, image.file, imagename="gulls.image", action = click.capture)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.line", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.listbox")
### * rp.listbox

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.listbox
### Title: Listbox for a panel
### Aliases: rp.listbox
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    data.plotfn <- function(panel) {
##D      if (panel$plot.type == "histogram")
##D        hist(panel$x)
##D      else
##D        if (panel$plot.type == "boxplot")
##D          boxplot(panel$x)
##D        else
##D          plot(density(panel$x))
##D      panel
##D    }
##D    panel <- rp.control(x = rnorm(50))
##D    rp.listbox(panel, plot.type,
##D        c("histogram", "boxplot", "density estimate"),
##D        action = data.plotfn, title = "Plot type") 
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.listbox", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.logistic")
### * rp.logistic

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.logistic
### Title: Interactive display of logistic regression with a single
###   covariate
### Aliases: rp.logistic
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   rp.logistic(river$Temperature, river$Low)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.logistic", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.menu")
### * rp.menu

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.menu
### Title: Top level menu for a panel
### Aliases: rp.menu
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    a <- rp.control()
##D    # The action function has to come first so that it already exists for rp.menu, 
##D    # as it creates the callback functions on the fly it requires action to already 
##D    # be defined.
##D    domenu <- function(panel) {
##D       rp.messagebox(panel$menuchoice, title = "You chose")
##D       panel
##D       }
##D    rp.menu(a, menuchoice, labels=list(list("File","Quit"),
##D               list("Edit","Copy","Cut","Paste")), action=domenu)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.menu", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.messagebox")
### * rp.messagebox

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.messagebox
### Title: Displays a message
### Aliases: rp.messagebox
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    rp.messagebox("Click OK to continue.", title = "Test message")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.messagebox", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.mururoa")
### * rp.mururoa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.mururoa
### Title: Sampling in Mururoa Atoll
### Aliases: rp.mururoa
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   rp.mururoa()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.mururoa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.normal")
### * rp.normal

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.normal
### Title: Interactive fitting of a normal distribution
### Aliases: rp.normal
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   y <- rnorm(50, mean = 10, sd = 0.5)
##D   rp.normal(y)
##D   
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.normal", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.notebook")
### * rp.notebook

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.notebook
### Title: Define a notebook within an rpanel
### Aliases: rp.notebook rp.notebook.raise
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D panel <- rp.control(title="Notebook example with two notebooks")
##D rp.notebook(panel, c("File", "Edit"), width=600, height=400,
##D             pos=list(row=0, column=0), background="lightgray",
##D             font="Arial", name="n1")
##D rp.notebook.raise(panel, "n1", "Edit")
##D rp.button(panel, function(panel){
##D 	                rp.messagebox("Button pressed!"); panel },
##D 	                "Test this", parentname="Edit")
##D rp.messagebox("A second tabbed notebook can be added to the same window.")
##D rp.notebook(panel, c("A tab 1", "A tab 2"), width=200, height=200,
##D             pos=list(row=1, column=1), background="Navy", foreground="White")
##D rp.messagebox("A tabbed notebook can be placed inside a tabbed notebook.")
##D rp.notebook(panel, c("Tab within tab", "Another tab"),
##D             width=200, height=100, parentname="File", name="n3")
##D rp.notebook.raise(panel, "n1", "File")
##D rp.notebook.raise(panel, "n3", "Another tab")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.notebook", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.panel")
### * rp.panel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.panel
### Title: Returns a panel
### Aliases: rp.panel
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    # create a panel - will be created in .rpenv as "newpanel"
##D    rp.control(panelname = "newpanel")
##D    # creates the panel, but does not return a handle to it - created as ".rpanel2"
##D    rp.control()
##D    # pick up the first panel
##D    panel2 <- rp.panel("newpanel")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.panel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.plot3d")
### * rp.plot3d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.plot3d
### Title: Interactive display of a plot of three variables
### Aliases: rp.plot3d
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   x <- rnorm(50)
##D   y <- rnorm(50)
##D   z <- rnorm(50)
##D   scaling <- rp.plot3d(x, y, z, xlim = c(-3, 3))
##D   # In addition you may add a line to the plot with these two lines;
##D   #  a <- scaling(c(-3,3), c(0,0), c(0,0))
##D   #  lines3d(a$x, a$y, a$z, col = "green", size = 2) 
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.plot3d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.plot4d")
### * rp.plot4d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.plot4d
### Title: Animated scatterplot
### Aliases: rp.plot4d rp.spacetime
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D # The quakes data
##D 
##D with(quakes, {
##D   rp.plot4d(cbind(long, lat), depth)
##D   rp.plot4d(cbind(long, lat), depth, mag)
##D })
##D 
##D # SO2 over Europe
##D 
##D with(SO2, {
##D   location <- cbind(longitude, latitude)
##D 
##D   if (require(mgcv) & require(maps)) {
##D      location1 <- location[,1]
##D      location2 <- location[,2]
##D      model <- gam(logSO2 ~ s(location1, location2, year))
##D      loc1  <- seq(min(location1), max(location1), length = 30)
##D      loc2  <- seq(min(location2), max(location2), length = 30)
##D      yr    <- seq(min(year), max(year), length = 30)
##D      newdata <- expand.grid(loc1, loc2, yr)
##D      names(newdata) <- c("location1", "location2", "year")
##D      model <- predict(model, newdata)
##D      model <- list(x = cbind(loc1, loc2), z = yr,
##D                    y = array(model, dim = rep(30, 3)))
##D      mapxy <- map('world', plot = FALSE,
##D                   xlim = range(longitude), ylim = range(latitude))
##D      rp.plot4d(location, year, logSO2, model,
##D                  col.palette = rev(heat.colors(20)),
##D                  foreground.plot = function() map(mapxy, add = TRUE))
##D   }
##D   else
##D     rp.plot4d(location, year, logSO2, col.palette = rev(heat.colors(20)))
##D })
##D 
##D # Dissolved Oxygen in the River Clyde
##D 
##D with(Clyde, {
##D 
##D   rp.plot4d(cbind(Doy, DO), Station, location.plot = FALSE)
##D   rp.plot4d(cbind(Station, DO), Doy, location.plot = FALSE)
##D   rp.plot4d(cbind(Station, Doy), Year, DO)
##D 
##D   # Highlight the data before and after a sewage treatment plant update in 1985
##D   ind     <- Year >= 80 & Year <= 89 & !(Year == 85)
##D   year    <- Year[ind] + Doy[ind] / 365
##D   station <- Station[ind]
##D   doy     <- Doy[ind]
##D   do      <- DO[ind]
##D   group   <- factor(c("after 1985", "before 1985")[1 + 
##D                   as.numeric(year < 85)])
##D   rp.plot4d(cbind(doy, do), station, group,
##D        col.palette = c("red", "green"), location.plot = FALSE)
##D })
##D 
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.plot4d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.power")
### * rp.power

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.power
### Title: Interactive power calculations for a two-sample t-test
### Aliases: rp.power
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   rp.power()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.power", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.radiogroup")
### * rp.radiogroup

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.radiogroup
### Title: Radiobuttons for a panel
### Aliases: rp.radiogroup
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    data.plotfn <- function(panel) {
##D      if (panel$plot.type == "histogram") 
##D        hist(panel$x)
##D      else 
##D        if (panel$plot.type == "boxplot")
##D          boxplot(panel$x)
##D        else 
##D          plot(density(panel$x))
##D      panel
##D      }
##D    panel <- rp.control(x = rnorm(50))
##D    rp.radiogroup(panel, plot.type, 
##D        c("histogram", "boxplot", "density estimate"), 
##D        action = data.plotfn, title = "Plot type")     
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.radiogroup", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.regression")
### * rp.regression

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.regression
### Title: Graphical display of regression effects (interactive with one or
###   two covariates)
### Aliases: rp.regression rp.regression2
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   with(CofE, {
##D     rp.regression(Employ, Giving)
##D     rp.regression(cbind(Employ, Attend), Giving)
##D     rp.regression(Giving ~ Employ + Elect + Attend)
##D   })
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.regression", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.rmplot")
### * rp.rmplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.rmplot
### Title: Interactive plotting of repeated measurement data
### Aliases: rp.rmplot
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    LH    <- luthor[,2:16]
##D    gp    <- factor(luthor[,1])
##D    times <- c(1:5,(5+(1:10)/2))
##D    rp.rmplot(log(LH), fac = gp, timept = times)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.rmplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.sample")
### * rp.sample

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.sample
### Title: Interactive demonstration of sampling variation
### Aliases: rp.sample
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    rp.sample()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.sample", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.slider")
### * rp.slider

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.slider
### Title: Slider for an rpanel
### Aliases: rp.slider rp.slider.change
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D density.draw <- function(panel) {
##D    plot(density(panel$x, bw = panel$h))
##D    panel
##D    }
##D panel <- rp.control(x = rnorm(50))
##D rp.slider(panel, h, 0.5, 5, log = TRUE, action = density.draw)
##D    
##D printer <- function(panel) {
##D   print(panel$h)
##D   panel
##D }
##D panel <- rp.control(x = rnorm(50), h=c(1,2,3))
##D rp.slider(panel, h, c(0.5,0.5,0.5), c(5,5,5),
##D   log = c(TRUE,TRUE,TRUE), action = printer,
##D   title=c('h','h1','h2'), initval=c(1,2,3))
##D     
##D # An example which changes the slider position through another widget
##D     
##D draw <- function(panel) {
##D   hist(panel$x)
##D   abline(v=panel$v, col="red", lty=2)
##D   panel
##D }
##D 
##D redraw <- function(panel) {
##D   rp.tkrreplot(panel, plot)
##D   panel
##D }
##D 
##D redraw1 <- function(panel) {
##D   rp.tkrreplot(panel, plot)
##D   rp.slider.change(panel, "slider", panel$v)
##D   panel
##D }
##D 
##D x <- rnorm(25)
##D panel <- rp.control(v = 0, x = x)
##D rp.tkrplot(panel, plot, draw, pos="right")
##D rp.slider(panel, v, min(x), max(x), redraw, name = "slider")
##D rp.doublebutton(panel, v, diff(range(x))/100, action=redraw1)
##D 
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.slider", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.surface")
### * rp.surface

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.surface
### Title: Interactive visualisation of a surface and its uncertainty
### Aliases: rp.surface

### ** Examples

## Not run: 
##D if (require(sm)) {
##D    with(trawl, {
##D       location  <- cbind(Longitude, Latitude)
##D       model     <- sm.regression(location, Score1, ngrid = 15, display = "none")
##D       longitude <- model$eval.points[ , 1]
##D       latitude  <- model$eval.points[ , 2]
##D       xgrid     <- as.matrix(expand.grid(longitude, latitude))
##D       S         <- sm.weight2(location, xgrid, model$h)
##D       covar     <- tcrossprod(S) * model$sigma^2
##D       rp.surface(model$estimate, covar, longitude, latitude, location, Score1)
##D    })
##D }
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.surface", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.tables")
### * rp.tables

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.tables
### Title: Interactive statistical tables
### Aliases: rp.tables
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   rp.tables()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.tables", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.text")
### * rp.text

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.text
### Title: Text boxes for a panel
### Aliases: rp.text rp.text.change
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D panel <- rp.control(x=1)
##D callback <- function(panel)
##D {
##D   rp.text.change(panel, "t2", panel$x)
##D   panel$x = panel$x+1
##D   panel
##D }
##D rp.text(panel, "This is a test", name="t1")
##D rp.text(panel ,"And so is this", font="Arial", foreground="white",
##D         background="navy", action=callback, name="t2")
##D rp.text(panel,"Here is some more text, this time across several lines.\n
##D                Here is some more text, this time across several lines.\n
##D                Here is some more text, this time across several lines.", name="t3")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.text", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.textentry")
### * rp.textentry

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.textentry
### Title: Text entry boxes for a panel
### Aliases: rp.textentry
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    plotf <- function(panel) {
##D       with(panel, {
##D 		 pars   <- as.numeric(pars)
##D          xgrid <- seq(0.1, max(c(pars[3], 5), na.rm = TRUE), length = 50)
##D          dgrid <- df(xgrid, pars[1], pars[2])
##D          plot(xgrid, dgrid, type = "l", col = "blue", lwd = 3)
##D          if (!is.na(pars[3])) {
##D             lines(rep(pars[3], 2), c(0, 0.95 * max(dgrid)), lty = 2, col = "red")
##D             text(pars[3], max(dgrid), as.character(pars[3]), col = "red")
##D             }
##D          })
##D       panel
##D       }
##D 
##D    panel <- rp.control(pars = c(5, 10, NA))
##D    rp.textentry(panel, pars, plotf, labels = c("df1", "df2", "observed"),
##D           initval = c(10, 5, 3))
##D    rp.do(panel, plotf)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.textentry", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.timer")
### * rp.timer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.timer
### Title: Creates a series of timed actions
### Aliases: rp.timer
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D   stopme <- function(panel) panel$count<=20
##D   callme <- function(panel) {
##D     print(panel$count)
##D     panel$count = panel$count+1
##D     panel
##D   } 
##D   panel <- rp.control(count=1)
##D   rp.timer(panel, 500, callme, stopme)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.timer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.tkrplot")
### * rp.tkrplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.tkrplot
### Title: rpanel calls for tkrplot and tkrreplot
### Aliases: rp.tkrplot rp.tkrreplot
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    draw <- function(panel) {
##D       plot(1:20, (1:20)^panel$h)
##D       panel
##D       }
##D    
##D    redraw <- function(panel) {
##D       rp.tkrreplot(panel, tkrp)
##D       panel
##D       }
##D 
##D    rpplot <- rp.control(title = "Demonstration of rp.tkrplot", h = 1)
##D    rp.tkrplot(rpplot, tkrp, draw)
##D    rp.slider(rpplot, h, action = redraw, from = 0.05, to = 2.00, resolution = 0.05)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.tkrplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rp.widget.dispose")
### * rp.widget.dispose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rp.widget.dispose
### Title: Removes a widget
### Aliases: rp.widget.dispose
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D  p1 <- rp.control()
##D  rp.button(p1, I, "press me", name="b1")
##D  rp.widget.dispose(p1, "b1")
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rp.widget.dispose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rpanel.package")
### * rpanel.package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rpanel-package
### Title: Simple interactive controls for R functions using the tcltk
###   package
### Aliases: rpanel-package rpanel
### Keywords: package iplot dynamic

### ** Examples

## Not run: 
##D    rp.gulls()
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rpanel.package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("worldbank")
### * worldbank

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: worldbank
### Title: Data on CO2 emissions, GDP, life.expectancy and population for
###   the countries of the world between 1960 and 2007
### Aliases: worldbank co2.emissions gdp life.expectancy population
### Keywords: iplot dynamic

### ** Examples

## Not run: 
##D    rp.bubbleplot(log(gdp), log(co2.emissions), 1960:2007, size = population, 
##D       col = life.expectancy,
##D       interpolate = TRUE, hscale = 1.5, vscale = 1.5)
## End(Not run)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("worldbank", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

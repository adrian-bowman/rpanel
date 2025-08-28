# d <- read.csv("/Volumes/adrian/research/george/Clyde/sepa20140114_1406_ClydeRiverRunData.csv")
# source("/Volumes/adrian/research/george/Clyde/clyde-prepare.r")
d <- read.csv("~/ownCloud/rpanel_1.1-4-not-yet-released/sepa20140114_1406_ClydeRiverRunData.csv")
source("~/ownCloud/rpanel_1.1-4-not-yet-released/clyde-prepare.r")

library(rpanel)

x <- cbind(d$Station, Depth = -as.numeric(d$Depth))
rp.plot4d(x, d$year, d$DO, zlab = "Year", ylab = "DO")

d1 <- subset(d, Depth == 0)
x <- cbind(Station = d1$DistanceFromWeir, "Day of year" = d1$doy)
rp.plot4d(x, d1$year, d1$DO, zlab = "Year", ylab = "DO")

d1 <- subset(d, Depth == 0)
x <- cbind(d1$year, "Day of year" = d1$doy)
rp.plot4d(x, d1$DistanceFromWeir, d1$DO, zlab = "Station", ylab = "DO")

source("~/ownCloud/madrid/sm.r")
source("~/ownCloud/madrid/sm-pam-utilities.r")
source("~/ownCloud/madrid/sm-fake-package.r")
source("~/ownCloud/rpanel_1.1-4-not-yet-released/rpanel/R/rp-plot4d.r")

# modela <- sm(DO ~  s(doy, period = 365) * s(year) * s(Station) , random = survey, data = d, subset = Depth == 0)
# save(modela, file = "modela.dmp")
load("~/ownCloud/rpanel_1.1-4-not-yet-released/modela.dmp")
summary(modela)
plot(modela, 1:3)
plot(modela, 1, partial.residuals = FALSE)
plot(modela, 2, partial.residuals = FALSE)
plot(modela, 3, partial.residuals = FALSE, ylim = c(-1.7, 2.7))
plot(modela, 4, include.lower.terms = TRUE, display = "persp")
plot(modela, 5, include.lower.terms = TRUE, display = "persp")
plot(modela, 6, include.lower.terms = TRUE, display = "persp")
plot(modela, 7, include.lower.terms = TRUE, reference = "no effect", hscale = 1.3)
plot(modela, 7, include.lower.terms = TRUE, deriv = "year", deriv.order = 1,
       reference = "no effect", hscale = 1.3)
plot(modela, 7, include.lower.terms = TRUE, order = c(1, 3, 2))
plot(modela, 7, include.lower.terms = TRUE, deriv = "year", deriv.order = 1,
		 reference = "no effect", order = c(3, 1, 2), hscale = 1.3)
plot(modela, 7, include.lower.terms = FALSE, deriv = "year", deriv.order = 1,
		 reference = "no effect")

# modelb <- sm(DO ~  s(doy, period = 365) * s(Station) + s(year) * s(Station) , random = survey, data = d, subset = Depth == 0)
# save(modelb, file = "~/Desktop/modelb.dmp")
summary(modelb)
source("~/ownCloud/madrid/sm.r")
plot(modelb, 4, include.lower.terms = TRUE)
plot(modelb, 4, include.lower.terms = TRUE, display = "persp", theta = 50)
plot(modelb, 5, include.lower.terms = TRUE, deriv = "year", deriv.order = 1, display = "persp",
		 theta = 50)
plot(modelb, 5, include.lower.terms = TRUE, deriv = "year", deriv.order = 1)

modela <- sm(DO ~  Tide.State + s(doy, period = 365) + s(Station) * s(year), random = survey, data = d, subset = Depth == 0)
summary(modela)
newdata <- data.frame(doy = c(20, 20), Station = c(10, 10), year = 2001 + c(20, 20)/365, Tide.State = c("ebb", "flood"))
newdata <- data.frame(doy = 20, Station = 10, year = 2001 + 20/365, Tide.State = factor("ebb", levels = levels(d$Tide.State)))
pred    <- predict(modela, newdata, se.fit = TRUE)

plot(modela, 1)
plot(modela, 1, se = TRUE)
plot(modela, 1, reference = "no effect")
plot(modela, 1, xlim = c(-2, 500), ylim = c(-20, 20))

plot(modela, 4)
plot(modela, 4, reference = "no effect")

plot(modela, 4, display = "persp")
plot(modela, display = "persp", x1lim = c(-20, 20), x2lim = c(1950, 2050))

plot(modela1)
modelb <- sm(DO ~ s(doy) * s(Station) + s(year), random = survey, data = d, subset = Depth == 0)
modelb <- sm(DO ~ s(doy, period = 365) * s(Station) * s(year), random = survey, data = d,
                 subset = Depth == 0, display = "none")
modelb <- sm(DO ~ s(Temperature) * s(Station) + s(year), random = survey, data = d, subset = Depth == 0,
                  display = "none")
summary(modelb)

load("~/Desktop/modelb.dmp")
plot(modelb, 7, display = "persp", include.lower.terms = TRUE)

plot(modelb, 1:3)
plot(modelb, "1 + 2")
plot(modelb, 4)
plot(modelb, 4, display = "persp", include.lower.terms = TRUE, include.mean = TRUE)
plot(modelb, 4, order = 2:1)
plot(modelb, 4, include.lower.terms = TRUE, include.mean = TRUE)
plot(modelb, 4, display = "persp", include.lower.terms = TRUE, include.mean = TRUE)
modelc <- sm(DO ~ s(Station, doy, year), random = survey, data = d, subset = Depth == 0)
modelc <- sm(DO ~ s(doy) * s(Station) * s(year), random = survey, data = d, subset = Depth == 0, display = "none")
summary(modelc)
plot(modelc, 7)
plot(modelc, 7, order = 3:1, hscale = 1.5)
plot(modelc, 6, reference = "no effect")
plot(modelc, 7, reference = "no effect")

dd <- subset(d, Depth == 0)
table(as.numeric(d1$survey))
nlevels(d1$survey)

mdlfit  <- array(NA, dim = c(rep(30, 3), nlevels(d$Depth)))
mdl3    <- array(NA, dim = c(rep(30, 3), nlevels(d$Depth)))
mdl3se  <- array(NA, dim = c(rep(30, 3), nlevels(d$Depth)))
for (i in 1:nlevels(d$Depth)) {
   d1        <- subset(d, Depth == levels(Depth)[i])
   DO        <- d1$DO
   station   <- d1$Station
   day       <- d1$doy
   location1 <- d1$DistanceFromWeir
   location2 <- d1$doy
   location  <- cbind(station = d1$DistanceFromWeir, day = d1$doy)
   year      <- d1$year
   
   # The period argument doesn't handle things properly
   model0    <- sm(DO ~ s(location, period = c(365, NA)) + s(year))
   model0    <- sm(DO ~ s(location) + s(year))
   pinf <- plot(model0, 1)
   
   dfrm <- data.frame(DO, station, day, year)
   modela <- sm(DO ~ s(day) + s(station) + s(year), data = dfrm)
   modelb <- sm(DO ~ s(day) * s(station) + s(year), data = dfrm)
   plot(modelb, 4)
   plot(modelb, 4, reference = "no effect", col.palette = col.pal)
   
   model     <- sm(DO ~ s(location) * s(year))
   model1    <- sm(DO ~ s(station) + s(day) + s(year))
   plot(model1, partial.residuals = TRUE)
   x         <- cbind(station = d1$DistanceFromWeir, day = d1$doy, year = d1$year)
   model4    <- sm(DO ~ s(x))
   plot(model4)
   model3    <- sm(DO ~ s(station) * s(day) * s(year))
   summary(model3)
   plot(model3, 7, include.lower.terms = FALSE, include.lower.reference = FALSE, reference = "no effect")
   plot(model3, 7, include.lower.terms = TRUE, include.lower.reference = FALSE, reference = "no effect")
   plot(model3, 6, include.lower.terms = TRUE, include.lower.reference = TRUE, reference = "no effect")
   plot(model3, 6, reference = "no effect")
   # model     <- gam(y ~ ti(location1, location2) + ti(z) + ti(location1, location2, z))
   pinf  <- plot(model, 1, partial.residuals = FALSE, col.palette = col.pal, ngrid = 100)
   pinf  <- plot(model, 2)
   pinf  <- plot(model, 3)
   pinf  <- plot(model, 3, se = TRUE)
   pinf  <- plot(model, 3, reference = "no effect")
   pinf3 <- plot(model3)
   pinf3 <- plot(model3, display = "none")
   pinf3[[1]]$st.error <- c(pinf[[1]]$est) / pinf[[1]]$st.error
   plot(model3)
   plot(model3, plotinfo = pinf3, se = TRUE)
   loc1      <- seq(min(location1), max(location1), length = 30)
   loc2      <- seq(min(location2), max(location2), length = 30)
   zz        <- seq(min(z), max(z), length = 30)
   newd      <- expand.grid("location1" = loc1, "location2" = loc2, "z" = zz)
   # mdlfit[ , , , i]  <- predict(model, newd)
   mdlfit[ , , , i]  <- predict(model, newd)
   mdl       <- predict(model, newd, type = "terms", se.fit = TRUE)
   mdl3[ , , , i]    <- mdl$fit[ , 3]
   mdl3se[ , , , i]  <- mdl$se.fit[ , 3]
   cat(i/nlevels(d$Depth), "")
   if (i == nlevels(d$Depth)) cat("\n")
}
model.fit <- list(x = cbind(loc1, loc2), z = zz, y = mdlfit)
model.3   <- list(x = cbind(loc1, loc2), z = zz, y = mdlfit)
model.3se <- list(x = cbind(loc1, loc2), z = zz, y = mdlse)

x  <- cbind(Station = d$Station, "Day of year" = d$doy)
library(RColorBrewer)
col.pal <- rev(colorRampPalette(brewer.pal(9, "BuGn"))(100))
rp.plot4d(x, d$year, d$DO, group = d$Depth, col.palette = col.pal,
             group.name = "Depth", zlab = "Year", ylab = "DO")
rp.plot4d(x, d$year, d$DO, model.fit, group = d$Depth, col.palette = col.pal,
             group.name = "Depth", zlab = "Year", ylab = "DO")


# This is the best one
d1      <- subset(d, Depth == 0)
x       <- cbind(Station = d1$Station, "Day of year" = d1$doy)
library(colorspace)
col.pal <- heat_hcl(50, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5))
col.pal <- sequential_hcl(50, c. = c(120, 0), l = c(50, 90))

x       <- cbind(Station = d$Station, Depth = -d$Depth)
rp.plot4d(x, d$year, d$DO, zlab = "Year", ylab = "DO")


library(RColorBrewer)
col.pal <- rev(colorRampPalette(brewer.pal(9, "BuGn"))(100))
x       <- cbind(Station = d1$Station, "Day of year" = d1$doy)
# This one shows the shading when a standard Quartz window is used.
rp.plot4d(x, d1$year, d1$DO, zlab = "Year", ylab = "DO", col.palette = col.pal,
      panel = FALSE, z.window.pars = c(1980, 2))
# Contrast this with the uniform window
rp.plot4d(x, d1$year, d1$DO, zlab = "Year", ylab = "DO", col.palette = col.pal,
      panel = FALSE, z.window.pars = c(1980, 2), z.window = "uniform")
rp.plot4d(x, d1$year, d1$DO, zlab = "Year", ylab = "DO", col.palette = col.pal)
rp.plot4d(x, d1$year, d1$DO, zlab = "Year", ylab = "DO")
rp.plot4d(x, d1$year, d1$DO, model, zlab = "Year", ylab = "DO")


rgb2hsv(col2rgb("white"))

library(colorspace)
col.pal <- heat_hcl(50, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5))

par(mfrow = c(5, 1))
for (alpha in seq (0, 1, length = 5)) {
   colour   <- col.pal
   clr      <- col2rgb(colour)
   clr      <- rgb2hsv(clr)
   clr      <- alpha * c(0, 0, 1) + (1 - alpha) * clr
   # clr[2, ] <- clr[2, ] * alpha
   # clr      <- apply(clr, 2, function(x) hsv(x[1], x[2], x[3], alpha))
   clr      <- apply(clr, 2, function(x) hsv(x[1], x[2], x[3]))
   pal(clr)
}
par(mfrow = c(1, 1))

library(colorspace)
pal <- function(col, border = "light gray") {
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
par(mfrow = c(5, 1))
for (alpha in seq (0, 1, length = 5))
   # pal(terrain_hcl(50, c = c(65, 0), l = c(45, 95), power = c(1/3, 1.5), alpha = alpha))
   pal(heat_hcl(50, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5), alpha = alpha))
par(mfrow = c(1, 1))


# ggplot has the advantage of showing the change in sampling patterns more clearly?
# However, the matrix layout is a bit distracting.  A row layout would be better but can't fit.
d1$yr <- cut(d1$year, 9)
ggplot(d1, aes(Station, doy, col = DO)) + geom_point() + facet_wrap(~ yr)

# Why are the colours of the points not the same in the plot with and without the model?
# What does ggplot do about colour contours and colours?
# Why does the vertical axis of the location plot not match that of the colour scale?
# The slider for the size of the probe needs a label.
# Can the X11 font be improved?
# Better choice of colour palette?  See ggplot colours.
# There are no Depth 0 measurements in recent years.  Odd.
#       table(floor(d$year)[d$Depth == 0])
# The p-spline model can wraparound doy and avoid ballooning at Christmas.

# How can Depth be included?  Common ones are 0,2,3,6,7.5.
# Separate plots can be given common DO scales through col.breaks.
#    This doesn't seem to make much difference.
rng  <- range(d$DO)
del  <- 0.04 * diff(rng)
brks <- seq(rng[1] - del, rng[2] + del, length = 21)
d1      <- subset(d, Depth == 6)
x       <- cbind(Station = d1$DistanceFromWeir, "Day of year" = d1$doy)
rp.plot4d(x, d1$year, d1$DO, zlab = "Year", ylab = "DO", col.breaks = brks)
# There are strong effects of Station, doy and year so these really have to be present.

# Add a grouping factor
d1 <- subset(d, Depth %in% c(0, 2, 3, 6, 7.5))
d1$Depth <- factor(d1$Depth)
x  <- cbind(Station = d1$Station, "Day of year" = d1$doy)
library(RColorBrewer)
col.pal <- rev(colorRampPalette(brewer.pal(9, "BuGn"))(100))
rp.plot4d(x, d1$year, d1$DO, group = d1$Depth, col.palette = col.pal,
             group.name = "Depth", zlab = "Year", ylab = "DO")
rp.plot4d(x, d1$year, d1$DO, model.fit, group = d1$Depth, col.palette = col.pal,
             group.name = "Depth", zlab = "Year", ylab = "DO")
rp.plot4d(x, d1$year, d1$DO, group = d1$Depth,
             group.name = "Depth", zlab = "Year", ylab = "DO")
x  <- cbind(Station = d1$Station, "Day of year" = d1$doy)
rp.plot4d(x, d1$year, group = d1$Depth,
             group.name = "Depth", zlab = "Year", ylab = "DO")
x  <- cbind("Day of year" = d1$doy, "DO" = d1$DO)
grp <- factor(d1$year <= 1985)
rp.plot4d(x, d1$Station, group = d1$Depth,
             group.name = "Depth", zlab = "Year", ylab = "DO")
rp.plot4d(x, d1$Station, grp, group = d1$Depth,
             group.name = "Depth", zlab = "Year", ylab = "DO")


x  <- cbind(d1$doy, d1$DO)
rp.plot4d(x, d1$Station, group = d1$Depth, location.plot = FALSE)
x  <- cbind(d1$Station, d1$DO)
rp.plot4d(x, d1$doy, group = d1$Depth, location.plot = FALSE)
grp <- factor(c("before 1985", "after 1985")[1 + as.numeric(floor(d1$year) <= 1985)])
x  <- cbind(d1$doy, d1$DO)
rp.plot4d(x, d1$Station, grp, group = d1$Depth,
   col.palette = c("red", "green"), location.plot = FALSE)


# Look at the Temperature and Salinity patterns
# These may add little beyond the Station:doy:year interaction?
d1      <- subset(d, Depth == 0)
x       <- cbind(Station = d1$DistanceFromWeir, "Day of year" = d1$doy)
rp.plot4d(x, d1$year, d1$Temperature, zlab = "Year", ylab = "DO")
rp.plot4d(x, d1$year, d1$Salinity,    zlab = "Year", ylab = "DO")

# Mimic ggplot
library(ggplot2)
qplot(1:10, 1:10)
par(mar = c(3, 3, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
plot(1:10, type = "n", axes = FALSE)
usr <- par("usr")
rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
grid(col = "white", lty = 1)
axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
        col.axis = grey(0.6), cex.axis = 0.8)
axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
        col.axis = grey(0.6), cex.axis = 0.8)
points(1:10, 1:10, pch = 16)
            
         }

d2 <- subset(d, DistanceFromWeir == 0 & Depth == 0)
rp.plot3d(d2$year, d2$DO, d2$doy)

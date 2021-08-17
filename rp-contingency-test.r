#     A cartoon for contingency tables

setwd("~/ownCloud/rpanel_1.1-4-not-yet-released")

library(rpanel)

x <- t(matrix(c(87,73,64,15,25,16,109,84,92), ncol = 3))
rownames(x) <- c("red", "yellow", "blue")
colnames(x) <- c("round", "square", "oblong")

#     Doll & Hill (1950) data on smoking and lung cancer
#     Males and females together
x           <- matrix(c(688, 650, 21, 59), ncol = 2)
#     Males only
x           <- matrix(c(647, 622, 2, 27), ncol = 2)
#     Females only
x           <- matrix(c(41, 28, 19, 32), ncol = 2)
rownames(x) <- c("cases", "controls")
colnames(x) <- c("smoker", "non-smoker")
margins     <- FALSE

source("rp-contingency.r")
pdf("figures/contingency-1.pdf")
rp.contingency(x, structure = "row populations", panel = FALSE, margins = margins)
dev.off()
pdf("figures/contingency-2.pdf")
rp.contingency(x, structure = "row populations", scale = "proportions", panel = FALSE,
   margins = margins)
dev.off()
rp.contingency(x, structure = "row populations", panel = FALSE, display = "plots", margins = margins)
rp.contingency(x, structure = "row populations", panel = FALSE, display = "plots",
   scale = "proportions", col.data = rainbow_hcl(2)[1], margins = margins)
pdf("figures/contingency-5.pdf")
rp.contingency(x, structure = "row populations", panel = FALSE, display = "plots",
   scale = "proportions", reference.model = TRUE, superimpose = TRUE,
   margins = margins, col.data = rainbow_hcl(2)[1], col.model = rainbow_hcl(2)[2])
dev.off()
pdf("figures/contingency-6.pdf")
rp.contingency(x, structure = "row populations", panel = FALSE, display = "plots",
   scale = "proportions", reference.model = TRUE, superimpose = TRUE, variation = TRUE,
   margins = margins, col.data = rainbow_hcl(2)[1], col.model = rainbow_hcl(2)[2])
dev.off()

chisq.test(x)
chisq.test(x, correct = FALSE)
chisq.test(x, simulate.p.value = TRUE)
1 - pchisq(4.9105, 1)
grd <- seq(0.1, 6, length = 50)
par(mar = c(0, 0, 0, 0) + 0.1)
plot(c(0, 6), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
denstrip(grd, dchisq(grd, 1), 0.5, 1, colmax = "green")
axis(1, pos = 0.5, mgp = c(1.5, 0.5, 0), tcl = -0.3)

library(denstrip)
xf.control <- 60 - 19
xf.case    <- 60 - 32
nf         <- 60
xm.control <- 649 - 2
xm.case    <- 649 - 27
nm         <- 649
pf.case    <- xf.case / nf
pf.control <- xf.control / nf
pm.case    <- xm.case / nm
pm.control <- xm.control / nm
y  <- c(pf.case, pf.control, pm.case, pm.control)
se.f <- sqrt(pf.case * (1 - pf.case) / nf + pf.control * (1 - pf.control) / nf)
se.m <- sqrt(pm.case * (1 - pm.case) / nm + pm.control * (1 - pm.control) / nm)
rng <- range(c(y, pf.case - 3 * se.f, pf.case + 3 * se.f, pf.control - 3 * se.f, pf.control + 3 * se.f,
                pm.case - 3 * se.m, pm.case + 3 * se.m, pm.control - 3 * se.m, pm.control + 3 * se.m))
rng <- c(0, 1.05)
pdf("figures/proportions.pdf", height = 2)
par(mar = c(3, 0, 0, 0) + 0.1, mgp = c(1.5, 0.5, 0), tcl = -0.3)
plot(rng, c(0, 1), type = "n", axes = FALSE, xlab = "Proportion of smokers", ylab = "", xaxs = "r")
# axis(1, pos = 0, cex.axis = 1, mgp = c(2, 0.5, 0), tcl = -0.3)
axis(1)
points(y, rep(0.3, 4), col = c("pink", "pink", "blue", "blue"), pch = c(1, 16, 1, 16),
       cex = c(rep(1, 2), rep(sqrt(nm/nf), 2)))
grd      <- seq(-3, 3, length = 51)
del      <- diff(grd)[1] / 2
denwidth <- 0.2
srt      <- sort(c(pf.case, pf.control))
par(new = TRUE)
plot(rng - srt[1], c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
denstrip(diff(srt) + (grd - del) * se.f, dnorm(grd), at = 0.7, width = denwidth, colmax = "pink", horiz = TRUE)
at  <- pretty(rng - srt[1])
at1 <- min(max(at[at <= max(diff(srt) - 3 * se.f, min(at))]), 0)
at2 <- max(min(at[at >= min(diff(srt) + 3 * se.f, max(at))]), 0)
# axis(1, at = at[at >= at1 & at <= at2], pos = 0.7, col = "grey", col.axis = "grey",
       	         # mgp = c(1.5, 0.5, 0), tcl = -0.3)
axis(1, at = c(0, 0.2, 0.4), pos = 0.7, col = "grey", col.axis = "grey",
       	         mgp = c(1.5, 0.5, 0), tcl = -0.3)
srt      <- sort(c(pm.case, pm.control))
par(new = TRUE)
plot(rng - srt[1], c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
denstrip(diff(srt) + (grd - del) * se.m, dnorm(grd), at = 0.7, width = denwidth, colmax = "blue", horiz = TRUE)
at  <- pretty(rng - srt[1])
at1 <- min(max(at[at <= max(diff(srt) - 3 * se.m, min(at))]), 0)
at2 <- max(min(at[at >= min(diff(srt) + 3 * se.m, max(at))]), 0)
# axis(1, at = at[at >= at1 & at <= at2], pos = 0.7, col = "grey", col.axis = "grey",
       	         # mgp = c(1.5, 0.5, 0), tcl = -0.3)
axis(1, at = c(0, 0.05), c("0", "0.05"), pos = 0.7, col = "grey", col.axis = "grey",
         mgp = c(1.5, 0.5, 0), tcl = -0.3)
axis(1, at = c(0.05), pos = 0.7, col = "grey", col.axis = "grey",
         mgp = c(1.5, 0.5, 0), tcl = -0.3)
dev.off()


source("rp-contingency.r")
rp.contingency(x, structure = "row populations",
   display = "plots", scale = "proportions",
   reference.model = TRUE, superimpose = TRUE, variation = TRUE, 
   panel = FALSE)
rp.contingency(x, structure = "row populations",
   display = "plots", scale = "proportions",
   reference.model = TRUE, superimpose = TRUE, variation = TRUE, 
   panel = FALSE)
chisq.test(x)

source("rp-contingency.r")
rp.contingency(x, display = "plots", 
   reference.model = TRUE, superimpose = FALSE,
   scale = "proportions", variation = FALSE,   structure = "row populations")

source("rp-contingency.r")
rp.contingency(x, display = "graphs")
source("rp-contingency.r")
rp.contingency(x, display = "graphs", fitted.model.showing = TRUE)


# ggplot2 graphics

library(ggplot2)

rtots  <- apply(x, 1, sum)
ctots  <- apply(x, 2, sum)
rprops <- sweep(x, 1, rtots, "/")
ylab   <- "row proportions"
props  <- rprops
props0 <- ctots / sum(ctots)
nms1 <- rownames(x)
nms2 <- colnames(x)
props[1] <- 1
d <- data.frame(props = c(props), x = rep(0.5, length(props)), nms1, nms2)
plt <- qplot(x, props, xlab = "", ylab = "proportions", data = d) 
plt + facet_grid(nms1 ~ nms2) + 
      scale_x_continuous(limits = c(0, 1),
           breaks = c(0, 1), label = rep("", 2), expand = rep(0, 2)) + 
      scale_y_continuous(limits = c(0, 1))

# use margins = TRUE for totals?


# Lattice graphics

library(lattice)

panel.fn <- function(x, y) {
   panel.xyplot(x, y)
   panel.lines(x, y)
   panel.xyplot(x, props0, col = "red")
   panel.lines(x, props0, col = "red", lty = 2)
   }
xyplot(props ~ nms[ , 2] | nms[ , 1], 
          ylim = c(0, 1), xlab = "", layout = c(1, nrow(x)),
          strip = FALSE, strip.left = TRUE, as.table = TRUE, ylab = ylab,
          panel = panel.fn)

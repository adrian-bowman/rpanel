#       PCA3: rpanel function for PCA
#                    with three variable.

# Test data
turtles <- read.table("P:/data/3h/MultStats/turtles.TXT", header=T)
names(turtles)  #"Length"  "Width"   "Breadth" "Gender" 
summary(turtles)

turtles.f <- turtles[turtles$Gender=='female',-4]
turtles.m <- turtles[turtles$Gender=='male',-4]

for (i in 1:3) {
	assign(paste('x',i,sep=''),turtles[,i]) 
	assign(paste('x',i,'lab',sep=''),names(turtles)[i])
} 

objects()

rp.PCA3 <- function(x1, x2, x3, x1lab  = NA, x2lab = NA, x3lab = NA) {

   if (is.na(x1lab)) x1lab <- deparse(substitute(x1))
   if (is.na(x2lab)) x2lab <- deparse(substitute(x2))
   if (is.na(x3lab)) x2lab <- deparse(substitute(x3))

   ngrid <- 20
   x1lo   <- min(x1) - 0.05 * diff(range(x1))
   x1hi   <- max(x1) + 0.05 * diff(range(x1))
   x2lo   <- min(x2) - 0.05 * diff(range(x2))
   x2hi   <- max(x2) + 0.05 * diff(range(x2))
   x3lo   <- min(x3) - 0.05 * diff(range(x3))
   x3hi   <- max(x3) + 0.05 * diff(range(x3))
 #  xgrid <- seq(xlo, xhi, length = ngrid)
 #  zgrid <- seq(zlo, zhi, length = ngrid)
 #  smatx <-   matrix(rep(xgrid, ngrid), ncol = ngrid)
 #  smatz <- t(matrix(rep(zgrid, ngrid), ncol = ngrid))
 #  smat  <- array(c(mean(y) + 0 * smatx,
 #                      coef(lm(y ~ x))[1]     + coef(lm(y ~ x))[2]     * smatx,
 #                      coef(lm(y ~ z))[1]     + coef(lm(y ~ z))[2]     * smatz,
 #                      coef(lm(y ~ x + z))[1] + coef(lm(y ~ x + z))[2] * smatx
 #                                             + coef(lm(y ~ x + z))[3] * smatz),
 #                      dim = c(ngrid, ngrid, 4))
   n   <- length(x1)
 #  fv  <- matrix(c(rep(mean(y), n),   
 #               fitted(lm(y ~ x)), 
 #               fitted(lm(y ~ z)), 
 #               fitted(lm(y ~ x + z))), ncol = 4)
 #  both           <- paste(xlab, "and", zlab)
 #  dimnames(smat) <- list(NULL, NULL, c("No effects", xlab, zlab, both))
 #  dimnames(fv)   <- list(NULL, c("No effects", xlab, zlab, both))
 #  ylo    <- min(ylo, smat)
 #  yhi    <- max(yhi, smat)
   x1scale <- pretty(c(x1lo, x1hi))
   x2scale <- pretty(c(x2lo, x2hi))
   x3scale <- pretty(c(x3lo, x3hi))
   x1scale <- x1scale[x1scale >= x1lo & x1scale <= x1hi]
   x2scale <- x2scale[x2scale >= x2lo & x2scale <= x2hi]
   x3scale <- x3scale[x3scale >= x3lo & x3scale <= x3hi]

   x1adj1 <- mean(c(x1lo, x1hi))
   x2adj1 <- mean(c(x2lo, x2hi))
   x3adj1 <- mean(c(x3lo, x3hi))
   x1adj2 <- (x1hi - x1lo) / 2
   x2adj2 <- (x2hi - x2lo) / 2
   x3adj2 <- (x3hi - x3lo) / 2
   x1 <- (x1 - x1adj1) / x1adj2
   x2 <- (x2 - x2adj1) / x2adj2
   x3 <- (x3 - x3adj1) / x3adj2
#   x1grid <- (x1grid - x1adj1) / x1adj2
#   x3grid <- (x3grid - x3adj1) / x3adj2
#   smat  <- (smat - x2adj1) / x2adj2
#   fv    <- (fv - x2adj1) / x2adj2
   x1scale.adj <- (x1scale - x1adj1) / x1adj2
   x2scale.adj <- (x2scale - x2adj1) / x2adj2
   x3scale.adj <- (x3scale - x3adj1) / x3adj2
   rx1 <- c(-1, 1)
   rx2 <- c(-1, 1)
   rx3 <- c(-1, 1)

library(rgl)
open3d()
bg3d(col = c("white", "black"))
points3d(x1, x2, x3, size = 3, color = "red")  #turtles[,1:3]
view3d(-30, 30, fov = 1)
bbox3d(color=c('white','black','white','white','white'),xlab=c(x1lab))



   rgl.lines(rx1[c(1,2,2,2,2,1,1,1)], rx2[rep(1,8)], rx3[c(1,1,1,2,2,2,2,1)], 
                col = "black")
   rgl.lines(rx1[c(1,2,2,2,2,1,1,1)], rx2[rep(2,8)], rx3[c(1,1,1,2,2,2,2,1)],
                col = "black")
   for (i in 1:2) for (j in 1:2) 
      rgl.lines(rx1[c(i,i)], rx2[c(1,2)], rx3[c(j,j)], col = "black")

   rgl.texts(mean(rx1), min(rx2),  min(rx3), x1lab, col = "black")
   delta <- 0.1
   nx2ticks <- length(x2scale)
   if (nx2ticks/2 - floor(nx2ticks/2) > 0) x2pos <- 1 / (nx2ticks - 1) else x2pos <- 0
   rgl.texts(c(0,-1 - 2 * delta,  -1 - 2 * delta),
             c(-1 - 2 * delta,  x2pos,            -1 - 2 * delta),
             c( 1 + 2 * delta,  -1 - 2 * delta,  0), 
             c(x1lab, x2lab, x3lab), col = 'black')  #adj = c(0.5, 0.5, 1),
   rgl.texts((x1scale - x1adj1) / x1adj2, -1 - delta,  1 + delta, as.character(x1scale), col = 'black')
   rgl.texts(-1 - delta, (x2scale - x2adj1) / x2adj2, -1 - delta, as.character(x2scale), col = 'black')
   rgl.texts(-1 - delta, -1 - delta, (x3scale - x3adj1) / x3adj2, as.character(x3scale), col = 'black')
   rgl.segments((x1scale - x1adj1) / x1adj2, -1, 1,(x1scale - x1adj1) / x1adj2, -1 - delta/4,  1 + delta/4, col = 'black')
   rgl.segments(-1, (x2scale - x2adj1) / x2adj2, -1,-1 - delta/4, (x2scale - x2adj1) / x2adj2, -1 - delta/4, col = 'black')
   rgl.segments(-1,  -1, (x3scale - x3adj1) / x3adj2,-1 - delta/4,  -1 - delta/4, (x3scale - x3adj1) / x3adj2, col = 'black')



   panel.name <- rp.panelname()
   spin.panel <- rp.control("Spin plot", x = x1, y = x2, z= x3, 
                     xlab = x1lab, ylab = x2lab, zlab = x3lab, theta = -30, phi = 30, 
                     realname = panel.name, fov = 1, current.PCA = "None") #xgrid = x1grid, zgrid = x3grid,  smat = smat, fv = fv)
   spin.panel <- rp.doublebutton(spin.panel, theta, -1, title = "Theta", action = rp.rotate)
   spin.panel <- rp.doublebutton(spin.panel, phi, -1,   title = "Phi",   action = rp.rotate)
   spin.panel <- rp.radiogroup(spin.panel, PCs,
                     c("None", "PC1", "PC1 & PC2", "PC1 & PC2 & PC3"),
                     title = "Show PCs", action = rp.PCA.fn)
   spin.panel <- rp.checkbox(spin.panel, regression.showing,
                     title = "Show regression", action = rp.regression2.residuals.fn)
   assign(panel.name, spin.panel, env = .GlobalEnv)
   }

rp.rotate <- function(panel) {
   with(panel, {
      if (phi < -90) phi <- -90
      if (phi >  90) phi <-  90
      rgl.viewpoint(theta = theta, phi = phi, fov = fov)
      })
   panel
   }


   library(rgl)
   rgl.open()
   rgl.bg(col = c("white", "black"))
   rgl.points(x1, x2, x3, size = 3, color = "red")
   rgl.viewpoint(-30, 30, fov = 1)
   
   rgl.lines(rx1[c(1,2,2,2,2,1,1,1)], rx2[rep(1,8)], rx3[c(1,1,1,2,2,2,2,1)], 
                col = "black")
   rgl.lines(rx1[c(1,2,2,2,2,1,1,1)], rx2[rep(2,8)], rx3[c(1,1,1,2,2,2,2,1)],
                col = "black")
   for (i in 1:2) for (j in 1:2) 
      rgl.lines(rx1[c(i,i)], rx2[c(1,2)], rx3[c(j,j)], col = "black")

rp.PCA.fn <- function(panel) {
   with(panel, {
      if (current.PCA != "None") rgl.pop()  rgl.clear()
	if (PCs != 'None') 
		data.pca <- princomp(cbind(x1,x2,x3))
		pc <- loadings(data.pca)[,1]
		pc.scores <- data.pca$scores[,1]
		pc.upp <- c(mean(x1),mean(x2),mean(x3))+max(pc.scores)*pc
		pc.low <- c(mean(x1),mean(x2),mean(x3))+min(pc.scores)*pc
	      rgl.lines(c(pc.upp[1],pc.low[1]),c(pc.upp[2],pc.low[2]),c(pc.upp[3],pc.low[3]),size=3,col='blue') 
		rgl.texts(pc.upp[1],pc.upp[2],pc.upp[3],"PC1",col='blue')
}

rp.regression2.model.fn <- function(panel) {
   with(panel, {
      if (current.model != "None") {
         rgl.pop()
         if (residuals.showing) rgl.pop()
         }
      if (model != "None") {
         rgl.surface(xgrid, x3grid, smat[,, model], alpha = 0.5)
         if (residuals.showing)
            rgl.segments(x, fv[, model], x3, x, x2, x3, col = "green")
         }
      })
   panel$current.model <- panel$model
   panel
   }

rp.pca3.projections.fn <- function(jplot3) {
   with(jplot3, {
      if (model != "None") {
         if (residuals.showing)
            rgl.segments(x, fv[, model], x3, x, x2, x3, col = "green")
         else rgl.pop()
         }
      })
   jplot3
   }

rgl.segments <- function(x0, y0, z0, x1, y1, z1, ...)
         rgl.lines(c(rbind(x0, x1)), c(rbind(y0, y1)), c(rbind(z0, z1)), ...)




rp.regression2(Giving, Employ, Attend)

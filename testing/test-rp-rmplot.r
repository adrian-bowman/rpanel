detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

y <- matrix(rnorm(150), ncol = 3)
rp.rmplot(y, fac = rep(1:2, each = 25))
rp.rmplot(c(y), id = factor(rep(1:25, 6)), timept = rep(1:3, each = 50))

LH     <- luthor[,2:16]
gp     <- factor(luthor[,1])
times  <- c(1:5, (5 + (1:10) / 2))
rp.rmplot(log(LH), fac = gp, timept = times, 
   type = "mean+bar", col = c(2, 4), lty = 3:4, lwd = 3,
   xlabels = rep("a", 15), panel = FALSE)
gp[3] <- NA
rp.rmplot(log(LH), fac = gp, timept = times)
levels(gp) <- 1:3
gp[1] <- 3
rp.rmplot(log(LH), fac = gp, timept = times, panel = FALSE, type = "band")
rp.rmplot(log(LH), fac = gp, timept = times)
gp <- factor(as.character(gp))
gp[1:15] <- NA
rp.rmplot(log(LH), fac = gp, timept = times)

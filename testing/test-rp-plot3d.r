#     Test code for rp.plot3d
     
detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

n <- 1400
z <- 1:n
x <- 2 * z /n + rnorm(n)
y <- 4 * z / n + rnorm(n)

x3 <- cbind(xx = x, yy = y, zz = z)
x3 <- cbind(x, y, z)
colnames(x3) <- NULL

rp.plot3d(x3, xlab = "xlab")
rp.plot3d(x, y, z)

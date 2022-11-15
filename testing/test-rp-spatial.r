#     Test code for rpanel spatial functions

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

rp.geosim()

RFgetModelNames(type="positive definite", domain="single variable",
                iso="isotropic") 

## our choice is the exponential model;
## the model includes nugget effect and the mean:
model <- RMexp(var=5, scale=10) + # with variance 4 and scale 10
 RMnugget(var=1) + # nugget
 RMtrend(mean=0.5) # and mean
 
ratio <- 5
angle <- pi/4
ngrid <- 25
model <- RMmatern(nu = 0.5, scale = 1/sqrt(2)) # , var=5^2, scale=1/sqrt(2))
             # Aniso = diag(c(1, 1/ratio)) %*%
                     # matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)),
                            # ncol = 2))
x.seq <- seq(0, 1, length=ngrid) 
y.seq <- seq(0, 1, length=ngrid)
simu <- RFsimulate(model, x=x.seq, y=y.seq)
image(matrix(simu$variable1, nrow = ngrid))

RMmodel gives the Aniso information (RMangle)

library(geoR)
temp <- grf(50^2, grid = "reg", cov.model = "matern",
            cov.pars = c(500, 0.25), nugget = 0, messages = FALSE,
            kappa = 0.5,
            aniso.pars = c(0, 1))

library(fields)
grid <- list(x = seq(0, 5, length = 100), y = seq(0, 5, length = 100)) 
obj  <- circulantEmbeddingSetup(grid, Covariance = "Exponential", aRange=.5)
look <- circulantEmbedding(obj)
image.plot(grid[[1]], grid[[2]], look) 


rp.firth()

rp.mururoa()




mururoa.list$cov.pars
mururoa.list$nugget
mururoa.list$trend.fn
ptsm        <- as.matrix(expand.grid(seq(0, 100, by = 0.5),
                                     seq(0, 50, by = 0.5)))
trendmurmat <- apply(ptsm, 1, mururoa.list$trend.fn)
trendmurmat <- matrix(trendmurmat, ncol = 101)
field <- grf(nrow(ptsm), ptsm, cov.model = "gaussian",
             cov.pars = c(1, 1), 
             kappa = 4, nugget = 0, messages = FALSE)
fieldm <- matrix(field$data, nrow = 201)
filled.contour(seq(0, 100, by = 0.5), seq(0, 50, by = 0.5),
     fieldm)
sd(c(fieldm))
field <- grf(201^2, grid = "reg", cov.model = "matern",
             cov.pars = c(0.04, 50), 
             kappa = 4, nugget = 0, messages = FALSE)
fieldm <- matrix(field$data, nrow = 201)
filled.contour(fieldm)
sd(c(fieldm))
nug  <- grf(nrow(ptsm), ptsm, cov.model = "pure.nugget",
            cov.pars = c(mururoa.list$nugget, 0), 
            nugget = 0, messages = FALSE)
true.surface <- trendmurmat + fieldm
image(trendmurmat)
image(fieldm)
image(true.surface)
sd(c(trendmurmat))
sd(c(fieldm))
sd(c(nug$data))

source("rp-firth.r")
rp.firth()


source("fake-package.r")
rp.firth(file = file.path(getwd(), "firth.dmp"))


spatial.samp <-function(arg)
   rp.rosyth(parameters = arg)
hideargument <- function(f, arg)
	function() f(arg)
spatial.sampling <- hideargument(spatial.samp, 
                       list(strat.effect = rep(0, 4)))
spatial.sampling()
rm(setRF)
rm(hideargument)


rm(list = ls())
load("rpanel/R/sysdata.rda")
mururoa.list$cov.pars <- c(0.2^2, 15)
mururoa.list$nugget   <- 0.1^2
mururoa.list$trend.fn <- function(x) 
              exp(-0.5*(x[1]-70)^2/50^2)*exp(-0.5*(x[2]-25)^2/25^2)
firth.list$cov.pars <- c(0.35, 15)
firth.list$nugget   <- 0.25
firth.list$trend.fn <- function(x) (120 - 0.01 * (x[1] - 100)^2) / 20
firth.list$strat.effect <- c(0, 0.5, 1, 2)
save(list = ls(), 
  file = "rpanel/R/sysdata.rda")

load("rpanel/R/sysdata.rda")
source("fake-package.r")
rp.firth()
rp.mururoa()


rm(list = ls())
load("rpanel/data/sysdata.rda")
# names(mururoa.list)
mururoa.list$trend.fn <- 
     function(x) 30 - 0.01*(x[1] - 70)^2 - 0.02*(x[2] - 25)^2
# x <- seq(0, 100, by = 0.5)
# y <- seq(0,  50, by = 0.5)
# ptsm <- as.matrix(expand.grid(x, y))
# trendmurmat <- apply(ptsm, 1, mururoa.list$trend.fn)
# trendmurmat <- matrix(trendmurmat, ncol = 101)
# image(seq(0, 100, by = 0.5), seq(0,  50, by = 0.5), trendmurmat)
# range(trendmurmat)
mururoa.list$cov.pars <- c(200, 15)
mururoa.list$nugget   <- 25
mururoa.list$col.outside <- "lightskyblue"
mururoa.list$col.inside  <- "lightskyblue1"
mururoa.list$col.border  <- "darkgreen"
save(list = ls(), 
  file = "~/hea/rpanel/rpanel_1.0-5-not-yet-released/rpanel/R/sysdata.rda")

cl <- grep("green", colours(), value = TRUE)
cl <- grep("blue", colours(), value = TRUE)
ncl <- length(cl)
plot(c(0, ncl), c(0, ncl))
for (i in 1:length(cl))
   abline(h = i, col = cl[i], lwd = 3)

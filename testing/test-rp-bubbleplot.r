#      Animated bubble plot

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

rp.bubbleplot(log(gdp), log(co2.emissions), 1960:2007,
    size = population, 
    col = life.expectancy, interpolate = TRUE)








setwd("/Volumes/adrian/hea/rpanel/rpanel_1.1-0-not-yet-released")
# setwd("~/Desktop/India/R-functions")

source("fake-package.r")
# library(rpanel)

gdp   <- read.csv("gdp-pc.csv", row.names = 1)
co2   <- read.csv("co2-pc.csv", row.names = 1)
life  <- read.csv("life.csv",   row.names = 1)
pop   <- read.csv("pop.csv",    row.names = 1)
ccode <- gdp[,1]
gdp   <- gdp[-(1:28), -c(1, 50:52)]
co2   <- co2[-(1:28), -c(1, 50:52)]
life  <- life[-(1:28), -c(1, 50:52)]
pop   <- pop[-(1:28), -c(1, 50:52)]

ind <- grep("'", rownames(gdp))
for (i in ind) {
   rownames(gdp)[i]  <- sub("'", "", rownames(gdp)[i])
   rownames(co2)[i]  <- sub("'", "", rownames(co2)[i])
   rownames(pop)[i]  <- sub("'", "", rownames(pop)[i])
   rownames(life)[i] <- sub("'", "", rownames(life)[i])
}
# rownames(gdp)  <- NULL
# rownames(co2)  <- NULL
# rownames(life) <- NULL
# rownames(pop)  <- NULL
population      <- pop
life.expectancy <- life

save(gdp, co2, population, life.expectancy, file = "worldbank.rda")

# rp.bubbleplot(log(gdp), log(co2), 1960:2007)

# Look only at those who have data for the whole period?

# rp.plot3d(c(log(as.matrix(gdp))), c(log(as.matrix(co2))),
        # rep(1960:2007, each = nrow(gdp)))

# mx  <- max(gdp, na.rm = TRUE)
# wt  <- (gdp == mx)
# which(apply(wt, 1, any))
# which(apply(wt, 2, any))
# wmx <- which.max(gdp == mx)
# gdp[wmx %% nrow(gdp), wmx %/% nrow(gdp) + 1]

# ind <- which(apply(co2, 1, function(x) all(is.na(x))))
# rp.rmplot(log(co2[-ind, ]),
        # fac = rep(1, nrow(co2[-ind, ])), tim = 1960:2007)
# ind <- which(apply(gdp, 1, function(x) all(is.na(x))))
# rp.rmplot(log(gdp[-ind, ]),
        # fac = rep(1, nrow(gdp[-ind, ])), tim = 1960:2007)

# i <- 48
# plot(gdp[ , i], co2[ , i],
   # xlim = range(gdp, na.rm = TRUE), ylim = range(co2, na.rm = TRUE))


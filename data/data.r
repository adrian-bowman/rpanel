#     Data

setwd("/Volumes/adrian/hea/rpanel/rpanel_1.1-0-not-yet-released/rpanel/R")
# setwd("~/Desktop/rpanel_1.1-0-not-yet-released/rpanel/R")

load("../data/worldbank.rda")
save(co2, gdp, life.expectancy, population,
     file = "../data/worldbank.rda")

SO2 <- read.csv("../../SO2.dat")
names(SO2)
SO2 <- subset(SO2, !is.na(logSO2))
nrow(SO2)
save(SO2, file = "../data/SO2.rda")

Clyde <- read.table("../../clyde.dat")
names(Clyde) <- c("Station", "Day", "Month", "Year", "Depth", "Season", "DO")
Clyde     <- subset(Clyde, Depth == 0 & !is.na(DO))
mdays     <- cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))
Clyde$Doy <- Clyde$Day + mdays[Clyde$Month]
Clyde$id  <- factor(match(1000 * Clyde$Year + Clyde$Doy,
                   sort(unique(1000 * Clyde$Year + Clyde$Doy))))
Clyde     <- Clyde[ , c("Station", "Day", "Month", "Year",
                        "Doy", "id", "DO")]
save(Clyde, file = "../data/Clyde.rda")


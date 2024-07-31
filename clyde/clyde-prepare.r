#            Organise the data

# Remove high temperature and salinity values
ind <- (!is.na(d[ , 10])) & ((d[ , 10] > 50) | d[,11] > 32)
d   <- subset(d, !ind)

# Restrict depths to the common ones
d$Depth[!(d$Depth %in% c(0, 2, 3, 6, 7.5))] <- NA
tbl       <- table(d$DateGMT, factor(d$Depth))
ind       <- (apply(tbl, 1, sum) == 0)
dates     <- rownames(tbl)[ind]
ind       <- (as.character(d$DateGMT) %in% dates)
d         <- d[!ind, ]
ind       <- is.na(d$DateGMT)
d         <- d[!ind, ]
ind       <- is.na(d$Depth)
d         <- d[!ind, ]
d$DateGMT <- factor(as.character(d$DateGMT))
table(d$DateGMT, factor(d$Depth))
d <- subset(d, Depth %in% c(0, 2, 3, 6, 7.5))
d$Depth <- factor(d$Depth)

d$site    <- factor(d$DistanceFromWeir)
d$Station <- d$DistanceFromWeir


# Identify year and doy
d$year  <- as.numeric(matrix(unlist(strsplit(as.character(d$DateGMT), "-")),
                            ncol = 3, byrow = TRUE)[ , 3])
d$month <- matrix(unlist(strsplit(as.character(d$DateGMT), "-")),
                            ncol = 3, byrow = TRUE)[ , 2]
d$day   <- as.numeric(matrix(unlist(strsplit(as.character(d$DateGMT), "-")),
                            ncol = 3, byrow = TRUE)[ , 1])
d$month <- match(d$month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                          "Sep", "Oct", "Nov", "Dec"))
ndays <- cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))
d$doy <- ndays[d$month] + d$day
d$survey <- factor(paste(d$doy, d$year, sep = ""))
d$year <- d$year + d$doy / 366
d$do     <- as.numeric(d$DO > 5)


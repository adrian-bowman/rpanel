#     Two-way anova

if (reinstall) devtools::install("rpanel")
library(rpanel)

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Show datasets
datasets <- rp.datalink()

# Set the local directory
rp.datalink("~/Desktop/temp")
rp.datalink("~/Desktop/temp", "set local directory")
datasets <- rp.datalink()

# Download a single file
temp <- rp.datalink("covid19_tracking_UK")
rp.datalink("DO_Clyde", "download")
rp.datalink("all", "download")

# Code for compiling the book
devtools::install("rpanel")
library(rpanel)
rp.datalink("~/iCloud/teaching/book/data", "set local directory")
rp.datalink("paste", "download")

rp.datalink("~/Desktop/temp", "set local directory")
rp.datalink("covid19_tracking_UK", "download")
rp.datalink("covid19_tracking_UK")
rp.datalink(NULL, "set local directory")
rp.datalink("covid19_tracking_UK")

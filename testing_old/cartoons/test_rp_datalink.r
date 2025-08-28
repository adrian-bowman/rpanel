#     Tests for rp.datalink

if (reinstall) devtools::install("rpanel")
library(rpanel)

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Show datasets
datasets <- rp.datalink()

# Set the local directory
rp.datalink("~/Desktop/temp", "set local directory")
datasets <- rp.datalink()

# Download a single file
temp <- rp.datalink("covid19_tracking_UK")
rp.datalink("DO_Clyde", "download")
# rp.datalink("all", "download")

# Download a zipped folder
temp <- rp.datalink("children_services")
rp.datalink("DO_Clyde", "download")

# Code for compiling the book
rp.datalink("~/iCloud/teaching/book/data", "set local directory")
# rp.datalink("children_services", "download")

library(tidyverse)
path <- rp.datalink("children_services")

rp.datalink("covid19_tracking_UK", "download")
rp.datalink("covid19_tracking_UK")
rp.datalink(NULL, "set local directory")
rp.datalink("covid19_tracking_UK")


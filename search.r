#     Search for text in all files

target    <- "lattice"
diry      <- "."
diry      <- "tests"
diry      <- 'man'
diry      <- "R"
recursive <- TRUE

files <- list.files(diry, full.names = TRUE, recursive = recursive)
ind   <- c(grep(".rda", files), grep(".gif", files))
if (length(ind) > 0) files <- files[-ind]

for (ifl in files) {
   # cat(ifl, "\n")
   file <- readLines(ifl)
   grp <- grep(target, file, fixed = TRUE)
   if (length(grp) > 0) {
      cat(ifl, "\n")
      for (jfl in grp) cat(jfl, file[jfl], "\n")
   }
}

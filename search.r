#     Search for text in all files

target    <- "doughnut"
diry      <- "."
diry      <- "testing"
diry      <- "R"
recursive <- TRUE

files <- list.files(diry, full.names = TRUE, recursive = recursive)
ind   <- c(grep(".rda", files), grep(".gif", files))
files <- files[-ind]

for (ifl in files) {
   # cat(ifl, "\n")
   file <- readLines(ifl)
   grp <- grep(target, file, fixed = TRUE)
   if (length(grp) > 0) {
      cat(ifl, "\n")
      for (jfl in grp) cat(jfl, file[jfl], "\n")
   }
}

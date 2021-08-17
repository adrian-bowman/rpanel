#     Search for text in all files

target    <- "combo"
diry      <- "."
diry      <- "~/research/rpanel/testing"
diry      <- "~/research/rpanel/rpanel_without_bwidget"
recursive <- FALSE
recursive <- TRUE

files <- list.files(diry, full.names = TRUE)
files <- list.files(diry, full.names = TRUE, recursive = recursive)
ind   <- grep(".rda", files)
files <- files[-ind]

for (ifl in files) {
   # cat(ifl, "\n")
   file <- readLines(ifl)
   grp <- grep(target, file)
   if (length(grp) > 0) {
      cat(ifl, "\n")
      for (jfl in grp) cat(jfl, file[jfl], "\n")
   }
}

#     Reconfigure the Hyde data

txt   <- readLines("data/Hyde_genealogy/hyde_original.txt")
out   <- txt[1]
i.out <- 1
for (i in 2:length(txt)) {
  ln <- unlist(strsplit(txt[i], " "))
  zeros <- which(nchar(ln) == 0)
  if (length(zeros) > 0) ln <- ln[-zeros]
  lngth <- length(ln)
  # cat(i, lngth, "\n")
  if (lngth  > 3) stop("line", i, "is too long.")
  if (lngth == 0) stop("line", i, "is empty.")
  if (lngth == 1)
    father <- ln
  else {
    i.out <- i.out + 1
    if (lngth == 3) out[i.out] <- paste(ln, collapse = " ")
    if (lngth == 2) out[i.out] <- paste(father, ln[1], ln[2])
  }
}
writeLines(out, "data/Hyde_genealogy/hyde.txt")

Hyde <- read.table('data/Hyde_genealogy/hyde.txt', header = TRUE)
save(Hyde, file = 'rpanel/data/Hyde.rda')


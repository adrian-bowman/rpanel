#     Invoke all the tests

test.prompt <- FALSE
reinstall   <- FALSE

devtools::install("rpanel")
library(rpanel)

test_label <- function(label, test.prompt) {
  cat("\n**", label, "...")
  if (test.prompt) readline(prompt = "   Press [enter] to continue ...") else cat("\n\n")
}

# Exclusions
fls <- list.files("testing/cartoons", full.names = TRUE)
fls <- fls[-match("testing/cartoons/test_all_cartoons.R", fls)]
fls <- fls[-match("testing/cartoons/test_rp_datalink.r", fls)]
fls <- fls[-match("testing/cartoons/test-rp-plot4d.r", fls)]
fls <- fls[-match("testing/cartoons/test-rp-plot4d-clyde.r", fls)]
for (fl in fls) {
   cat("\n******", fl, "******\n\n")
   source(fl)
}
cat("\nCompleted.\n")

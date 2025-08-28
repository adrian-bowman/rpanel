#     Invoke all the tests

test.prompt <- FALSE
reinstall   <- FALSE

library(rpanel)
devtools::install("rpanel")

test_label <- function(label, test.prompt) {
  cat("\n**", label, "...")
  if (test.prompt) readline(prompt = "   Press [enter] to continue ...") else cat("\n\n")
}

# Exclusions
fls     <- list.files("testing/tools", full.names = TRUE)
exclude <- c("testing/tools/test_all_tools.R", "testing/tools/test-meermeyer.r",
             "testing/tools/compressed.gif",   "testing/tools/test-block.r",
             "testing/tools/test-combo.r",     "testing/tools/test-notebook.r")
fls     <- fls[-match(exclude, fls)]
for (fl in fls) {
   cat("\n******", fl, "******\n\n")
   source(fl)
}
cat("\nCompleted.\n")

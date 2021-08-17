#     Test code for rp.block

devtools::install("rpanel")

fn <- function(panel) {
  cat("Hello\n")
  panel
}
panel <- rp.control()
rp.button(panel, action = fn, title = "Hello")
rp.block(panel)


boxp.sim <- function(panel) {
  boxplot(rnorm(50))
  panel
}
panel <- rp.control()
rp.button(panel, action = boxp.sim, title = "Boxplot")
rp.block(panel)

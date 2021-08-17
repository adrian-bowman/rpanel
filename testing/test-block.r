#     Test code for rp.block

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

fn <- function(panel) {
  cat("hello\n")
  panel
}
# Create an rpanel and add the button "Simulate" to it.
panel <- rp.control()
rp.button(panel, action = fn, title = "Simulate")
rp.block(panel)


boxp.sim <- function(panel) {
  boxplot(rnorm(50))
  panel
}
panel <- rp.control()
rp.button(panel, action = boxp.sim, title = "Test")
rp.block(panel)

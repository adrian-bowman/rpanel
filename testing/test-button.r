#     Test code for rp.combo

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

boxp.sim <- function(panel) {
  boxplot(rnorm(50))
  panel
  }
panel <- rp.control()
rp.button(panel, action = boxp.sim, title = "Simulate")

# From rp.block help file.
boxp.sim <- function(panel) {
  boxplot(rnorm(50))
  panel
}
panel <- rp.control(panelname="Samplepanel")
rp.button(panel, action = boxp.sim, title = "Simulate")
rp.button(panel, action = I, title = "Quit", quitbutton=TRUE)

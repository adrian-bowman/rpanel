#     Test code for rp.combo

library(rpanel)

detach(package:rpanel)
unloadNamespace("rpanel")

install.packages("~/research/rpanel/rpanel/", repos = NULL, type = "source")
library(rpanel)

action <- function(panel) {
   print(panel$option)
   panel
}
panel <- rp.control()
rp.combo(panel, option, "Pick an option:",
         c("Option1", "Option2", "Other options"), action = action)

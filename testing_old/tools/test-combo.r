#     Test code for rp.combo

library(rpanel)
if (reinstall) devtools::install("rpanel")

action <- function(panel) {
   print(panel$option)
   panel
}
panel <- rp.control()
rp.combo(panel, option, "Pick an option:",
         c("Option1", "Option2", "Other options"), action = action)

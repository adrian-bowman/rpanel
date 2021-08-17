#     Code to test rp.ancova

detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

data(gullweight)
attach(gullweight)

rp.ancova(hab, weight, month)
rp.ancova(hab, weight, month, model = c(TRUE, TRUE, TRUE, FALSE), model0 = c(TRUE, FALSE, FALSE, TRUE))
rp.ancova(hab, weight, month, style = "old")

fn <- function(panel) {
   print(panel$adrian)
   panel
}
panel <- rp.control()
rp.checkbox(panel, adrian, fn, name = "checkbox")
rp.checkbox.change(panel, "checkbox", "adrian", TRUE, action = fn)
rp.checkbox.change(panel, "checkbox", "adrian", FALSE, action = fn)

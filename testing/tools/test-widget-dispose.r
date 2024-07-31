library(rpanel)
if (reinstall) devtools::install("rpanel")

# action <- function(panel) {
#    print(panel$b1)
#    print(panel$b2)
#    print(panel$b3)
#    panel
# }   
# 
# p1 <- rp.control(b1 = 8, b2 = 0, b3 = 0)
# rp.button(p1, action, "press me", name = "b1")
# rp.button(p1, action, "press me", name = "b2")
# rp.button(p1, action, "press me", name = "b3")
# rp.radiogroup(p1, dummy, as.character(1:4), name = "rg")
# 
# rp.widget.dispose(p1, "b1")
# rp.widget.dispose(p1, "rg")
# 
# ls(rp.env())
# get("window1.b1", envir = rp.env())
# 

library(rpanel)
if (reinstall) devtools::install("rpanel")

# action <- function(panel) {
#    print(names(panel))
#    # print(as.numeric(tkwinfo("height", panel$.handle)))
#    rp.var.put(NULL, "h", panel$h)
#    panel
# }
# panel <- rp.control(h = 0)
# rp.slider(panel, h, 0, 1, action)
# rp.block(panel)
# 
# rp.var.get(NULL, "h")

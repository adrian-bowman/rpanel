
library(rpanel)
if (reinstall) devtools::install("rpanel")

panel <- rp.control(size = c(310, 650))
rp.slider(panel, slider, 0, 1, title = "Something", showvalue = TRUE, pos= c(10, 10, 290, 60))
rp.checkbox(panel, varcheck, I, pos = c(10, 70, 290, 100))
rp.button(panel, I, pos = c(10, 90, 200, 60))
rp.slider(panel, sliders, rep(0, 3), rep(1, 3), showvalue = TRUE, pos = c(20, 150, 250, 200))
rp.checkbox(panel, varchecks, I, labels = as.character(1:3), pos = c(20, 360, 250, 150))
rp.radiogroup(panel, radiog, 1:4, pos = c(20, 450, 250, 150))
rp.doublebutton(panel, db, 1, pos = c(20, 550, 250, 150))

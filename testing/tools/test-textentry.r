#     Test rp.textentry

library(rpanel)
if (reinstall) devtools::install("rpanel")

monitor <- function(panel) {
	print(panel$h)
	panel
}
panel <- rp.control(h = "F")
rp.textentry(panel, h, monitor)

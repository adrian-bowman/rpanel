#     Test rp.textentry

library(rpanel)
panel <- rp.control()
rp.textentry(panel, )

monitor <- function(panel) {
	print(panel$h)
	panel
}

#     Test sliders

library(rpanel)
panel <- rp.control()
rp.slider(panel, h, 0, 1, I, row = 0, col = 0, sticky = "news")
rp.button(panel, I, "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh", row = 1, col = 0)
rp.slider(panel, hh, 0, 1, I, row = 2, col = 0)

printer <- function(panel) {
	print(panel$h[1])
	print(panel$h[2])
	print(panel$h[3])
	panel
}
panel <- rp.control(x = rnorm(50), h = c(1, 2, 3))
rp.slider(panel, h, c(0.5, 0.5, 0.5), c(5, 5, 5),
    action = printer, labels = as.character(4:6))

panel <- rp.control(x = rnorm(50))
rp.slider(panel, h, c(0.5, 0.5, 0.5), c(5, 5, 5),
    # names = c("x","y","z"), 
    # title = "adrian",
    # labels=c('h','h1','h2'),
    action = printer)

panel <- rp.control(x = rnorm(50), h=c(1,2,3))
rp.slider(panel, h, c(0.5,0.5,0.5), c(5,5,5),
    log = c(TRUE,TRUE,TRUE), action = printer,
    label=c('h','h1','h2'), horizontal=FALSE)

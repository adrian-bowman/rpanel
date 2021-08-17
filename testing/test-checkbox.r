#     Test code for rp.checkbox

library(rpanel)

printer <- function(panel) {
	print(panel$h)
	panel
}
panel <- rp.control(x = rnorm(50),
                    h = c(x=TRUE, y=FALSE, z= TRUE))
rp.checkbox(panel, h,
             labels = c("a", "b", "c"),
             # names = c("a1", "a2", "a3"),
             # names = c("a1"),
             title = "adrian",
             action = printer)


oncheck <- function(panel) {
    if (panel$cb) cat("Box is checked\n")
      else cat("Not checked!\n")
      panel
    }
panel <- rp.control()
rp.checkbox(panel, cb, initval = TRUE, action = oncheck)

oncheck <- function(panel) {
 print(panel$cb)
 if (panel$cb[1]) cat("Box 1 is checked\n")
 else cat("1 Not checked!\n")
 if (panel$cb[2]) cat("Box 2 is checked\n")
 else cat("2 Not checked!\n")
 if (panel$cb[3]) cat("Box 3 is checked\n")
 else cat("3 Not checked!\n")
 panel
}
panel <- rp.control()
rp.checkbox(panel, cb, initval = c(TRUE,FALSE,TRUE), labels = c("One","Two","Three"), action = oncheck)

oncheck <- function(panel) {
 print(panel$cb)
 panel
}
panel <- rp.control()
rp.textentry(panel, cb, labels = c("One","Two","Three"),
   names = c("John","Adrian","Jim"), action = oncheck)

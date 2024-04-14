#------------------------------------------------------
#       rp.tkrplot
#------------------------------------------------------

library(rpanel)
if (reinstall) devtools::install("rpanel")

plotdata <- function(panel) {
   plot(panel$x, panel$y)
   print(panel$y)
   abline(lm(panel$y ~ panel$x))
   panel
   }
plotnewdata <- function(panel, x, y) {
	panel$x <- c(x, panel$x)
	panel$y <- c(y, panel$y)
   rp.do(panel, redraw)
	panel$x <- panel$x[-1]
	panel$y <- panel$y[-1]
   panel
   }
plotolddata <- function(panel, x, y) {
   rp.do(panel, redraw)
   panel
   }
redraw <- function(panel) {
   rp.tkrreplot(panel, tkrp)
   }
   
x <- runif(50, 0, 1000)
y <- x + rnorm(50, sd = 50)
rpplot <- rp.control(title = "Demonstration of click and drag", x = x, y = y,
                     pos = "top")
rp.tkrplot(rpplot, tkrp, plotdata, 
      action = plotnewdata, mousedragfun = plotnewdata, mouseupfun = plotolddata)




source("/Volumes/adrian/notes/computing/R/tips.txt")
library(tkrplot)
tt <- tktoplevel()
bb<-1
img <-tkrplot(tt, function() plot(1:20,(1:20)^bb))
f<-function(...) {
    b <- as.numeric(tclvalue("bb"))
    if (b != bb) {
        bb <<- b
        tkrreplot(img)
    }
}
s <- tkscale(tt, command=f, from=0.05, to=2.00, variable="bb",
             showvalue=FALSE, resolution=0.05, orient="horiz")
tkpack(img,s)

tkrnew <- function (parent, fun, hscale = 1, vscale = 1) 
{
    print("here 1")
	flush.console()
    image <- paste("Rplot", .make.tkindex(), sep = "")
    print("here 2")
	flush.console()
    .my.tkdev(hscale, vscale)
    print("here 3")
	flush.console()
    try(fun())
	flush.console()
    print("here 4")
	flush.console()
    .Tcl(paste("image create Rplot", image))
    print("here 5")
	flush.console()
    lab <- tklabel(parent, image = image)
    print("here 6")
	flush.console()
    tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete", 
        image)))
    print("here 7")
	flush.console()
    lab$image <- image
    lab$fun <- fun
    lab$hscale <- hscale
    lab$vscale <- vscale
    lab
}

hscale <- 1
vscale <- 1
fun    <- function() plot(1:10)
library(tkrplot)
tt <- tktoplevel()
parent <- tt
tkrnew(parent, fun, hscale, vscale)


detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

install.packages("~/research/rpanel/rpanel/", repos = NULL, type = "source")
library(rpanel)

action.button <- function(panel) {
   print("Button pressed!")
   panel
}
action.plot <- function(panel) {
   plot(1:10)
   panel
}
action.rp <- function(x, y, add = FALSE, panel = NULL, 
               panel.parentname = NULL) {
   act.button <- function(panel) {
      print("Inside action.rp")
      panel
   }
   act.plot <- function(panel) {
      plot(1:10)
      panel
   }
   if (!add)
      panel <- rp.control(x = x, y = y)
   rp.button(panel, act.button, parentname = panel.parentname)
   rp.tkrplot(panel, "plot", act.plot)
   panel
   }
panel <- rp.control()
rp.notebook(panel, c("One", "Two"), name = "n1")
rp.button(panel, action.button, parentname = "One")
rp.tkrplot(panel, plot, action.plot, parentname = "One", pos = "right", vscale = 0.7)
rp.notebook.raise(panel, "n1", "One")


x <- rnorm(50)
y <- rnorm(50)

# qplot(x, y) + geom_line(col = 3)

par(mar = c(3, 3, 4, 1) + 0.1, mgp = c(1.5, 0.3, 0), tcl = -0.3,
    cex.axis = 0.7)
plot(x, y, type = "n", axes = FALSE)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
    col = "grey90", border = "grey90")
grid(col = "white", lty = 1)
axis(1, col = "grey90", col.ticks = "grey70")
axis(2, col = "grey90", col.ticks = "grey70")
points(x, y, pch = 16, cex = 0.7)
points(x, y, pch =  1, cex = 0.7)

panel1 <- rp.control(x = x, y = y, xlab = "x", ylab = "y")
rp.notebook(panel1, c("One", "Two"), name = "n1", height = 500, width = 700)
rp.regression(x, y, hostpanel = panel1, hostpanel.parentname = "One")
rp.notebook.raise(panel1, "n1", "One")


action.rp(x, y, add = TRUE, panel = panel1,
      panel.parentname = "One")


panel <- rp.control(title = "Notebook example")
rp.notebook(panel, c("One", "Two"), width=600, height=400)

rp.notebook(panel, c("File", "Edit"), width=600, height=400,
            pos=list(row=0, column=0), background="lightgrey", 
            font="Arial", name="n1")
rp.notebook.raise(panel, "n1", "Edit")
rp.button(panel, function(panel) { 
   rp.messagebox("Button pressed!")
   panel},
   "Test this", parentname="Edit")
# A second tabbed notebook can be added to the same window.
rp.notebook(panel, c("A tab 1", "A tab 2"),
   width=200, height=200, pos=list(row=1, column=1), 
   background="Navy", foreground="White")
# A tabbed notebook can be placed inside a tabbed notebook.
rp.notebook(panel, c("Tab within tab", "Another tab"),
   width=200, height=100, parentname="File", name="n3")
rp.notebook.raise(panel, "n1", "File")
rp.notebook.raise(panel, "n3", "Another tab")

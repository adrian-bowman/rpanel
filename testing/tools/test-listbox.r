library(rpanel)
if (reinstall) devtools::install("rpanel")

draw<-function(panel) {
   plot(rnorm(100))
   panel
}
Cont.Names<-c("Benzene", "Toluene", "Ethylene")
replot.SmoothPlot <- function(panel) {
   rp.tkrreplot(panel, SmoothPlot)
   return(panel)
}
GWSDATpnl <- rp.control(panelname="GWSDATpnl")
rp.listbox(GWSDATpnl, Cont.rg,
    labels = sort(as.character(Cont.Names)),
    vals = sort(as.character(Cont.Names)),
    action = replot.SmoothPlot, title = "Solute")
rp.tkrplot(GWSDATpnl, SmoothPlot, draw)

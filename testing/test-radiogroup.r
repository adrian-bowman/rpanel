detach(package:rpanel)
unloadNamespace("rpanel")
library(rpanel)

data.plotfn <- function(panel) {
   print(panel$plot.type)
   if (panel$plot.type == "1") 
      hist(panel$x)
   else 
      if (panel$plot.type == "2")
         boxplot(panel$x)
      else 
         plot(density(panel$x))
   panel
}
panel <- rp.control(x = rnorm(50))
rp.radiogroup(panel, plot.type, as.character(1:3),
       c("histogram", "boxplot", "density estimate"), 
       action = data.plotfn, title = "Plot type")     

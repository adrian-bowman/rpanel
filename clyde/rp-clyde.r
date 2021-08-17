rp.clyde <- function(width = 800, height = 600) {
	
   clyde <- read.table("~/Desktop/rpanel_1.1-0-not-yet-released/clyde.dat")
   names(clyde) <- c("Station", "Day", "Month", "Year", "Depth", "Season", "DO")
   attach(clyde)
   Treatment <- (Year > 85)
   mdays     <- cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))
   Days      <- Day + mdays[Month]
   ind       <- (Depth==0 & Year > 78 & Year < 92 & Year != 85 & !is.na(DO))

   panel <- rp.control(DO = DO, Treatment = Treatment, Days = Days, ind = ind)
   
   rp.notebook(panel, c("Background", "Exploration", "Model"),
               width = width, height = height, name="notebook")

   rp.image(panel, "~/Desktop/armadillo.gif", armadillo, parent = "Background")

   rp.notebook.raise(panel, "notebook", "Background")

   invisible()
}

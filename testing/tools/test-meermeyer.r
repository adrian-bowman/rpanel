# Author: Dr. Martin Meermeyer, Summerterm 2012

# Clear R-Workspace
rm(list=ls(all=TRUE))
# Determine location of R-script and set as working directory
#path.act <- dirname(sys.frame(1)$ofile)
#setwd(path.act)
# Close all graphic devices
graphics.off()

# Load (or install) package
if(!require(rpanel))install.packages("rpanel")


x.1 <- 10
x.2 <- 20
mitte.x <- (x.2 - x.1)/2

y.1 <- 0.4
y.2 <- 0.7


cex.beschr  <- 1
cex.label   <- 1.5
liniendicke <- 2


aktion <- function(panel)
    {
    # Parameter aus Formular auslesen, 
    # sonst Fehlermeldung bei erstem Aufruf
    x  <- panel$x
    
    proz.interpol <- (x - x.1)/(x.2-x.1)
    
    proz.interpol.ausg <- paste("(",x,"-",x.1,")/(",x.2,"-",x.1,") =",round(proz.interpol, digits=4), sep="")
    
    x.interpol <- y.1 + (y.2-y.1)*proz.interpol
    
    interpol.ausg <- paste("F(x) = ",y.1," + ",round(proz.interpol*100, digits=2),"% x (",y.2," - ",y.1,") =", round(x.interpol, digits=2 ))
    
    plot(1,1, xlim=c(x.1,x.2), ylim=c(y.1, y.2), type="n")
    segments(x.1, y.1, x1 = x.2, y1 = y.2, lwd=liniendicke )

    segments((x.1-5), x.interpol, x1 = x, y1 = x.interpol, col=2, lwd=liniendicke )
    segments(x, x.interpol, x1 = x, y1 = (y.1-1), col=4, lwd=liniendicke )
    
    mtext(proz.interpol.ausg ,3,2)
    mtext(interpol.ausg ,3,1)
    
    panel
    }

rpplot <- rp.control(title="Lineare Interpolation", size = c(310, 80))

rp.slider(rpplot, x, from=x.1, to=x.2, initval=mitte.x, showvalue =TRUE, title="x (Quantil)", pos= c(10,10, 290,60), action = aktion)

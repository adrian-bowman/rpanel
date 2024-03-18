#     A new go at graphics for a contingency table

rp.contingency <- function(x, display = "comparison", show = "proportions",
                           scales = FALSE) {
   
   if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("the ggplot2 package is not available.")
   
   if (!all(display %in% c("comparison", "mosaic")))
      stop("display setting not recognised.")
   
   spc     <- 0.05
   nrw     <- nrow(x)
   ncl     <- ncol(x)
   colsums <- colSums(x)
   xpos    <- cumsum(colsums / sum(colsums))
   p       <- sweep(x, 2, colsums, "/")
   pcum    <- apply(p, 2, cumsum)
   rnms    <- factor(rep(rownames(p), ncl), levels = rownames(p))
   cnms    <- factor(rep(colnames(p), each = nrw), levels = colnames(p))
   lbl     <- if (show == "proportions") c(signif(p, 2)) else c(x)
   xbrks   <- (c(0, xpos[-ncl]) + xpos) / 2
   ybrks   <- (c(0, pcum[-nrw, 1]) + pcum[ , 1]) / 2
   xmn     <- rep(c(0, xpos[-ncl]), each = nrw)
   xmx     <- rep(xpos, each = nrw)
   ymn     <- c(rbind(rep(0, ncl), pcum[-nrw, ]))
   ymx     <- c(pcum)
   d       <- data.frame(p = c(p), x = c(x), xmn, xmx, ymn, ymx)

   if ("mosaic" %in% display) {
      plt <- ggplot2::ggplot(d) +
         ggplot2::geom_rect(ggplot2::aes(xmin = xmn, xmax = xmx,
                                         ymin = 1 - ymn, ymax = 1 - ymx),
                            fill = "lightgrey", col = "white") +
         ggplot2::geom_text(ggplot2::aes(x = (xmn + xmx) / 2,
                                         y = 1 - (ymn + ymx) / 2, label = lbl)) +
         # ggplot2::theme_minimal() +
         ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank()) +
                        # axis.text.y  = ggplot2::element_text(angle = 90,
                                                             # vjust = 0.5, hjust = 1)) +
         ggplot2::scale_x_continuous(breaks = xbrks, labels = colnames(x),
                                     expand = rep(0, 2), position = "top") +
         ggplot2::scale_y_continuous(breaks = 1 - ybrks, labels = rownames(x),
                                     expand = rep(0, 2))
   }
   
   if ("comparison" %in% display) {  
      ymn <- rep(0, nrw * ncl)
      ymx <- c(p)
      d   <- data.frame(p = c(p), x = c(x), xmn, xmx, ymn, ymx, rnms, cnms)
      plt <- ggplot2::ggplot(d) + 
         ggplot2::geom_rect(ggplot2::aes(xmin = xmn, xmax = xmx,
                                         ymin = ymn, ymax = ymx),
                            fill = "lightgrey", col = "white") +
         ggplot2::geom_text(ggplot2::aes(x = (xmn + xmx) / 2,
                                         y = (ymn + ymx) / 2, label = lbl)) +
         # theme_void() +
         ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                        # panel.spacing = unit(1, "lines"),
                        strip.background = ggplot2::element_rect(fill="white"),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank()) +
         ggplot2::scale_x_continuous(expand = rep(0.005, 2)) +
         ggplot2::scale_y_continuous(expand = rep(0.005, 2),
                                     position = "right") +
         ggplot2::labs(x = NULL, y = NULL) +
         ggplot2::facet_grid(rnms ~ cnms, scales = "free", space = "free",
                             switch = "y")
      # scales was once an argument - not needed
      if (!scales)
         plt <- plt + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                     axis.ticks.y = ggplot2::element_blank())
   }
   
   print(plt)
   
   return(invisible(plt))
}

# Density strip

# Tom W and others - something like the original attachment can be done as follows: note the use of a "ypos" variable to set the y position of each strip, and the "height" aesthetic for the strip thickness. 
# 
# x <- seq(-1, 5, by=0.01) # points to evaluate the density at
# df2 <- data.frame( x = rep(x, 3),
#                    dens = c(dnorm(x, 1.25, .35),  dnorm(x, -.5, .15),  dnorm(x,  2, 1)),
#                    ypos = rep(c(1, 4, 5), each=length(x)))
# 
# ggplot(df2, aes(x = x, y = ypos)) + theme_bw() +
#    geom_tile(aes(fill =  dens), height=0.2) +
#    scale_fill_continuous(low="white", high="black")
# 
# or if you want the maximum density to be black in every strip (see the paper for discussion of this):
#    
#    df2$maxdens <- rep(with(df2, tapply(dens, factor(ypos, levels=unique(ypos)), max)), each=length(x))
# df2$dens.unscaled <- df2$dens / df2$maxdens
# 
# ggplot(df2, aes(x = x, y = ypos)) + theme_bw() +
#    geom_tile(aes(fill =  dens.unscaled), height=0.2) +
#    scale_fill_continuous(low="white", high="black")

# Old code for a 'comparison'
# if ("comparison" %in% display) {
#    pmx   <- apply(p, 1, max)
#    base  <- c(0, cumsum(pmx[-nrw]))
#    ymn   <- rep(base, ncl)
#    ymx   <- c(sweep(p, 1, base, "+"))
#    d     <- data.frame(p = c(p), x = c(x), xmn, xmx, ymn, ymx)
#    ybrks <- base + pmx / 2
#    plt <- ggplot2::ggplot(d) +
#       ggplot2::geom_rect(ggplot2::aes(xmin = xmn, xmax = xmx,
#                                       ymin = ymn, ymax = ymx),
#                          fill = "lightgrey", col = "white") +
#       ggplot2::geom_text(ggplot2::aes(x = (xmn + xmx) / 2,
#                                       y = (ymn + ymx) / 2, label = lbl)) +
#       ggplot2::theme_minimal() +
#       ggplot2::theme(axis.title.x = ggplot2::element_blank(),
#                      axis.title.y = ggplot2::element_blank(),
#                      panel.grid.major = ggplot2::element_blank(),
#                      panel.grid.minor = ggplot2::element_blank()) +
#       ggplot2::scale_x_continuous(breaks = xbrks, labels = colnames(x),
#                                   expand = rep(0, 2)) +
#       ggplot2::scale_y_continuous(breaks = ybrks, labels = rownames(x),
#                                   expand = rep(0, 2))
# }


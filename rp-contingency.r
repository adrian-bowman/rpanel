rp.contingency <- function(x, names = list(rownames(x), colnames(x)),
                       scale = "counts", display = "table", panel = TRUE, panel.plot = TRUE,
                       reference.model = FALSE, structure = "unassigned",
                       superimpose = FALSE, variation = FALSE, margins = TRUE,
                       col.model = "green", col.data = "grey60", col.background = "grey95",
                       col.lines = "grey90",
                       hscale = 1, vscale = hscale * nrow(x) / ncol(x)) {

   library(grid)
   
   if (panel) final <- FALSE
   final <- FALSE
   if (is.null(names[[1]])) names[[1]] <- paste("row",    1:nrow(x))
   if (is.null(names[[2]])) names[[2]] <- paste("column", 1:nrow(x))

   rp.contingency.draw <- function(panel) {
   
      if (panel$final) {
         rp.contingency.final(panel)
         return(panel)
         }

   	  with(panel, {

         rtots  <- apply(x, 1, sum)
         ctots  <- apply(x, 2, sum)
         rprops <- sweep(x, 1, rtots, "/")
         cprops <- sweep(x, 2, ctots, "/")
         tprops <- x / sum(x)

         dsply <- display
         if (structure == "unassigned") {
         	dsply           <- "table"
         	reference.model <- FALSE
         	scale           <- "counts"
         }
         if (structure == "row populations") {
            props  <- rprops
            props0 <- ctots / sum(ctots)
            rsep   <- 1
            csep   <- 0.15
         }
         else if (structure == "column populations") {
            props  <- cprops
            props0 <- rtots / sum(rtots)
            rsep   <- 0.15
            csep   <- 1
         }
         else if (structure == "table population") {
            props  <- tprops
            props0 <- outer(rprops, cprops)
            rsep   <- 0.15
            csep   <- 0.15
         }
         else {
         	rsep   <- 0
         	csep   <- 0
         }

         grid.newpage()
         nr   <- nrow(x)
         nc   <- ncol(x)
         if (margins) 
            g <- grid.layout(2 * nr + 5, 2 * nc + 5,
                       widths  = unit(c(1, 1, 0.5, rep(c(1, csep), nc - 1), 1, 1.5, 1, 1),
                          c(rep("lines", 3), rep(c("null", "lines"), nc + 1))),
                       heights = unit(c(1, 1, 0.5, rep(c(1, rsep), nr - 1), 1, 1.5, 1, 1),
                          c(rep("lines", 3), rep(c("null", "lines"), nr + 1))))
         else
            g <- grid.layout(2 * nr + 4, 2 * nc + 4,
                       widths  = unit(c(1, 1, 0.5, rep(c(1, csep), nc - 1), 1, 1.5, 1),
                          c(rep("lines", 3), rep(c("null", "lines"), nc), "lines")),
                       heights = unit(c(1, 1, 0.5, rep(c(1, rsep), nr - 1), 1, 1.5, 1),
                          c(rep("lines", 3), rep(c("null", "lines"), nr), "lines")))
         # g <- grid.layout(2 * nr + 5, 2 * nc + 5,
         #               widths  = unit(c(1, 1, 0.5, rep(c(1, csep), nc - 1), 1, 1, 1, 1),
         #                  c(rep("lines", 3), rep(c("null", "lines"), nc), "null", "lines")),
         #               heights = unit(c(1, 1, 0.5, rep(c(1, rsep), nr - 1), 1, 1, 1, 1),
         #                  c(rep("lines", 3), rep(c("null", "lines"), nr), "null", "lines")))
         pushViewport(viewport(layout = g))
         
         for (i in 1:(nr + as.numeric(margins))) {
            for (j in 1:(nc + as.numeric(margins))) {
            	
         	   nr <- nrow(x)
         	   nc <- ncol(x)
               pushViewport(viewport(layout.pos.row = 2 * i + 2, 
                                     layout.pos.col = 2 * j + 2))

               if (reference.model | i <= nr & j <= nc |
                   (i == nr + 1 & j <= nc     & structure == "column populations") |
                   (i <= nr     & j == nc + 1 & structure == "row populations") |
                   (i == nr + 1 & j == nc + 1 & structure == "table population"))
                      grid.rect(gp = gpar(fill = col.background, col = col.background))

               if (scale == "proportions")
                  scl <- c(0, 0.25, 0.5, 0.75, 1)
               else
                  scl <- pretty(c(0, x), 3)
               nscl <- length(scl)

               if (i <= nr & j == nc + 1) {
                  if (structure == "row populations")
                        grid.text(as.character(sum(x[i, ])), gp = gpar(cex = 2, col = "darkgrey"))
                  if (structure %in% c("column populations", "table population") & reference.model) {
                     pr <- if (structure == "column populations") props0
                           else round(apply(x, 1, sum) / sum(x), 2)
                     if (dsply == "table")
                        grid.text(as.character(sum(x[i, ])), gp = gpar(cex = 2, col = col.model))
                     else if (dsply == "proportions")
                        grid.text(as.character(pr[i]), gp = gpar(cex = 2, col = col.model))
                     else if (dsply == "plots") {
                     	if (structure == "column populations") {
                     	   grid.segments(c(0.25, 0.5, 0.75), rep(0, 3), c(0.25, 0.5, 0.75), rep(1, 3),
                     	                    gp = gpar(col = col.lines))
                           grid.points(pr[i], 0.5, gp = gpar(col = "red"))
                           if (i > 1)
                              grid.lines(c((pr[i - 1] + pr[i]) / 2, pr[i]), c(1, 0.5),
                                            gp = gpar(col = "red"))
                           if (i < nr)
                              grid.lines(c(pr[i], (pr[i] + pr[i + 1]) / 2), c(0.5, 0),
                                            gp = gpar(col = "red"))
                     	}
                     	else {
                     	   grid.segments(rep(0, 3), c(0.25, 0.5, 0.75), rep(1, 3), c(0.25, 0.5, 0.75),
                     	                    gp = gpar(col = col.lines))
                     	   grid.points(0.5, pr[i], gp = gpar(col = "red"))
                     	}
                     }
                  }
               }
               
               else if (i == nr + 1 & j <= nc) {
                  if (structure == "column populations")
                     grid.text(as.character(sum(x[ ,j])), gp = gpar(cex = 2, col = "darkgrey"))
                  if (structure %in% c("row populations", "table population") & reference.model) {
                     pr <- if (structure == "row populations") round(props0, 2) 
                           else round(apply(x, 2, sum) / sum(x), 2)
                 	 if (dsply == "table") {
                 	 	if (scale == "counts")
                           grid.text(as.character(sum(x[ ,j])), gp = gpar(cex = 2, col = col.model))
                        else
                           grid.text(as.character(pr[j]), gp = gpar(cex = 2, col = col.model))
                 	 }
                     else if (dsply == "plots") {
                        grid.text(as.character(pr[j]), gp = gpar(cex = 2, col = col.model))
                     	# grid.segments(rep(0, 3), c(0.25, 0.5, 0.75), rep(1, 3), c(0.25, 0.5, 0.75),
                     	                  # gp = gpar(col = col.lines))
                 	 	# if (scale == "counts") {
                 	 	   # scl <- pretty(c(0, ctots), 3)
                           # grid.rect(0.5, 0, width = 0.2, height = ctots[j] / max(scl),
                              # just = c("centre", "bottom"),
                              # gp = gpar(fill = col.data, col = col.data))
                           # cumcol <- cumsum(rev(x[ ,j]))[-nrow(x)]
                           # grid.segments(rep(0.4, nrow(x) - 1), cumcol / max(scl),
                                         # rep(0.6, nrow(x) - 1), cumcol / max(scl),
                     	                  # gp = gpar(col = "white"))
                           # grid.lines(c(0.05, 0.05, 0.95, 0.95), c(0, rep(ctots[j] / max(scl), 2), 0),
                              # gp = gpar(col = "black", lwd = 2))
                 	 	# }
                        # else {
                           # strt <- 0.05
                           # for (k in 1:nrow(x)) {
                              # wdth <- 0.9 * rtots[k] / sum(rtots)
                              # grid.rect(strt, 0, width = wdth, height = props[k, j],
                                 # just = c("left", "bottom"),
                                 # gp = gpar(fill = col.model, col = col.background))
                              # strt <- strt + wdth
                           # }
                           # grid.lines(c(0.05, 0.05, 0.95, 0.95), c(0, rep(pr[j], 2), 0),
                              # gp = gpar(col = "black", lwd = 2))
                        # }
                     }
#                        grid.points(0.5, pr[j], gp = gpar(col = "red"))
#                        if (j > 1)
#                           grid.lines(c(0, 0.5), c((pr[j - 1] + pr[j]) / 2, pr[j]), 
#                                         gp = gpar(col = "red"))
#                        if (j < nc)
#                           grid.lines(c(0.5, 1), c(pr[j], (pr[j] + pr[j + 1]) / 2), 
#                                         gp = gpar(col = "red"))
                  }
               }
               
               else if (i == nr + 1 & j == nc + 1) {
               	  if (structure == "table population") 
                        grid.text(as.character(sum(x)), gp = gpar(cex = 2, col = "darkgrey"))
                  else if (reference.model)
                        grid.text(as.character(sum(x)), gp = gpar(cex = 2, col = "darkgrey"))
               }
               
               else if (dsply == "table" & scale == "counts") {
                  just <- if (reference.model & superimpose) "bottom" else "centre"
                  if (i <= nr) {
                     grid.text(as.character(x[i, j]), gp = gpar(cex = 2, col = "black"),
                              just = c("centre", just))
                     if (reference.model & superimpose)
                        grid.text(as.character(round(props0[j] * sum(x[i, ]), 1)), 
                              gp = gpar(cex = 2, col = col.model), just = c("centre", "top"))
                  }
                  else if (structure == "row populations")
                     grid.text(as.character(sum(x[, j])), gp = gpar(cex = 2, col = col.model))
               }
               
               else if (dsply == "table") {
                  a    <- if (scale == "counts") x else round(props, 2)
                  a0   <- if (scale == "counts") apply(x, 2, sum) else round(props0, 2)
                  just <- if (reference.model & superimpose) 
                                  "bottom" else "centre"
                  if (i <= nr) {
                     grid.text(as.character(a[i, j]), gp = gpar(cex = 2, col = "black"),
                              just = c("centre", just))
                     if (reference.model & superimpose)
                        grid.text(as.character(a0[j]), gp = gpar(cex = 2, col = col.model),
                              just = c("centre", "top"))
                  }
                  else if (reference.model)
                     grid.text(as.character(a0[j]), gp = gpar(cex = 2, col = col.model))
               }
               
               else if (dsply == "plots") {
                  if (structure %in% c("row populations", "table population")) {
                  	 grid.segments(rep(0, nscl - 2), scl[-c(1, nscl)] / max(scl), 
                  	               rep(1, nscl - 2), scl[-c(1, nscl)] / max(scl),
                   	               gp = gpar(col = col.lines))
                   	 est  <- if (scale == "counts") x[i, j] else props[i, j]
                   	 wdth <- if (scale == "counts")     0.2 else 0.8 * rtots[i] / sum(rtots)
                     grid.rect(0.5, 0, width = wdth, height = est / max(scl),
                        just = c("centre", "bottom"),
                        gp = gpar(fill = NULL, col = col.data))
                     if (reference.model & superimpose) {
                        ncells <- nrow(x) * ncol(x)
                        est0   <- if (scale == "counts") props0[j] * rtots[i]
                        else                   props0[j]
                        se.i   <- if (scale == "counts") sqrt(props0[j] * rtots[i] / (nr * nc))
                        else                   sqrt(props0[j] / (rtots[i] * nr * nc))
                        #                       Block
                        #                        grid.rect(0.5, 0, width = 0.8, height = props0[j],
                        #                           just = c("centre", "bottom"),
                        #                           gp = gpar(fill = col.model, col = col.model))
                        #                       Lines
                        #                        zgrid <- c(props0[j] - 2 * se.i, props0[j] + 2 * se.i)
                        #                        zgrid[zgrid < 0] <- 0
                        #                        zgrid[zgrid > 1] <- 1
                        #                        grid.segments(rep(0.1, 2), zgrid, rep(0.9, 2), zgrid, gp = gpar(lty = 2))
                        #                        Shading
                        if (variation) {
                           zgrid <- seq(est0 - 3 * se.i, est0 + 3 * se.i, length = 100)
                           zmax  <- if (scale == "counts") max(scl) else 1
                           zgrid <- zgrid[zgrid > 0 & zgrid < zmax]
                           lvl   <- as.numeric(substr(col.background, 5, 6))
                           shft  <- lvl - as.numeric(substr(col.model, 5, 6))
                           # grid.segments(0.05, zgrid / max(scl), 0.95, zgrid / max(scl),
                           # gp = gpar(col = paste("grey", 
                           # lvl - round(shft * exp(-0.5 * (est0 - zgrid)^2 / se.i^2))), sep = ""))
                           back  <- c(col2rgb(col.background) / 255)
                           front <- c(col2rgb(col.model) / 255)
                           wts   <- exp(-0.5 * (est0 - zgrid)^2 / se.i^2)
                           # clr   <- rbind((1 - wts) * back[1] + wts * front[1],
                           #                (1 - wts) * back[2] + wts * front[2],
                           #                (1 - wts) * back[3] + wts * front[3])
                           # clr   <- apply(clr, 2, function(x) rgb(x[1], x[2], x[3]))
                           clr   <- rgb(front[1], front[2], front[3], wts)
                           grid.segments(0.05, zgrid / max(scl), 0.95, zgrid / max(scl),
                                         gp = gpar(col = clr))
                        }
                        else
                           grid.lines(c(0.05, 0.05, 0.95, 0.95), c(0, rep(est0, 2), 0) / max(scl),
                                      gp = gpar(col = col.model, lwd = 2))
                     }
                     # grid.lines(c(0.5 - wdth / 2, 0.5 + wdth / 2), rep(props[i, j], 2),
                        # gp = gpar(col = "blue"))
#                     grid.points(0.5, props[i, j], pch = 16, gp = gpar(col = "black"))
#                     if (j > 1)
#                        grid.lines(c(0, 0.5), c((props[i, j - 1] + props[i, j]) / 2, props[i, j]), 
#                                      gp = gpar(col = "black"))
#                     if (j < nc)
#                        grid.lines(c(0.5, 1), c(props[i, j], (props[i, j] + props[i, j + 1]) / 2), 
#                                      gp = gpar(col = "black"))
                  }
                  else if (structure == "column populations") {
                     grid.segments(c(0.25, 0.5, 0.75), rep(0, 3), c(0.25, 0.5, 0.75), rep(1, 3),
                  	                  gp = gpar(col = col.lines))
                     grid.points(props[i, j], 0.5, pch = 16, gp = gpar(col = "black"))
                     if (i > 1)
                        grid.lines(c((props[i - 1, j] + props[i, j]) / 2, props[i, j]), c(1, 0.5), 
                                      gp = gpar(col = "black"))
                     if (i < nr)
                        grid.lines(c(props[i, j], (props[i, j] + props[i + 1, j]) / 2), c(0.5, 0),
                                      gp = gpar(col = "black"))
                  }
                  if (reference.model & superimpose) {
                     if (structure == "row populations") {
#                        grid.points(0.5, props0[j], gp = gpar(col = "red"))
#                        if (j > 1)
#                           grid.lines(c(0, 0.5), c((props0[j - 1] + props0[j]) / 2, props0[j]), 
#                                         gp = gpar(col = "red"))
#                        if (j < nc)
#                           grid.lines(c(0.5, 1), c(props0[j], (props0[j] + props0[j + 1]) / 2), 
#                                         gp = gpar(col = "red"))
                     }
                     else if (structure == "column populations") {
                        grid.points(props0[i], 0.5, gp = gpar(col = "red"))
                        if (i > 1)
                           grid.lines(c((props0[i - 1] + props0[i]) / 2, props0[i]), c(1, 0.5),
                                         gp = gpar(col = "red"))
                        if (i < nr)
                           grid.lines(c(props0[i], (props0[i] + props0[i + 1]) / 2), c(0.5, 0),
                                         gp = gpar(col = "red"))
                     }
                     else if (structure == "table population") {
                     	pr <- apply(x, 2, sum) * sum(x[ ,j]) / sum(x)^2
                        grid.points(0.5, pr[i], gp = gpar(col = "red"))
                        if (j > 1)
                           grid.lines(c(0, 0.5), c((pr[j - 1] + pr[j]) / 2, pr[j]), 
                                         gp = gpar(col = "red"))
                        if (j < nc)
                           grid.lines(c(0.5, 1), c(pr[j], (pr[j] + pr[j + 1]) / 2),
                                         gp = gpar(col = "red"))
                     }
                  }
               }
               popViewport()
            }
         }
         
         for (i in 1:nr) {
            pushViewport(viewport(layout.pos.row = 2 * i + 2, layout.pos.col = 2))
            grid.rect(gp = gpar(fill = "grey70", col = "grey70", lwd = 1))
            grid.text(names[[1]][i], rot = 90)
            popViewport()
            if (dsply %in% c("proportion plots", "plots")) {
               if (structure == "row populations") {
                  pushViewport(viewport(layout.pos.row = 2 * i + 2,
                                        layout.pos.col = 2 * nc + 2))
                  grid.yaxis(at = scl / max(scl), 
                          label = as.character(scl),  
                          main = FALSE, gp = gpar(cex = 0.7, lineheight = 0.5, col = "slategrey"))
                  popViewport()
               }
            }
         }
         
         for (j in 1:nc) {
            pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2 * j +2))
            grid.rect(gp = gpar(fill = "grey70", col = "grey70", lwd = 0.05))
            grid.text(names[[2]][j])
            popViewport()
            if (dsply %in% c("proportion plots", "plots")) {
               if (structure == "column populations") {
                  pushViewport(viewport(layout.pos.row = 2 * nr + 2,
                                        layout.pos.col = 2 * j + 2))
                  grid.xaxis(at = seq(0, 1, by = 0.25), 
                          label = c("0", " ", "0.5", " ", "1"),  
                          main = TRUE, gp = gpar(cex = 0.7, col = "slategrey"))
                  popViewport()
               }
            }
         }
      })
      
      panel
   }
   

   rp.contingency.final <- function(panel) {
   
      with(panel, {

         rtots  <- apply(x, 1, sum)
         ctots  <- apply(x, 2, sum)
         rprops <- sweep(x, 1, rtots, "/")
         cprops <- sweep(x, 2, ctots, "/")
         tprops <- x / sum(x)

         dsply <- display
         if (structure == "unassigned") {
         	dsply           <- "table"
         	reference.model <- FALSE
         	scale           <- "counts"
         }
         if (structure == "row populations") {
            props  <- rprops
            props0 <- ctots / sum(ctots)
            rsep   <- 1
            csep   <- 0.15
         }
         else if (structure == "table population") {
            props  <- tprops
            props0 <- outer(rprops, cprops)
            rsep   <- 0.15
            csep   <- 0.15
         }
         else {
         	rsep   <- 0
         	csep   <- 0
         }

         grid.newpage()
         nr   <- nrow(x)
         nc   <- ncol(x)
         g <- grid.layout(7, 1 + nr * (nc + 1) + 1,
                       widths  = unit(c(      1, rep(c(rep(     1, nc),     0.5), nr),      1),
                                      c("lines", rep(c(rep("null", nc), "lines"), nr), "null")),
                       heights = unit(c(1, 1, 0.15, 1, 0.15, 1, 1),
                                      c(rep("lines", 3), "null", rep("lines", 3))))
         pushViewport(viewport(layout = g))
         
         scl  <- if (scale == "proportions") pretty(c(0, max(props)))
                 else                        pretty(c(0, x), 3)
         nscl <- length(scl)

         col.background <- "white"
         col.model      <- "darkseagreen4"
         
         for (i in 1:nr) {
            for (j in 1:nc) {
            	
               pushViewport(viewport(layout.pos.row = 4, 
                                     layout.pos.col = 1 + (i - 1) * (nr + 1) + j))
               grid.rect(gp = gpar(fill = col.background, col = col.background))
               # grid.segments(rep(0, nscl - 2), scl[-c(1, nscl)] / max(scl), 
                  	         # rep(1, nscl - 2), scl[-c(1, nscl)] / max(scl),
                   	         # gp = gpar(col = col.lines))
               ncells <- nrow(x) * ncol(x)
               est0   <- if (scale == "counts") props0[j] * rtots[i]
                   	 	 else                   props0[j]
               se.i   <- if (scale == "counts") sqrt(props0[j] * rtots[i])
                         else                   sqrt(props0[j] / rtots[i])
               zgrid <- seq(est0 - 3 * se.i, est0 + 3 * se.i, length = 100)
               igrid <- (zgrid > 0 & zgrid < max(scl))
               zgrid <- zgrid[igrid]
               ngrid <- length(zgrid)
               # lvl   <- as.numeric(substr(col.background, 5, 6))
               # shft  <- lvl - as.numeric(substr(col.model, 5, 6))
               # colg  <- lvl - round(shft * exp(-0.5 * (est0 - zgrid)^2 / se.i^2))
               # colg  <- paste("grey", colg, sep = "")
               colg  <- col2rgb(col.model)
               colg  <- c(rgb2hsv(colg))
               colg  <- hsv(rep(colg[1], ngrid),
                            colg[2] * dnorm(seq(-3, 3, length = 100)[igrid]) / dnorm(0),
                            rep(1, ngrid))
               # grid.segments(0.1, zgrid / max(scl), 0.9, zgrid / max(scl),
                             # gp = gpar(col = paste("grey", 
                             # lvl - round(shft * exp(-0.5 * (est0 - zgrid)^2 / se.i^2))), sep = ""))
               grid.rect(rep(0.5, ngrid), zgrid / max(scl),
                         width = rep(0.7, ngrid), height = 6 * se.i / (99 * max(scl)),
                         gp = gpar(fill = colg, col = colg))
               grid.lines(c(0.1, 0.1, 0.9, 0.9), c(0, rep(est0, 2), 0) / max(scl),
                           gp = gpar(col = col.model, lwd = 2))
               est  <- if (scale == "counts") x[i, j] else props[i, j]
               wdth <- if (scale == "counts")     0.2 else 0.8 * rtots[i] / sum(rtots)
               grid.rect(0.5, 0, width = wdth, height = est / max(scl),
                        just = c("centre", "bottom"),
                        gp = gpar(fill = col.data, col = col.data))

               if (structure == "table population") {
                  pr <- apply(x, 2, sum) * sum(x[ ,j]) / sum(x)^2
                  grid.points(0.5, pr[i], gp = gpar(col = "red"))
                  if (j > 1)
                     grid.lines(c(0, 0.5), c((pr[j - 1] + pr[j]) / 2, pr[j]), 
                                gp = gpar(col = "red"))
                  if (j < nc)
                     grid.lines(c(0.5, 1), c(pr[j], (pr[j] + pr[j + 1]) / 2),
                                gp = gpar(col = "red"))
               }
               popViewport()
            }
         }
         
         # Category labels
         for (i in 1:nr) {
            pushViewport(viewport(layout.pos.row = 2,
                                  layout.pos.col = 1 + (nc + 1) * (i - 1) + 1:nc))
            grid.rect(gp = gpar(fill = "grey70", col = "grey70", lwd = 1))
            grid.text(names[[1]][i])
            popViewport()
            for (j in 1:nc) {
               pushViewport(viewport(layout.pos.row = 6,
                                     layout.pos.col = 1 + (nc + 1) * (i - 1) + j))
               grid.rect(gp = gpar(fill = "grey70", col = "grey70", lwd = 1))
               grid.text(names[[2]][j], gp = gpar(cex = 0.5))
               popViewport()
            }
         }
         
         # p-value
         pushViewport(viewport(layout.pos.row = 7, layout.pos.col = 2:(nr * (nc + 1))))
         grid.text(paste("p.value =", round(chisq.test(x)$p.value, 3)))
         popViewport()
         
         pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1 + (nc + 1) * nr))
         grid.yaxis(at = scl / max(scl), 
                    label = as.character(scl),  
                    main = FALSE, gp = gpar(cex = 0.7, lineheight = 0.5, col = "slategrey"))
         popViewport()
                           
   	  })
      panel
   }
   
   rp.contingency.testplot <- function(panel) {
      with(panel, {
         if (panel$reference.model & panel$superimpose & panel$structure != "unassigned") {
         	clr   <- paste("grey", round(seq(100, 0, length = 101)), sep = "")
         	exps  <- outer(apply(x, 1, sum), apply(x, 2, sum)) / sum(x)
            tstat <- sum((x - exps)^2 / exps)
         	degf  <- (nrow(x) - 1) * (ncol(x) - 1)
         	pct   <- qchisq(0.99, degf)
       	    xlim  <- max(pct * 1.5, tstat * 1.1)
       	    grd   <- seq(0, xlim, length = 100)
       	    del   <- diff(grd)[1] / 2
       	    grd   <- grd[-1] - del
       	    ind   <- cut(dchisq(grd, degf), length(clr), labels = FALSE)
       	    par(mar = c(1, 1, 1, 1), oma = rep(0, 4), tcl = -0.2, xaxs = "i", mgp = c(1, 0, 0))
       	    plot(c(0, xlim), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
       	    axis(1, cex.axis = 0.7)
       	    lines(par()$usr[1:2], rep(par()$usr[3], 2))
       	    rect(grd - del, 0.05, grd + del, 1, col = clr[ind], border = clr[ind])
       	    points(tstat, 0.525, col = "red", pch = 16)
       	    title(paste("p-value:", round(1 - pchisq(tstat, degf), 3)),
       	        cex.main = 0.8, font.main = 1)
         }
         else {
       	    par(mar = c(0, 0, 0, 0) + 0.1)
            plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
         }
      })
      panel
   }

   rp.contingency.redraw <- function(panel) {
      rp.tkrreplot(panel, plot)
      rp.tkrreplot(panel, testplot)
      panel
   }

   rp.contingency.swap <- function(panel) {
      panel$x <- t(panel$x)
      rp.do(panel, rp.contingency.redraw)
      panel
   }

   if (panel) {
     panel <- rp.control(x = x, display = display, reference.model = reference.model,
                    col.data = col.data, col.model = col.model, col.background = col.background,
                    col.lines = col.lines, variation = variation, superimpose = superimpose,
                    scale = scale, final = final, margins = margins)
     if (panel.plot) {
       rp.tkrplot(panel, plot, rp.contingency.draw, pos = "right",hscale = hscale, vscale = vscale)
       action.fn <- rp.contingency.redraw
     }
     else
         action.fn <- rp.contingency.draw

     rp.radiogroup(panel, structure, 
               c("unassigned", "row populations", "column populations", "table population"),
               initval = structure, action = rp.contingency.redraw)
     rp.radiogroup(panel, scale, c("counts", "proportions"), action = rp.contingency.redraw)
     rp.radiogroup(panel, display, c("table", "plots"), action = rp.contingency.redraw)
     rp.checkbox(panel, reference.model, rp.contingency.redraw, "reference model")
     rp.checkbox(panel, superimpose, rp.contingency.redraw, "compare models")
     rp.checkbox(panel, variation, rp.contingency.redraw, "show variability")
     rp.tkrplot(panel, testplot, rp.contingency.testplot, hscale = 0.5, vscale = 0.15)
     rp.do(panel, rp.contingency.redraw)
     # rp.button(panel, rp.contingency.swap, "Swap rows and columns")
   }
   else {
     panel <- list(x = x, display = display, reference.model = reference.model,
                    col.data = col.data, col.model = col.model, col.background = col.background,
                    col.lines = col.lines, variation = variation, superimpose = superimpose,
                    scale = scale, final = final, margins = margins)
     rp.contingency.draw(panel)
   }
   
   invisible()

}

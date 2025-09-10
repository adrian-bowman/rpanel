#     A new go at graphics for a contingency table

# Allow only the case of two rows in mosaic style?

rp.contingency.ggforce <- function(d) {
   
   plt <- ggplot2::ggplot(d, ggplot2::aes(cnms, p)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(x = cnms, y = p / 2, label = lbl)) +
      ggplot2::facet_wrap(~rnms, ncol = 1, strip.position = 'right')
      # ggforce::facet_col(~rnms, scales = 'free_y', strip.position = 'right')
      # ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
      #                panel.grid.minor = ggplot2::element_blank(),
      #                # axis.text.x =  ggplot2::element_blank(),
      #                axis.ticks.x = ggplot2::element_blank(),
      #                axis.title.x = ggplot2::element_blank(),
      #                axis.title.y = ggplot2::element_blank())
                     # axis.text.y  = ggplot2::element_text(angle = 90,
                                             # vjust = 0.5, hjust = 1)) +

   uncertainty <- FALSE
   if (uncertainty) {
      # Display the common pattern of proportions
      xc        <- (d$xmn + d$xmx) / 2
      wdth      <- (d$xmx - d$xmn) + sep_x
      ind       <- (cnms == colnames(p)[1])
      xc[ind]   <- xc[ind] + sep_x / 4
      wdth[ind] <- wdth[ind] - sep_x / 2
      ind       <- (cnms == colnames(p)[ncl])
      xc[ind]   <- xc[ind] - sep_x / 4
      wdth[ind] <- wdth[ind] - sep_x / 2
      common <- ymn + rep(rowSums(x) / sum(x), ncl)
      if (!aligned) {
         common      <- 1 - cumsum(rowSums(x) / sum(x))
         common[nrw] <- sum(x[nrw, ]) / sum(x)
         common      <- rep(common, ncl)
      }
      # Is this formula correct?
      se    <- sqrt(common / (colsums * nrw * ncl))
      ind   <- if (aligned) 1:nrow(d)
               else if (nrw == 2) which(rnms %in% levels(rnms)[1])
               else which(rnms %in% levels(rnms)[c(1, nrw)])
      ngrid <- 101
      dlim  <- 3
      ygrid <- seq(-dlim, dlim, length = ngrid)
      dens  <- rep(dnorm(ygrid), (nrw - as.numeric(!aligned)) * ncl)
      fn    <- function(x) common[x] + ygrid * se[x]
      y     <- c(sapply(ind, fn))
      wdth  <- rep(wdth[ind], each = ngrid)
      hght  <- rep((ygrid[2] - ygrid[1]) * se[ind], each = ngrid)
      alpha <- dens * 0.7 / max(dens)
      if (!shading) {
         # Add horizontal lines to show the common proportions
         alpha <- 0.7
         dfrm  <- data.frame(x = xmn[ind], xend = xmx[ind], y = common[ind])
         plt   <- plt + ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y),
                                              col = rp.colours['reference'], linewidth = 1,
                                              alpha = alpha, data = dfrm)
         # This line needs to be fixed to scale the densities
         dscl  <- 0.1 / max(dens)
         wdth  <- dens * dscl
         alpha <- rep(alpha, length(y))
      }
      dgrd <- data.frame(x = rep(xc[ind],   each = ngrid), y, wdth, hght, alpha)
      clr  <- if (aligned) 'grey25' else 'white'
      plt  <- plt +
         ggplot2::geom_tile(ggplot2::aes(x, y, width = wdth, height = hght),
                            alpha = alpha,
                            fill = clr,
                            # fill = rp.colours['reference'],
                            # fill = 'grey25',
                            # fill = 'white',
                            # fill = 'black',
                            data = dgrd)
      # if (shading)
      #    plt <- plt + ggplot2::scale_alpha(range = c(0.5, 0.8))
      # Add notches
      xstart <- if (shading) rep(xmn[ind], 2)
                else rep(xc[ind] - dnorm(2) * dscl / 2, 2)
      xstop  <- if (shading) rep(xmx[ind], 2)
                else rep(xc[ind] + dnorm(2) * dscl / 2, 2)
      ltype  <- if (shading)  'dashed' else 'solid'
      dfrm   <- data.frame(x = xstart, xend = xstop,
                           y = c(common[ind] + 2 * se[ind],
                                 common[ind] - 2 * se[ind]))
      print(dfrm)
      plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend),
                              col = clr, linetype = 'dashed',
                              data = dfrm)
    }
   
   # scales <- FALSE
   # if (!scales)
   #    plt <- plt + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
   #                                axis.ticks.y = ggplot2::element_blank())
   
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


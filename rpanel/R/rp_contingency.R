#     A new go at graphics for a contingency table

# Allow only the case of two rows in mosaic style?

rp.contingency <- function(x, style = "mosaic", values = "observed",
                           uncertainty = FALSE, uncertainty.style = 'shading') {
   
   if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("the ggplot2 package is not available.")
   if (length(style) != 1)
      stop("'style' should be of length 1.")
   if (length(values) != 1)
      stop("'values' should be of length 1.")
   if (!(style %in% c("mosaic", "aligned")))
      stop("'style' setting not recognised.")
   if (!(uncertainty.style %in% c("shading", "violin")))
      stop("'uncertainty.style' setting not recognised.")
   if (!all(values %in% c("observed", "proportions", "expected")))
      stop("'values' setting not recognised.")
   if (any(is.na(x)))
      stop("missing data are not allowed.")
   if (!any(class(x) %in% c("matrix", "table")))
      stop("the input should be a matrix or a table.")
   if (length(dim(x)) != 2)
      stop("the input should be two-dimensional.")
   is.wholenumber <-
      function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
   if (!all(is.wholenumber(c(x))))
      stop("the input should consist only of integer counts.")
   if (nrow(x) < 2 | ncol(x) < 2)
      stop("there must be at least two rows and columns.")
   
   nrw      <- nrow(x)
   ncl      <- ncol(x)
   # if (nrw == 1) uncertainty <- FALSE
   # aligned  <- (style == "aligned") | (style == "mosaic" & nrw > 3)
   if (uncertainty & (nrw > 2)) style <- "aligned"
   aligned <- (style == "aligned")
   sep_x    <- 0 # 0.02
   sep_y    <- if (aligned) 0.02 else 0
   colsums  <- colSums(x)
   p        <- sweep(x, 2, colsums, "/")
   pmx      <- apply(p, 1, max)
   x_wdth   <- colsums / sum(colsums)
   xmn_mar  <- c(0, cumsum(x_wdth + sep_x)[-ncl])
   xmx_mar  <- xmn_mar + x_wdth
   xbrks    <- (xmn_mar + xmx_mar) / 2
   xmn      <- rep(xmn_mar, each = nrw)
   xmx      <- rep(xmx_mar, each = nrw)
   rnms     <- factor(rep(rownames(p), ncl), levels = rownames(p))
   cnms     <- factor(rep(colnames(p), each = nrw), levels = colnames(p))
   expected <- round(outer(rowSums(x), colSums(x)) / sum(x), 1)
   lbl      <- switch(values, "proportions" = c(signif(p, 2)),
                              "observed"    = c(x),
                              "expected"    = c(expected))
   p_vec    <- if (style == "mosaic") c(p) else rep(pmx, ncl)
   colt     <- sum(p_vec[1:nrw])
   ymn      <- rep((1:ncl) * (colt + nrw * sep_y), each = nrw) - cumsum(p_vec + sep_y)
   ymx      <- ymn + c(p)
   lblht    <- if (aligned) pmx else p[ , 1]
   ybrks    <- ymn[1:nrw] + lblht / 2
   d        <- data.frame(p = c(p), x = c(x), xmn, xmx, ymn, ymx, rnms, cnms,
                          pmx = ymn + rep(pmx, ncl), row.names = NULL)
   # db <- data.frame(xmn_mar, xmx_mar,
   #                  ymn_mar = rep(0, ncl), ymx_mar = rep(max(ymx), ncl))
   # dc <- data.frame(x    = c(xmn, xmx, xmx, xmn),
   #                  xend = c(xmx, xmx, xmn, xmn),
   #                  y    = c(ymn + common, ymn + common, ymn, ymn),
   #                  yend = c(ymn + common, ymn, ymn, ymn + common))

   plt <- ggplot2::ggplot(d)
   
   # Create a light grey background for aligned display
   # if (display == "aligned")
   #    plt <- plt + ggplot2::geom_rect(ggplot2::aes(xmin = xmn, xmax = xmx,
   #                                                 ymin = ymn, ymax = pmx),
   #                                    fill = grey(0.9), col = grey(0.9))
   # ggplot2::geom_rect(data = db,
      #                    ggplot2::aes(xmin = xmn_mar, xmax = xmx_mar,
      #                                 ymin = ymn_mar, ymax = ymx_mar),
      #                    fill = grey(0.9), col = grey(0.9)) +

   plt <- plt +
      ggplot2::geom_rect(ggplot2::aes(xmin = xmn, xmax = xmx,
                                      ymin = ymn, ymax = ymx,
                                      fill = rnms), col = "black") +
      ggplot2::geom_text(ggplot2::aes(x = (xmn + xmx) / 2,
                                      y = (ymn + ymx) / 2, label = lbl)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"),
                     # panel.spacing = unit(1, "lines"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     # axis.text.x =  ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank()) +
                     # axis.text.y  = ggplot2::element_text(angle = 90,
                                             # vjust = 0.5, hjust = 1)) +
      ggplot2::scale_x_continuous(breaks = xbrks, labels = colnames(x),
                                  expand = rep(0, 2), position = "top") +
      ggplot2::scale_y_continuous(breaks = ybrks, labels = rownames(x),
                                  expand = rep(0, 2)) +
      ggplot2::theme(legend.position = "none")
   
   if (uncertainty) {
      # Display the common pattern of proportions
      clr <- "darkgrey"
      # plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = xmn, y = common,
      #                                                 xend = xmx, yend = common),
      #                                    col = clr_band)
      # plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = x, y = y,
      #                                                 xend = xend, yend = yend),
      #                                    data = dc,
      #                                    col = clr, fill = clr)
      # plt <- plt + ggplot2::geom_rect(ggplot2::aes(xmin = xmn, xmax = xmx,
      #                                              ymin = ymn, ymax = common),
      #                                 col = clr, fill = clr, alpha = 0.7)
      xc        <- (d$xmn + d$xmx) / 2
      wdth      <- (d$xmx - d$xmn) + sep_x
      ind       <- (cnms == colnames(p)[1])
      xc[ind]   <- xc[ind] + sep_x / 4
      wdth[ind] <- wdth[ind] - sep_x / 2
      ind       <- (cnms == colnames(p)[ncl])
      xc[ind]   <- xc[ind] - sep_x / 4
      wdth[ind] <- wdth[ind] - sep_x / 2
      # For geom_rect
      # dgrd  <- data.frame(xmin = rep(d$xmn - sep_x / 2, each = ngrid - 1),
      #                     xmax = rep(d$xmx + sep_x / 2, each = ngrid - 1),
      #                     ymin = ygrd[-length(ygrd)],
      #                     ymax = ygrd[-1],
      #                     pmx  = rep(d$pmx, each = ngrid - 1),
      #                     dens = rep(dnorm((ygrd[-length(ygrd)] + ygrd[-1]) / 2, nrw * ncl)))
      # For geom_tile
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
      ngrid <- 51
      ygrid <- seq(-3, 3, length = ngrid)
      dens  <- rep(dnorm(ygrid), (nrw - as.numeric(!aligned)) * ncl)
      xc    <- rep(xc[ind],   each = ngrid)
      fn    <- function(x) common[x] + ygrid * se[x]
      y     <- c(sapply(ind, fn))
      wdth  <- rep(wdth[ind], each = ngrid)
      hght  <- rep((ygrid[2] - ygrid[1]) * se[ind], each = ngrid)
      alpha <- dens
      if (uncertainty.style == 'violin') {
         # Add horizontal lines to show the common proportions
         dfrm  <- data.frame(x = xmn[ind], xend = xmx[ind], y = common[ind])
         plt   <- plt + ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y),
                                          col = clr, linewidth = 1, alpha = 0.7,
                                          data = dfrm)
         # This line needs to be fixed
         dens  <- 0.1 *dens / max(dens)
         wdth  <- dens
         alpha <- rep(0.7, length(y))
      }
      dgrd <- data.frame(x = xc, y, wdth, hght, alpha)
      plt  <- plt +
         ggplot2::geom_tile(ggplot2::aes(x, y, width = wdth, height = hght, alpha = alpha),
                            fill = clr, data = dgrd)
      if (uncertainty.style == 'shading')
         plt <- plt + ggplot2::scale_alpha(range = c(0, 0.8))
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


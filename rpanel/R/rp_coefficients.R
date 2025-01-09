# Effect plots for linear regression models

rp.coefficients <- function(model, style = 'density',
                            ci = TRUE, point.estimate = !ci,
                            se.scale = FALSE, marks = c(-2, 2),
                            labels, subset, col, ngrid = 200) {

   if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("the ggplot2 package is not available.")

   if (!('lm' %in% class(model)))
      stop("'model' is not a linear model object.")
   if (!("x" %in% names(model)))
      model$x <- model.matrix(model, model.frame(model))

   if ((length(style) != 1) | !(style %in% c('density', 'shading'))) {
      warning("invalid value for 'style' - setting to 'density'.")
      style <- 'density'
   }
   
	col    <- if (missing(col))    "#86B875" else col # colorspace::rainbow_hcl(3)[2]
	sbst   <- if (missing(subset)) NA else subset
	lbls   <- if (missing(labels)) NA else labels
	
	tbl <- summary(model)$coefficients

	# Apply subset, removing the intercept if subset is not specified
	if (any(is.na(sbst))) {
	   sbst <- 1:nrow(tbl)
	   intcpt <- match('(Intercept)', rownames(tbl))
	   if (!is.na(intcpt)) sbst <- sbst[-intcpt] 
	}
	if (length(sbst) == 0)
	   stop('no non-intercept terms are present.')
	tbl <- tbl[sbst, , drop = FALSE]
	
	# Check the labels
	if (any(is.na(lbls)))
	   lbls  <- rownames(tbl)
	# else
	#    renamed <- (length(lbls) == nrow(tbl))
	if (length(lbls) < nrow(tbl))
	   stop("the length of labels is smaller that the number of model terms (after 'subset' has been applied, if used).")
	# Allow labels which are a superset
	else if (length(lbls) > nrow(tbl)) {
	   if (!all(rownames(tbl) %in% lbls))
	      stop('the labels do not contain all the model terms.')
	   else
	      lbls.coef <- match(rownames(tbl), lbls)
	}
	else
	   lbls.coef <- 1:nrow(tbl)
	# else if (length(lbls) != nrow(tbl))
	#    stop("the 'labels' argument does not match the number of model terms.")
	
	coeff <- tbl[ , 1]
	se    <- tbl[ , 2]
	x     <- model$x[ , sbst, drop = FALSE]
	yname <- all.vars(model$call)[1]
	rng   <- t(apply(x, 2, range))
	for (i in 1:length(coeff)) {
	   if (abs(diff(rng[i, ]) - 1) > 1e-8)
	         lbls[lbls.coef[i]] <- paste(lbls[lbls.coef[i]], "\n(", signif(rng[i, 1], 5),
	                            " - ", signif(rng[i, 2], 5), ")", sep = "")
	}
	lbls   <- factor(lbls, levels = lbls)	# Order of the terms retained
	lbls   <- lbls[lbls.coef]
	ncoef  <- length(coeff)
	coeff  <- coeff * (rng[ , 2] - rng[ , 1])
	se     <-    se * (rng[ , 2] - rng[ , 1])
	mn     <- if (ci) coeff else rep(0, ncoef)
	yrange <- range(min(mn - 3 * se), max(mn + 3 * se), coeff, 0)
	mn     <- rep(mn, each = ngrid)
	se     <- rep(se, each = ngrid)
	ygrid  <- seq(yrange[1], yrange[2], length = ngrid + 2)[2:(ngrid + 1)] # Avoid end-points
	ygrid  <- rep(ygrid, ncoef)
	dgrid  <- dnorm(ygrid, mn, se) * se # multiply by se to give common maximum intensity
	dfrm   <- data.frame(x = rep(lbls, each = ngrid), y = ygrid, d = dgrid)
	
	# Create the main plot features
	plt <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y))
	if (style == 'density')
	   plt <- plt +
	      ggplot2::geom_tile(width = dgrid, fill = col, show.legend = FALSE)
	else if (style == 'shading')
	   plt <- plt +
	      ggplot2::geom_tile(ggplot2::aes(fill = d), width = 0.6, show.legend = FALSE) +
	      ggplot2::scale_fill_gradient(low = "grey92", high = col)
	
	# Add se scale if requested
	if (style == 'density' & se.scale) {
	   se.marks     <- seq(-3, 3, by = 1)
	   nmarks       <- length(se.marks)
	   mn           <- if (ci) coeff else rep(0, ncoef)
	   se           <- tbl[ , 2] * (rng[ , 2] - rng[ , 1])
	   dfrm.scale   <- data.frame(x  = rep(as.numeric(lbls), each = nmarks),
	                              mn = rep(mn, each = nmarks),
	                              se = rep(se, each = nmarks),
	                              sc = rep(se.marks, ncoef))
	   dfrm.scale$y <- dfrm.scale$mn + dfrm.scale$sc * dfrm.scale$se
	   dfrm.axes    <- data.frame(x    = lbls,
	                              y    = mn - min(se.marks) * se,
	                              yend = mn + min(se.marks) * se)
	   col.scale <- 'grey50'
	   plt <- plt +
	      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, yend = yend),
	                            col = col.scale, data = dfrm.axes) +
	      ggplot2::geom_segment(ggplot2::aes(x = x - 0.02, xend = x + 0.02, y = y),
	                            col = col.scale, data = dfrm.scale) +
	      ggplot2::geom_text(ggplot2::aes(x = x - 0.08, y = y,
	                                      label = as.character(sc)),
	                         col = col.scale, size = 3, data = dfrm.scale)
	}
	
	# Add se marks if requested
	if (style == 'density' & !is.null(marks)) {
	   mn           <- if (ci) coeff else rep(0, ncoef)
	   se           <- tbl[ , 2] * (rng[ , 2] - rng[ , 1])
	   dfrm.scale   <- data.frame(x  = rep(as.numeric(lbls), each = length(marks)),
	                              mn = rep(  mn, each = length(marks)),
	                              se = rep(  se, each = length(marks)),
	                              sc = rep(marks, ncoef))
	   dfrm.scale$y <- dfrm.scale$mn + dfrm.scale$sc * dfrm.scale$se
	   dfrm.scale$d <- dnorm(dfrm.scale$y, dfrm.scale$mn, dfrm.scale$se) * dfrm.scale$se
	   plt <- plt + ggplot2::geom_segment(ggplot2::aes(x    = x - 0.5 * d,
	                                                   xend = x + 0.5 * d,
	                                                   y    = y),
	                                      col = 'white', data = dfrm.scale)
	}
	
	# Other detailed aspects of the plot
	plt <-    plt + 
	          ggplot2::scale_x_discrete(drop = FALSE) +
	          ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
	          ggplot2::ylab(paste("Change in mean", yname)) +
	          ggplot2::xlab("Coefficients") +
	          ggplot2::ylim(yrange[1], yrange[2]) +
	          ggplot2::theme(panel.grid = ggplot2::element_blank(),
	                         legend.position = "none")
	if (point.estimate | !ci) {
	   dfrm.mn <- data.frame(x = as.numeric(lbls[lbls.coef]), y = coeff)
	   plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = x - 0.3, xend = x  + 0.3,
	                                                   fill = NULL, col = "red"),
	                                      data = dfrm.mn)
	}
	   
	print(plt)
	invisible(plt)
}
	
	# old code
#    cfs   <- coef(model)[-1]
#    p     <- length(cfs)
#    x     <- model$x[ , -1]
#    rng   <- diff(apply(x, 2, range))
#    chng  <- cfs * rng
#    se    <- coef(summary(model))[-1, 2] * rng
#    ylab  <- attr(model$terms, "variables")
#    ylab  <- strsplit(deparse(ylab), ",")[[1]][1]
#    ylab  <- substr(ylab, 6, nchar(ylab))
#    if (any(is.na(prng))) prng <- range(chng + 3 * se, chng - 3 * se)
#    par(mar = c(3, 3, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
#    plot(c(0.4, p + 0.6), prng, ylab = paste("Change in", ylab), xlab = "",
#    		 type = "n", axes = FALSE)
#    usr <- par("usr")
#    rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
#    abline(h = axTicks(2), col = "white")
#    abline(h = 0, lty = 2, lwd = 2)
#    # grid(col = "white", lty = 1)
#    axis(1, 1:p, names(cfs), tick = FALSE, lwd = 0, mgp = c(3, 0, 0))
#    rng <- signif(apply(x, 2, range))
#    rng <- paste(rng[1, ], "-", rng[2, ])
#    axis(1, 1:p, rng,        tick = FALSE, lwd = 0, mgp = c(3, 1, 0), cex.axis = 0.7)
#    axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
#    		 col.axis = grey(0.6), cex.axis = 0.8)
#    xgrid <- seq(usr[3], usr[4], length = 500)
#    for (i in 1:p) denstrip::denstrip(xgrid, dnorm(xgrid, chng[i], se[i]), i, 0.7,
#                               colmax = col, colmin = "transparent", horiz = FALSE)
#    # segments(usr[1], usr[3], usr[1], usr[4], lwd = 2)
#    invisible()
# }

# Density strip plots for linear regression models

rp.regression3 <- function(model, yrng, ci, point.estimate, labels, subset, col, ngrid) {

   if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("the ggplot2 package is not available.")

	if ("formula" %in% class(model)) model <- lm(model, x = TRUE)
	if (!("x" %in% names(model)))
	   stop("the 'model' argument needs to be created by calling 'lm' with the argument 'x' = TRUE'")
	
	if (is.na(col)) {
      col <- if (requireNamespace("colorspace", quietly = TRUE)) colorspace::rainbow_hcl(3)[2]
   	       else grey(0.5)
	}
	
	tbl <- summary(model)$coefficients

	# Apply subset, removing the intercept if subset is not specified
	if (any(is.na(subset))) {
	   subset <- 1:nrow(tbl)
	   intcpt <- match('(Intercept)', rownames(tbl))
	   if (!is.na(intcpt)) subset <- subset[-intcpt] 
	}
	if (length(subset) == 0)
	   stop('no non-intercept terms are present.')
	tbl <- tbl[subset, , drop = FALSE]
	
	# Check the labels
	if (any(is.na(labels)))
	   labels  <- rownames(tbl)
	else
	   renamed <- (length(labels) == nrow(tbl))
	if (length(labels) < nrow(tbl))
	   stop('the length of labels is smaller that the numer of model terms.')
	# Allow labels which are a superset
	else if (length(labels) > nrow(tbl)) {
	   if (!all(rownames(tbl) %in% labels))
	      stop('the labels do not contain all the model terms.')
	   else
	      labels.coef <- match(rownames(tbl), labels)
	}
	else
	   labels.coef <- 1:nrow(tbl)
	# else if (length(labels) != nrow(tbl))
	#    stop("the 'labels' argument does not match the number of model terms.")
	
	coeff <- tbl[ , 1]
	se    <- tbl[ , 2]
	x     <- model$x[ , subset, drop = FALSE]
	rng   <- t(apply(x, 2, range))
	for (i in 1:length(coeff)) {
	   if (abs(diff(rng[i, ]) - 1) > 1e-8)
	         labels[labels.coef[i]] <- paste(labels[labels.coef[i]], "\n(", signif(rng[i, 1], 5),
	                            " - ", signif(rng[i, 2], 5), ")", sep = "")
	}
	labels <- factor(labels, levels = labels)	# Order of the terms retained
	lbls   <- labels[labels.coef]
	ncoef  <- length(coeff)
	coeff  <- coeff * (rng[ , 2] - rng[ , 1])
	se     <- se * (rng[ , 2] - rng[ , 1])
	mn     <- if (ci) coeff else rep(0, ncoef)
	yrng   <- if (any(is.na(yrng))) range(min(mn - 3 * se), max(mn + 3 * se), coeff, 0) else yrng
	mn     <- rep(mn, each = ngrid)
	se     <- rep(se, each = ngrid)
	xgrid  <- seq(yrng[1], yrng[2], length = ngrid + 2)[2:(ngrid + 1)] # Avoid end-points
	xgrid  <- rep(xgrid, ncoef)
	dgrid  <- dnorm(xgrid, mn, se) * se # multiply by se to give common maximum intensity
	dfrm   <- data.frame(x = xgrid, y = rep(lbls, each = ngrid), d = dgrid)
	plt    <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y, fill = d)) +
	          ggplot2::geom_tile(height = 0.6, show.legend = FALSE) +
	          ggplot2::scale_fill_gradient(low = "grey92", high = col) +
	          ggplot2::scale_y_discrete(drop = FALSE) +
	          ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
	          ggplot2::xlab("Effect") + ggplot2::ylab("Model coefficients") +
	          ggplot2::xlim(yrng[1], yrng[2]) +
	          ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none")
	if (point.estimate | !ci)
	   plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = x, y = y - 0.4,
	                                                  xend = x, yend = y  + 0.4,
	                                                  fill = NULL, col = "red"),
	                          data = data.frame(x = unique(coeff), y = as.numeric(labels)))
	   
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

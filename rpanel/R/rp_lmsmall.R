#     A general function for linear models

rp.lmsmall <- function(x, ylab, xlab, zlab, ci = TRUE,
                       panel = TRUE, panel.plot = TRUE,
                       hscale = 1, vscale = hscale, ...) {
   
   # Deal with formula or model inputs
   class.x <- class(x)
   if ('formula' %in% class.x)
      model <- lm(x, ...)
   else if ('lm' %in% class.x)
      model <- x
   else
      stop('x is not a formula or a linear model object.')
   if (!('model' %in% names(model))) {
      model <- update(model, model = TRUE)
      message("model refitted with 'model = TRUE'.")
   }

   # Extract required information from the model
   mf           <- model$model
   response.ind <- attr(model$terms, 'response')
   var.types    <- attr(model$terms, 'dataClasses')
   numeric.ind  <- which(var.types == 'numeric')
   numeric.ind  <- numeric.ind[numeric.ind != response.ind]
   factor.ind   <- which(var.types == 'factor')
   trms         <- attr(model$terms, 'term.labels')
   if (length(trms) == 0) stop('at least one predictor variable is required.')
   if (length(numeric.ind) + length(factor.ind) > 2)
      stop('this function deals with no more than two predictor variables.')

   # Extract the raw data
   y <- mf[ , response.ind]
   x <- mf[ , trms[1]]
   z <- if (length(trms) > 1) mf[ , trms[2]] else rep(NA, length(x))
   jitter.x <- jitter(as.numeric(x), factor = 0.5, amount = NULL)
   
   # Set up variable labels
   yterm <- names(var.types)[response.ind]
   xterm <- trms[1]
   zterm <- if (length(trms) > 1) trms[2] else NA
   if (missing(ylab)) ylab <- yterm
   if (missing(xlab)) xlab <- xterm
   zlab <- if (length(trms) > 1 & missing(zlab)) zterm
   # if (length(numeric.ind) == 2 & length(factor.ind) == 0 & length(xlab) == 1)
   #    stop('xlab is of length 1 but length 2 is needed.')

   # Other models
   if (length(numeric.ind) == 1 & length(factor.ind) == 0) type <- 'regression.one'
   if (length(numeric.ind) == 2 & length(factor.ind) == 0) type <- 'regression.two'
   if (length(numeric.ind) == 1 & length(factor.ind) == 1) type <- 'ancova'
   if (length(numeric.ind) == 0 & length(factor.ind) == 1) type <- 'one.way'
   if (length(numeric.ind) == 0 & length(factor.ind) == 2) type <- 'two.way'
   ttl <- switch(type, regression.one = 'Simple regression',
                       regression.two = 'Regression with two covariates',
                               ancova = 'Analysis of covariance',
                              one.way = 'One-way analysis of variance',
                              two.way = 'Two-way analysis of variance')

   # Simple linear regression
   if (type == 'regression.one')
      return(rp.regression(x, y, panel = panel, xlab = xlab, ylab = ylab))
   
   bgdcol      <- "grey85"
   model.nodes <- data.frame(x = c(0.5, 0.25, 0.75, 0.5, 0.5),
                             y = 0.9 - c(0, 1, 1, 2, 3) * 0.25,
                             label = paste(yterm, '~',
                                           c('1', xterm, zterm,
                                             paste(xterm, zterm, sep = ' + '),
                                             paste(xterm, zterm,
                                                   paste(xterm, zterm, sep = ':'),
                                                   sep = ' + '))),
                             comparison1 = c(2, 3, 4, 4, 5),
                             comparison2 = c(1, 1, 2, 3, 4))
   if (type == 'one.way')
      model.nodes <- data.frame(x = c(0.5, 0.5),
                                y = 0.8 - c(0, 1) * 0.6,
                                label = paste(yterm, '~', c('1', xterm)),
                                comparison1 = 1, comparison2 = 2)
   
   # Find the coefficient names from the maximal model
   form <- paste(yterm, '~', trms[1])
   if (length(trms) > 1)
      form <- paste(yterm, '~', trms[1], '+', trms[2])
   if (type %in% c('ancova', 'two.way')) {
      form      <- paste(form, ' + ', trms[1], ':', trms[2], sep = '')
      model.max <- update(model, form)
   }
   else
      model.max <- model
   labels.max <- names(coefficients(model.max))[-1]
   
   models  <- list()
   nmodels <- switch(type, regression.two = 4, ancova = 5,
                           one.way = 2, two.way = 5)
   for (i in 1:nmodels)
      models[[i]] <- update(model, model.nodes$label[i])
   style <- if (requireNamespace('ggplot2', quietly = TRUE)) 'ggplot'
            else 'standard'
   
   # Linear regression with two covariates
   if (length(numeric.ind) == 2 & length(factor.ind) == 0)
      return(rp.regression2(y, x, z, ylab  = ylab, x1lab = xlab, x2lab = zlab,
                            panel = panel))
      
   if (panel) {
      pnl <- rp.control(ttl, models = models, y = y, x = x, z = z,
                        jitter.x = jitter.x,
                        type = type, style = style, labels.max = labels.max,
                        xlab = xlab, ylab = ylab, zlab = zlab,
                        yterm = yterm, xterm = xterm, zterm = zterm,
                        ci = ci, bgdcol = bgdcol, highlighted.node = NA,
                        model.nodes = model.nodes, click.coords = rep(NA, 2))
      rp.menu(pnl, model.display,
              list(c('Display', 'blank', 'coefficients', 'terms')),
              initval = 'blank', action = rp.lmsmall.redraw)
      rp.grid(pnl, "models", row = 0, column = 0, background = bgdcol)
      rp.tkrplot(pnl, modelnodes, rp.lmsmall.modelnodes, action = rp.lmsmall.click,
                 hscale = 0.7 * hscale, vscale = 0.5 * vscale, 
                 grid = "models", row = 0, column = 0, background = "white")

      if (panel.plot) {
         rp.grid(pnl, "dataplot", row = 0, column = 1, background = "white")
         rp.tkrplot(pnl, plot, rp.lmsmall.draw,
                    hscale = hscale, vscale = vscale, 
                    grid = "dataplot", row = 0, column = 0, background = "white")
         # rp.listbox(pnl, analysis, c('none', 'coefficients', 'terms'),
                    # title = 'analysis',
                    # grid = "models", row = 1, column = 0, background = "white")
         rp.tkrplot(pnl, fplot, rp.lmsmall.effectsplot,
                    hscale = hscale * 0.7, vscale = vscale * 0.5, 
                    grid = "models", row = 2, column = 0, background = bgdcol)
         action.fn <- rp.lmsmall.redraw
      }
      else {
         # This needs to be amended to handle the effects plot
         action.fn <- rp.lmsmall.draw
         rp.text(pnl, "        Model", grid = "models", row = 0, column = 1, background = bgdcol)
         rp.text(pnl,       "current", grid = "models", row = 1, column = 0, background = bgdcol)
         rp.text(pnl,           "new", grid = "models", row = 1, column = 2, background = bgdcol)
         rp.checkbox(pnl, model11, action.fn, labels = "", initval = init.model[1],
                     grid = "models", row = 2, column = 0, name = "model11", background = bgdcol)
         rp.checkbox(pnl, model12, action.fn, labels = "", initval = init.model[2],
                     grid = "models", row = 3, column = 0, name = "model12", background = bgdcol)
         for (i in 1:length(model.options)) rp.text(pnl, model.options[i],
                                                    grid = "models", row = i + 1, column = 1, background = bgdcol)
         rp.checkbox(pnl, model01, action.fn, labels = "", initval = init.model0[1],
                     grid = "models", row = 2, column = 2, name = "model01", background = bgdcol)
         rp.checkbox(pnl, model02, action.fn, labels = "", initval = init.model0[1],
                     grid = "models", row = 3, column = 2, name = "model02", background = bgdcol)
         # rp.checkbox(pnl, model, action.fn, rep("", length(model.options)), title = "current",
         #      initval = init.model, grid = "controls", row = 1, column = 0, background = bgdcol)
         rp.checkbox(pnl, model13, action.fn, labels = "", initval = init.model[3],
                     grid = "models", row = 4, column = 0, name = "model13", background = bgdcol)
         rp.checkbox(pnl, model14, action.fn, labels = "", initval = init.model[4],
                     grid = "models", row = 5, column = 0, name = "model14", background = bgdcol)
         # rp.grid(pnl, "models", row = 1, column = 1, grid = "controls")
         rp.checkbox(pnl, model03, action.fn, labels = "", initval = init.model0[1],
                     grid = "models", row = 4, column = 2, name = "model03", background = bgdcol)
         rp.checkbox(pnl, model04, action.fn, labels = "", initval = init.model0[1],
                     grid = "models", row = 5, column = 2, name = "model04", background = bgdcol)
      }
      # rp.do(pnl, action.fn)
   }
   else {
      pnl <- list(x = x, y = y, z = factor(group), xlab = xlab, ylab = ylab,
                  xterm = xterm, zterm = zterm, term.names = term.names, 
                  style = style, bgdcol = bgdcol)
      rp.lmsmall.draw(pnl)
   }
   
   invisible()
   
}

rp.lmsmall.modelnodes <- function(panel) {
   fillcol <- rep('white', 5)
   if (!any(is.na(panel$highlighted.node)))
      fillcol[panel$highlighted.node] <- 'lightblue'
   
   with(panel$model.nodes, {
      par(oma = c(0, 0, 1, 0), plt = c(0, 1, 0, 1))
      plot(0:1, 0:1, type = 'n', axes = FALSE, xlab = '', ylab = '')
      segments(x[c(1, 1, 2, 3, 4)], y[c(1, 1, 2, 3, 4)],
               x[c(2, 3, 4, 4, 5)], y[c(2, 3, 4, 4, 5)])
      sw   <- strwidth(label)
      sh   <- strheight(label)
      frsz <- 0.02
      rect(x - sw/2 - frsz, y - sh/2 - frsz, x + sw/2 + frsz, y + sh/2 + frsz,
           col = fillcol)
      text(x, y, label)
      if (!is.null(comparison1)) {
         points((x[comparison1] + x[comparison2]) / 2,
                (y[comparison1] + y[comparison2]) / 2,
                pch = 16, col = 'grey', cex = 2)
      }
      text(0.5, 1, '(click to fit)')
      title('Models', outer = TRUE)
   })
   
   # ggplot code - but this does not allow clicked co-ordinate identification
   # plt <- ggplot2::ggplot(panel$model.nodes, ggplot2::aes(x, y, label = label)) +
   #    ggplot2::annotate('segment',
   #                      x    = panel$model.nodes$x[c(1, 1, 2, 3, 4)],
   #                      y    = panel$model.nodes$y[c(1, 1, 2, 3, 4)],
   #                      xend = panel$model.nodes$x[c(2, 3, 4, 4, 5)],
   #                      yend = panel$model.nodes$y[c(2, 3, 4, 4, 5)]) +
   #    ggplot2::geom_label(size = 6, fill = fillcol) +
   #    ggplot2::theme_void() +
   #    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
   #    ggplot2::ggtitle("Models") +
   #    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20,
   #                                                      face = "bold"))
   # print(plt)
   
   panel
}

rp.lmsmall.click <- function(panel, x, y) {
   d.nodes <- (panel$model.nodes$x - x)^2 + (panel$model.nodes$y - y)^2
   comp1   <- panel$model.nodes$comparison1
   comp2   <- panel$model.nodes$comparison2
   if (!is.null(comp1)) {
      x.comps <- (panel$model.nodes$x[comp1] + panel$model.nodes$x[comp2]) / 2
      y.comps <- (panel$model.nodes$y[comp1] + panel$model.nodes$y[comp2]) / 2
      d.comps <- (x.comps - x)^2 + (y.comps - y)^2
      d.nodes <- c(d.nodes, d.comps)
   }
   hpt <- which.min(d.nodes)
   n.nodes <- length(panel$model.nodes$x)
   panel$comparison <- NULL
   if (hpt > n.nodes) {
      hpt <- hpt - n.nodes
      panel$highlighted.node <- c(comp1[hpt], comp2[hpt])
      panel$comparison <- hpt
   }
   else
      panel$highlighted.node <- hpt
   rp.control.put(panel$panelname, panel)
   rp.tkrreplot(panel, modelnodes)
   rp.tkrreplot(panel, plot)
   rp.tkrreplot(panel, fplot)
   panel
}

rp.lmsmall.draw <- function(panel) {
   
   hlight <- panel$highlighted.node
   
   # Ancova
   if (panel$type == 'ancova') {
      
      dfrm <- data.frame(y = panel$y, x = panel$x, z = panel$z)
      xlab <- panel$xlab
      zlab <- panel$zlab
      if (is.factor(panel$x)) {
         dfrm$x <- panel$z
         dfrm$z <- panel$x
         xlab   <- panel$zlab
         zlab   <- panel$xlab
      }
      if (!any(is.na(hlight))) {
         mdl            <- panel$models[[hlight[1]]]
         dfrm$fvals     <- fitted(mdl)
         var.types      <- attr(mdl$terms, 'dataClasses')
         factor.present <- any(var.types == 'factor')
      }
      
      if (panel$style == 'ggplot') {
         plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y, group = z, col = z)) +
            ggplot2::geom_point() +
            ggplot2::labs(x = xlab, y = panel$ylab, col = zlab)
         if (!any(is.na(hlight))) {
            if (factor.present)
               plt <- plt + ggplot2::geom_line(ggplot2::aes(y = fvals))
            else
               plt <- plt + ggplot2::geom_line(ggplot2::aes(y = fvals,
                                             col = NULL, group = NULL))
            form <- panel$model.nodes$label[hlight[1]]
         }
         else
            form <- 'none'
         plt <- plt + ggplot2::ggtitle(paste("Model:", form))
         print(plt)
      }
   
   }
   
   # Anova
   if (panel$type %in% c('one.way', 'two.way')) {
      clr  <- colorspace::rainbow_hcl(3)
      dfrm <- data.frame(x = panel$x, y = panel$y, z = panel$z,
                         jitter.x = panel$jitter.x)
      plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(y, x)) +
         ggplot2::xlab(panel$ylab) + ggplot2::ylab(panel$xlab) +
         ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.background = ggplot2::element_rect(fill = "grey90")) +
         # ggplot2::geom_hline(yintercept = 1:nlevels(x) + 0.5, col = "white") +
         ggplot2::ggtitle(panel$ttl)
      if (length(hlight) != 2) {
         plt <- plt + ggplot2::stat_density(ggplot2::aes(fill = ggplot2::after_stat(density)),
                                            geom = "tile",
                                            height = 0.7, position = "identity", show.legend = FALSE) +
            ggplot2::scale_fill_gradient(low = "grey90", high = clr[3])
      }
      else if (length(hlight) == 2) {
         mdl   <- panel$models[[hlight[1]]]
         mdl0  <- panel$models[[hlight[2]]]
         mn    <- tapply(fitted(mdl0), list(panel$x, panel$z), mean)
         se    <- tapply(fitted(mdl0), list(panel$x, panel$z), length)
         ngps  <- nrow(unique(data.frame(panel$x, panel$z)))
         df0   <- mdl0$df.residual
         df1   <- mdl$df.residual
         se    <- summary(mdl)$sigma * sqrt(abs(df0 - df1)) / sqrt(se * ngps)
         dfrm1 <- data.frame(y = c(mn), x = rep(rownames(mn), ncol(mn)),
                             z = rep(colnames(mn), each = nrow(mn)))
         ngrid <- 200
         xgrid <- seq(min(panel$y), max(panel$y), length = ngrid)
         dfrm1 <- data.frame(xgrid = rep(xgrid, each = nrow(dfrm1)),
                             x = rep(dfrm1$x, ngrid), z = rep(dfrm1$z, ngrid))
         dfrm1$dgrid <- dnorm(dfrm1$xgrid, mn[cbind(dfrm1$x, dfrm1$z)],
                              se[cbind(dfrm1$x, dfrm1$z)])
         plt <- plt + ggplot2::geom_tile(ggplot2::aes(x = xgrid, y = x, fill = dgrid),
                                         height = 0.8, data = dfrm1,
                                         show.legend = FALSE) +
            ggplot2::scale_fill_gradient(low = "grey90", high = clr[2])
      }
      if (!any(is.na(hlight))) {
         mdl <- panel$models[[hlight[1]]]
         plt <- plt + ggplot2::stat_summary(ggplot2::aes(x = fitted(mdl)), width = 0,
                                            fun = "mean", linewidth = 1,
                                            fun.min = function(x) mean(x) - 0.45,
                                            fun.max = function(x) mean(x) + 0.45,
                                            geom = "crossbar", orientation = "x", col = clr[1])
      }
      fvs  <- c(sapply(panel$models, function(x) range(fitted(x))))
      yrng <- range(panel$y, fvs)
      plt  <- plt +
         ggplot2::geom_point(ggplot2::aes(x = y, y = jitter.x), col = clr[3]) +
         ggplot2::xlim(yrng[1], yrng[2])
      plt  <- plt + ggplot2::coord_flip()
      if (panel$type == "two.way")
         plt <- plt + ggplot2::facet_grid(. ~ z)
      # if (!interactive) {
      #    plt   <- plt + ggplot2::ggtitle(paste("p-value:", round(p.value, 3)))
      #    panel <- plt
      # }
      print(plt)
   }

   panel
}

rp.lmsmall.effectsplot <- function(panel) {
   with(panel, {
      nhl    <- length(highlighted.node)
      hlight <- (!any(is.na(highlighted.node)) && 
                    (highlighted.node[1] > 1 | nhl > 1))
      if (hlight) mdl <- models[[highlighted.node[1]]]
      if (hlight & (model.display == 'coefficients'))
         print(rp.coefficients(mdl, ci = ci, labels = labels.max))
      else if (hlight & (model.display == 'terms' | nhl == 2))
         print(rp.drop1(mdl))
      else {
         par(mar = c(0, 0, 0, 0) + 0.1, bg = bgdcol)
         plot(c(0, 1), c(0, 1), type = "n",
              xlab = "", ylab = "", axes = FALSE)
      }
   })
   panel
}

rp.lmsmall.redraw <- function(panel) {
   rp.tkrreplot(panel, plot)
   rp.tkrreplot(panel, fplot)
   rp.tkrreplot(panel, modelnodes)
   panel
}

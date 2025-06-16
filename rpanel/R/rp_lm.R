#     A general function for linear models

rp.lm <- function(x, ylab, xlab, zlab,
                  panel = TRUE, panel.plot = TRUE, uncertainty.display = 'density',
                  hscale = 1, vscale = hscale,
                  inference = 'coefficients', ci = TRUE,
                  display.model, comparison.model, residuals.showing, ...) {
   
   static <- !panel
   if (missing(display.model)) display.model <- NULL
   if (missing(comparison.model)) comparison.model <- NULL
   if (!is.null(comparison.model) & is.null(display.model))
      stop('if comparison.model is specified then display.model must also be specified')
   if (missing(residuals.showing)) residuals.showing <- FALSE
   estcol <- '#86B875'
   refcol <- '#E495A5'
   style  <- if (requireNamespace('ggplot2', quietly = TRUE)) 'ggplot' else 'standard'
   
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
   if (length(numeric.ind) + length(factor.ind) > 2) {
      if (inference == 'coefficients') return(rp.coefficients(model))
      if (inference == 'terms') return(drop1(model))
   }

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
   if (missing(zlab)) zlab <- zterm
   # if (length(numeric.ind) == 2 & length(factor.ind) == 0 & length(xlab) == 1)
   #    stop('xlab is of length 1 but length 2 is needed.')

   # Identify type of model
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

   # Simple linear regression: export to a dedicated function
   if (type == 'regression.one')
      return(rp.regression(x, y, panel = panel, xlab = xlab, ylab = ylab))
   
   # Set up the model nodes
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
                                comparison1 = 2, comparison2 = 1)
   if (type == 'regression.two') {
      model.nodes   <- model.nodes[1:4, ]
      model.nodes$y <- 0.85 - c(0, 1, 1, 2) * 0.35
      # Set up the 3D plot
      ngrid  <- 20
      xlo   <- min(x) - 0.05 * diff(range(x))
      xhi   <- max(x) + 0.05 * diff(range(x))
      ylo   <- min(y) - 0.05 * diff(range(y))
      yhi   <- max(y) + 0.05 * diff(range(y))
      zlo   <- min(z) - 0.05 * diff(range(z))
      zhi   <- max(z) + 0.05 * diff(range(z))
      xgrid <- seq(xlo, xhi, length = ngrid)
      zgrid <- seq(zlo, zhi, length = ngrid)
      smatx <- matrix(rep(xgrid, ngrid), ncol = ngrid)
      smatz <- t(matrix(rep(zgrid, ngrid), ncol = ngrid))
      smat  <- array(c(mean(y, na.rm = TRUE) + 0 * smatx,
                       coef(lm(y ~ x))[1] + coef(lm(y ~ x))[2] * smatx,
                       coef(lm(y ~ z))[1] + coef(lm(y ~ z))[2] * smatz,
                       coef(lm(y ~ x + z))[1] + coef(lm(y ~ x + z))[2] * smatx +
                          coef(lm(y ~ x + z))[3] * smatz),
                     dim = c(ngrid, ngrid, 4))
      fv <- matrix(c(fitted(lm(y ~ 1)), fitted(lm(y ~ x)),
                     fitted(lm(y ~ z)), fitted(lm(y ~ x + z))), ncol = 4)
      dimnames(smat) <- list(NULL, NULL, model.nodes$label)
      dimnames(fv)   <- list(NULL, model.nodes$label)
      ylo     <- min(ylo, smat)
      yhi     <- max(yhi, smat)
      ylim    <- c(ylo, yhi)
      scaling <- rp.plot3d(x, y, z, xlab = xlab, ylab = ylab,
                           zlab = zlab, ylim = ylim, col = 'red')
      current.model <- 'None'
   }
   
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
   
   # Check that the display.model and comparison.model formula are submodels of the maximal model
   terms.max <- attr(terms(model.max), 'term.labels')
   if (!is.null(display.model)) {
      terms.display <- attr(terms(display.model), 'term.labels')
      if (!all(terms.display %in% terms.max))
         stop('display.model is not a valid model.')
   }
   if (!is.null(comparison.model)) {
      terms.comparison <- attr(terms(comparison.model), 'term.labels')
      if (!all(terms.comparison %in% terms.max))
         stop('comparison.model is not a valid model.')
   }
   
   # Create a list of valid models
   # and, for anova, estimates and se's for comparisons.
   # Use this to define the plotted response range.
   models  <- list()
   nmodels <- switch(type, regression.two = 4, ancova = 5,
                           one.way = 2, two.way = 5)
   for (i in 1:nmodels)
      models[[i]] <- update(model, model.nodes$label[i])
   if (type %in% c('one.way', 'two.way')) {
      model.est <- list()
      comp.est  <- list()
      comp.se   <- list()
      for (i in 1:nmodels) {
         mdlm   <- models[[i]]
         mdl    <- models[[model.nodes$comparison1[i]]]
         mdl0   <- models[[model.nodes$comparison2[i]]]
         lst    <- if (type == 'two.way') list(x, z)
                   else list(x)
         ngps    <- nrow(unique(data.frame(x, z)))
         df0     <- mdl0$df.residual
         df1     <- mdl$df.residual
         model.est[[i]] <- tapply(fitted(mdlm), lst, mean)
         comp.est[[i]]  <- tapply(fitted(mdl0), lst, mean)
         comp.se[[i]]   <- tapply(fitted(mdl0), lst, length)
         comp.se[[i]]   <- summary(mdl)$sigma * sqrt(abs(df0 - df1)) /
                              sqrt(comp.se[[i]] * ngps)
      }
      response.range <- range(y, unlist(model.est),
                              unlist(comp.est) + 3 * unlist(comp.se),
                              unlist(comp.est) - 3 * unlist(comp.se),
                              na.rm = TRUE)
   }
   else {
      model.est <- NULL
      comp.est  <- NULL
      comp.se  <- NULL
      response.range <- range(y)
   }
   
   # Linear regression with two covariates
   # if (length(numeric.ind) == 2 & length(factor.ind) == 0)
   #    return(rp.regression2.lm(y, x, z,
   #                             yterm = yterm, x1term = xterm, x2term = zterm,
   #                             ylab = ylab, x1lab = xlab, x2lab = zlab,
   #                             panel = panel, models = models, title = ttl,
   #                             display = display.model,
   #                             residuals.showing = residuals.showing))
      
   if (panel) {
      pnl <- rp.control(ttl, models = models, y = y, x = x, z = z,
                        model.est = model.est, comp.est = comp.est, comp.se = comp.se,
                        response.range = response.range,
                        jitter.x = jitter.x, uncertainty.display = uncertainty.display,
                        type = type, style = style, labels.max = labels.max,
                        xlab = xlab, ylab = ylab, zlab = zlab,
                        yterm = yterm, xterm = xterm, zterm = zterm,
                        ci = ci, bgdcol = bgdcol, estcol = estcol, refcol = refcol,
                        highlighted.node = NA, static = static,
                        model.nodes = model.nodes, click.coords = rep(NA, 2),
                        residuals.showing = residuals.showing,
                        current.model = current.model, scaling = scaling,
                        smat = smat, fv = fv, xgrid = xgrid, zgrid = zgrid)
      rp.menu(pnl, model.display,
              list(c('Inference', 'none', 'coefficients', 'terms')),
              initval = 'none', action = rp.lm.redraw)
      rp.grid(pnl, "models", row = 0, column = 0, background = bgdcol)
      rp.tkrplot(pnl, modelnodes, rp.lm.modelnodes, action = rp.lm.click,
                 hscale = 0.7 * hscale, vscale = 0.5 * vscale, 
                 grid = "models", row = 0, column = 0, background = "white")
      
      if (panel.plot) {
         rp.grid(pnl, "dataplot", row = 0, column = 1, background = "white")
         if (type != 'regression.two')
            rp.tkrplot(pnl, plot, rp.lm.draw,
                  hscale = hscale, vscale = vscale, 
                  grid = "dataplot", row = 0, column = 0, background = "white")
         else
            rp.checkbox(pnl, residuals.showing, rp.regression2.residuals, "Show residuals",
                        grid = "models", row = 1, column = 0)
         
         # rp.listbox(pnl, analysis, c('none', 'coefficients', 'terms'),
                    # title = 'analysis',
                    # grid = "models", row = 1, column = 0, background = "white")
         rp.tkrplot(pnl, fplot, rp.lm.effectsplot,
                    hscale = hscale * 0.7, vscale = vscale * 0.5, 
                    grid = "models", row = 2, column = 0, background = bgdcol)
         action.fn <- rp.lm.redraw
      }
      else {
         # This needs to be amended to handle the effects plot
         action.fn <- rp.lm.draw
         rp.text(pnl, "        Model", grid = "models", row = 0, column = 1, background = bgdcol)
         rp.text(pnl,       "current", grid = "models", row = 1, column = 0, background = bgdcol)
         rp.text(pnl,           "new", grid = "models", row = 1, column = 2, background = bgdcol)
         rp.checkbox(pnl, model11, action.fn, labels = "", initval = init.model[1],
                     grid = "models", row = 2, column = 0, name = "model11", background = bgdcol)
         rp.checkbox(pnl, model12, action.fn, labels = "", initval = init.model[2],
                     grid = "models", row = 3, column = 0, name = "model12", background = bgdcol)
         for (i in 1:length(model.options)) rp.text(pnl, model.options[i],
                                                    grid = "models", row = i + 1, column = 1,
                                                    background = bgdcol)
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
      # pnl <- list(x = x, y = y, z = z, xlab = xlab, ylab = ylab,
      #             xterm = xterm, zterm = zterm, term.names = term.names, 
      #             style = style, bgdcol = bgdcol)
      pnl <- list(ttl, models = models, y = y, x = x, z = z,
                  model.est = model.est, comp.est = comp.est, comp.se = comp.se,
                  response.range = response.range,
                  jitter.x = jitter.x, uncertainty.display = uncertainty.display,
                  type = type, style = style, labels.max = labels.max,
                  xlab = xlab, ylab = ylab, zlab = zlab,
                  yterm = yterm, xterm = xterm, zterm = zterm,
                  ci = ci, bgdcol = bgdcol, estcol = estcol, refcol = refcol,
                  highlighted.node = NA, static = static,
                  model.nodes = model.nodes, click.coords = rep(NA, 2),
                  display.model = display.model, comparison.model = comparison.model,
                  current.model = current.model, scaling = scaling,
                  smat = smat, fv = fv, xgrid = xgrid, zgrid = zgrid)
      rp.lm.draw(pnl)
   }
   
   invisible(pnl)
   
}

rp.lm.modelnodes <- function(panel) {
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

rp.lm.click <- function(panel, x, y) {
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
   rp.tkrreplot(panel, fplot)
   if (panel$type != 'regression.two')
      rp.tkrreplot(panel, plot)
   else
      panel <- rp.lm.draw(panel)
   panel
}

rp.lm.draw <- function(panel) {
   
   if (panel$static) {
      fn <- function(x, trms) {
         mt <- attr(terms(as.formula(x)), 'term.labels')
         (length(trms) == length(mt)) & all(trms %in% mt)
      }
      if (!is.null(panel$display.model)) {
         terms.display <- attr(terms(panel$display.model), 'term.labels')
         terms.model   <- sapply(panel$model.nodes$label, fn, terms.display)
         hlight <- which(terms.model)
      }
      else
         hlight <- panel$highlighted.node
      if (!is.null(panel$comparison.model)) {
         terms.comparison <- attr(terms(panel$comparison.model), 'term.labels')
         terms.model      <- sapply(panel$model.nodes$label, fn, terms.comparison)
         comp.ind         <- c(hlight, which(terms.model))
         fn <- function(x) all(comp.ind %in% x)
         comps    <- as.matrix(panel$model.nodes[ , c('comparison1', 'comparison2')])
         comp.ind <- which(apply(comps, 1, fn))
         if (length(comp.ind) == 0) stop(paste('only adjacent models can be compared ',
            'in this function.\n  Use the anova function to perform more general comparisons.'))
         # The 1 below deals with the one.way case
         hlight   <- comps[comp.ind[1], ]
      }
   }
   else
      hlight <- panel$highlighted.node

   # Two covariates
   if (panel$type == 'regression.two') {
      with(panel, {
         if (current.model != "None") {
            rgl::pop3d()
            if (residuals.showing) rgl::pop3d()
         }
         if (!any(is.na(hlight))) {
            a <- scaling(xgrid, smat[ , , hlight[1]], zgrid)
            rgl::surface3d(x = a$x, z = a$z, y = a$y, alpha = 0.5)
            if (residuals.showing) {
               a <- scaling(c(t(cbind(x, x))), c(t(cbind(y, fv[ , hlight[1]]))),
                            c(t(cbind(z, z))))
               rgl::segments3d(a$x, a$y, a$z, col = "green")
            }
         }
      })
      if (!any(is.na(hlight))) panel$current.model <- hlight[1]
   }
   
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
      dfrm <- data.frame(x = panel$x, y = panel$y, jitter.x = panel$jitter.x)
      if (panel$type == 'two.way') dfrm$z <- panel$z
      plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(y, x)) +
         ggplot2::xlab(panel$ylab) + ggplot2::ylab(panel$xlab) +
         ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.background = ggplot2::element_rect(fill = "grey90")) +
         ggplot2::ggtitle(panel$ttl)
      ngrid <- 512
      xgrid <- seq(panel$response.range[1], panel$response.range[2], length = ngrid)
      del   <- diff(panel$response.range) / ngrid
      # Plot the data density
      if (length(hlight) != 2) {
         lst   <- if (panel$type == 'two.way') list(panel$x, panel$z) else list(panel$x)
         fn    <- function(x) {
            bw   <- if (length(x) > 1) bw.norm(x) else diff(panel$response.range) / 8
            dens <- density(x, bw = bw, n = ngrid,
                            from = panel$response.range[1] + del,
                            to   = panel$response.range[2] - del)
            cbind(dens$x, dens$y)
         }
         dgrid <- tapply(panel$y, lst, fn)
         xdens <- c(sapply(dgrid, function(x) x[ , 1]))
         ydens <- c(sapply(dgrid, function(x) x[ , 2]))
         dfrm0 <- data.frame(xdens, ydens, x = rep(levels(panel$x), each = ngrid))
         if (panel$type == 'two.way')
            dfrm0$z = rep(levels(panel$z), each = nlevels(panel$x) * 512)
         plt <- plt + ggplot2::geom_tile(ggplot2::aes(x = xdens, y = x,
                                                      fill = ydens, height = 0.8),
                                         data = dfrm0, show.legend = FALSE) +
         # This creates missing tiles - I don't know why
         # plt <- plt + ggplot2::stat_density(ggplot2::aes(fill   = ggplot2::after_stat(density)),
         #                                    geom = "tile", trim = TRUE,
         #                                    height = 0.7, position = "identity",
         #                                    show.legend = FALSE) +
            ggplot2::scale_fill_gradient(low = 'grey90', high = 'grey50')
         # geom_violin doesn't look good with small samples
         # plt <- plt + ggplot2::geom_violin(bw = 'nrd', col = NA, fill = 'grey80')
      }
      # plot the model comparison uncertainties
      else if (length(hlight) == 2) {
         fn       <- function(x) all(hlight %in% x)
         comps    <- as.matrix(panel$model.nodes[ , c('comparison1', 'comparison2')])
         comp.ind <- which(apply(comps, 1, fn))
         est      <- panel$comp.est[[comp.ind]]
         se       <- panel$comp.se[[comp.ind]]
         ngrid    <- 500
         xgrid    <- seq(panel$response.range[1] + del, panel$response.range[2] - del,
                         length = ngrid)
         if (panel$type == 'two.way') {
            dfrm1 <- data.frame(y = c(est), x = rep(rownames(est), ncol(est)),
                                z = rep(colnames(est), each = nrow(est)))
            dfrm1 <- data.frame(xgrid = rep(xgrid, each = nrow(dfrm1)),
                                x = rep(dfrm1$x, ngrid), z = rep(dfrm1$z, ngrid))
            dfrm1$dgrid <- dnorm(dfrm1$xgrid, est[cbind(dfrm1$x, dfrm1$z)],
                                              se[cbind(dfrm1$x, dfrm1$z)])
         }
         else {
            dfrm1 <- data.frame(y = est, x = names(est))
            dfrm1 <- data.frame(xgrid = rep(xgrid, each = nrow(dfrm1)),
                                x = rep(dfrm1$x, ngrid))
            dfrm1$dgrid <- dnorm(dfrm1$xgrid, est[dfrm1$x], se[dfrm1$x])
         }
         if (panel$uncertainty.display == 'shading') {
            plt <- plt + ggplot2::geom_tile(ggplot2::aes(x = xgrid, y = x,
                                            fill = dgrid, height = 0.8),
                                            data = dfrm1, show.legend = FALSE) +
               ggplot2::scale_fill_gradient(low = "grey90", high = panel$refcol)
         }
         else {
            dfrm1$dgrid <- 0.8 * dfrm1$dgrid / dnorm(0, 0, min(se))
            plt <- plt + ggplot2::geom_tile(ggplot2::aes(x = xgrid, y = x, height = dgrid),
                                            col = NA, fill = panel$refcol,
                                            show.legend = FALSE, data = dfrm1)
         }
      }
      if (!any(is.na(hlight))) {
         mdl <- panel$models[[hlight[1]]]
         afx <- as.numeric(factor(dfrm$x))
         plt <- plt + ggplot2::geom_segment(ggplot2::aes(x    = fitted(mdl),
                                                         y    = afx - 0.45,
                                                         yend = afx + 0.45),
                                            linewidth = 1, col = panel$estcol)
         print(plt)
      }
      plt  <- plt +
         ggplot2::geom_point(ggplot2::aes(x = y, y = jitter.x), col = clr[3]) +
         ggplot2::xlim(panel$response.range[1], panel$response.range[2])
      plt  <- plt + ggplot2::coord_flip()
      if (panel$type == "two.way")
         plt <- plt + ggplot2::facet_grid(. ~ z)
      print(plt)
   }

   panel
}

rp.lm.effectsplot <- function(panel) {
   with(panel, {
      nhl    <- length(highlighted.node)
      hlight <- (!any(is.na(highlighted.node)) && 
                 (highlighted.node[1] > 1 | nhl > 1))
      if (hlight) mdl <- models[[highlighted.node[1]]]
      if (hlight & (model.display == 'coefficients'))
         print(rp.coefficients(mdl, ci = ci, labels = labels.max))
      else if (hlight & (model.display == 'terms') & nhl == 2) {
         mdl0 <- models[[highlighted.node[2]]]
         r0   <- rownames(anova(mdl0))
         r1   <- rownames(anova(mdl))
         trm  <- r1[!(r1 %in% r0)]
         print(rp.drop1(mdl, trm))
      } else {
         par(mar = c(0, 0, 0, 0) + 0.1, bg = bgdcol)
         plot(c(0, 1), c(0, 1), type = "n",
              xlab = "", ylab = "", axes = FALSE)
      }
   })
   panel
}

rp.regression2.residuals <- function(panel) {
   with(panel, {
      if (length(highlighted.node) > 0) {
         if (residuals.showing) {
            mdl <- model.nodes$label[highlighted.node[1]]
            a <- scaling(c(t(cbind(x, x))), c(t(cbind(y, fv[ , mdl]))),
                         c(t(cbind(z, z))))
            rgl::segments3d(a$x, a$y, a$z, col = "green")
         }
         else
            rgl::pop3d()
      }
   })
   panel
}

rp.lm.redraw <- function(panel) {
   if (panel$type != 'regression.two')
      rp.tkrreplot(panel, plot)
   else
      panel <- rp.lm.draw(panel)
   rp.tkrreplot(panel, fplot)
   rp.tkrreplot(panel, modelnodes)
   panel
}

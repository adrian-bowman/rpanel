#   ANCOVA

rp.ancova <- function(x, y, group, panel = TRUE, panel.plot = TRUE,
                      model = NA, model0 = NA, xlab, ylab, glab, ci = TRUE,
                      hscale = NA, vscale = hscale, style = "ggplot") {
                    
   if(missing(x) || missing(y) || missing(group))
      stop('rp.ancova requires x, y, and group.')

   if (style == 'ggplot' & !requireNamespace('ggplot2', quietly = TRUE))
      stop('the ggplot2 package is not available.')
   
   xterm <- deparse(substitute(x))
   yterm <- deparse(substitute(y))
   zterm <- deparse(substitute(group))
   if (missing(xlab)) xlab <- xterm
   if (missing(ylab)) ylab <- yterm
   if (missing(glab)) glab <- zterm
      
   group <- factor(group)
   ind   <- !is.na(x + y + as.numeric(group))
   x     <- x[ind]
   y     <- y[ind]
   group <- group[ind]
         
   if (is.na(hscale)) hscale <- 1
   if (is.na(vscale)) vscale <- hscale
      
   if (style != "old") rp.ancova.new(x, y, group, panel = panel, panel.plot = panel.plot,
                           model = model, model0 = model0, xlab = xlab, ylab = ylab, glab = glab,
                           ci = ci, xterm = xterm, yterm = yterm, zterm = zterm,
                           hscale = hscale, vscale = vscale, style = style)
   else rp.ancova.old(x, y, group, panel = panel, panel.plot = panel.plot,
                          model = "None", xlab = xlab, ylab = ylab,
                          hscale = hscale, vscale = vscale)

      invisible()
}

rp.ancova.new <- function(x, y, group, panel = TRUE, panel.plot = TRUE,
                          model = NA, model0 = NA,
                          xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
                          glab = deparse(substitute(group)), ci = ci,
                          xterm = xterm, yterm = yterm, zterm = zterm,
                          hscale = NA, vscale = hscale, style = 'ggplot') {

   model.options <- c("overall mean", xterm, zterm, paste(xterm, ":", zterm))
   term.names    <- model.options[-1]
   init.model    <- model
   init.model0   <- model0
   if (any(is.na(init.model)))  init.model  <- rep(FALSE, 4)
   if (any(is.na(init.model0))) init.model0 <- rep(FALSE, 4)
   names(init.model) <- model.options
   bgdcol <- "grey85"
   
   dfrm  <- data.frame(y, x, group)
   names(dfrm) <- c("y", term.names[1:2])
   form <- "y ~ 1"
   for (i in 1:length(term.names))
      form <- paste(form, term.names[i], sep = " + ")
   model.full <- lm(as.formula(form), na.action = na.exclude, x = TRUE, data = dfrm)
   coef.names <- names(coefficients(model.full))

   if (panel) {
      pnl <- rp.control("Analysis of covariance", 
                    x = x, y = y, z = factor(group), xlab = xlab, ylab = ylab, glab = glab,
                    xterm = xterm, zterm = zterm, term.names = term.names,
                    coef.names = coef.names, ci = ci, coef.names = coef.names,
                    model11 = init.model[1],  model12 = init.model[2],  model13 = init.model[3], 
                    model14 = init.model[4],  model01 = init.model0[1], model02 = init.model0[2],
                    model03 = init.model0[3], model04 = init.model0[4],
                    model = init.model, model0 = init.model0, style = style,
                    # model.check = any(init.model), model0.check = any(init.model0),
                    bgdcol = bgdcol)
      rp.do(pnl, rp.ancova.fit)
      
      rp.grid(pnl, "controls", row = 0, column = 0, background = bgdcol)
      rp.grid(pnl, "models", grid = "controls", row = 0, column = 0, background = bgdcol)
      rp.grid(pnl, "fplot",  grid = "controls", row = 1, column = 0, background = bgdcol)

      if (panel.plot) {
         rp.grid(pnl, "dataplot", row = 0, column = 1, background = "white")
         rp.tkrplot(pnl, plot, rp.ancova.draw,  hscale = hscale, vscale = vscale, 
                    grid = "dataplot", row = 0, column = 0, background = "white")
         rp.text(pnl, "", grid = "fplot", row = 0, column = 0, background = bgdcol)
      	# rp.text(pnl, "", grid = "fplot", row = 1, column = 0, background = bgdcol)
      	# rp.tkrplot(pnl, fplot, rp.ancova.fplot, hscale = hscale * 0.7, vscale = vscale * 0.2, 
      	#           grid = "fplot", row = 2, column = 0, background = bgdcol)
      	rp.tkrplot(pnl, fplot, rp.ancova.effectsplot,
      	           hscale = hscale * 0.7, vscale = vscale * 0.7, 
      	           grid = "fplot", row = 2, column = 0, background = bgdcol)
         # rp.text(pnl, "", grid = "fplot", row = 3, column = 0, background = bgdcol)
         action.fn <- rp.ancova.redraw
      }
      else
         action.fn <- rp.ancova.draw

      # rp.text(pnl, "        Model", grid = "models", row = 0, column = 1, background = bgdcol)
      # rp.text(pnl,       "current", grid = "models", row = 1, column = 0, background = bgdcol)
      # rp.text(pnl,           "new", grid = "models", row = 1, column = 2, background = bgdcol)
      # rp.checkbox(pnl, model11, action.fn, labels = "", initval = init.model[1],
      #       grid = "models", row = 2, column = 0, name = "model11", background = bgdcol)
      # rp.checkbox(pnl, model12, action.fn, labels = "", initval = init.model[2],
      #       grid = "models", row = 3, column = 0, name = "model12", background = bgdcol)
      # for (i in 1:length(model.options)) rp.text(pnl, model.options[i], 
      #       grid = "models", row = i + 1, column = 1, background = bgdcol)
      # rp.checkbox(pnl, model01, action.fn, labels = "", initval = init.model0[1],
      #       grid = "models", row = 2, column = 2, name = "model01", background = bgdcol)
      # rp.checkbox(pnl, model02, action.fn, labels = "", initval = init.model0[1],
      #       grid = "models", row = 3, column = 2, name = "model02", background = bgdcol)
      # 
      # # rp.checkbox(pnl, model, action.fn, rep("", length(model.options)), title = "current", 
      # #      initval = init.model, grid = "controls", row = 1, column = 0, background = bgdcol)
      # rp.checkbox(pnl, model13, action.fn, labels = "", initval = init.model[3],
      #       grid = "models", row = 4, column = 0, name = "model13", background = bgdcol)
      # rp.checkbox(pnl, model14, action.fn, labels = "", initval = init.model[4],
      #       grid = "models", row = 5, column = 0, name = "model14", background = bgdcol)
      # # rp.grid(pnl, "models", row = 1, column = 1, grid = "controls")
      # rp.checkbox(pnl, model03, action.fn, labels = "", initval = init.model0[1],
      #       grid = "models", row = 4, column = 2, name = "model03", background = bgdcol)
      # rp.checkbox(pnl, model04, action.fn, labels = "", initval = init.model0[1],
      #       grid = "models", row = 5, column = 2, name = "model04", background = bgdcol)
      # rp.do(pnl, action.fn)
   }
   else {
      pnl <- list(x = x, y = y, z = factor(group), xlab = xlab, ylab = ylab,
                  xterm = xterm, zterm = zterm, term.names = term.names, 
                  model11 = init.model[1],  model12 = init.model[2],  model13 = init.model[3], 
                  model14 = init.model[4],  model01 = init.model0[1], model02 = init.model0[2],
                  model03 = init.model0[3], model04 = init.model0[4], style = style,
                  model.check = TRUE, model0.check = TRUE, bgdcol = bgdcol)
      rp.ancova.draw(pnl)
   }
      
   invisible()
   
}

rp.ancova.fit <- function(panel) {
   
   panel$model  <- c(panel$model11, panel$model12, panel$model13, panel$model14)
   panel$model0 <- c(panel$model01, panel$model02, panel$model03, panel$model04)
   panel$model.check  <- TRUE
   panel$model0.check <- TRUE
   if (!panel$model[1] & any(panel$model[-1])) {
      rp.messagebox("The overall mean must be included if other terms are present.")
      panel$model.check <- FALSE
   }
   if (panel$model[4] & !all(panel$model[2:3])) {
      rp.messagebox("The main effects must be included if the interaction term is present.")
      panel$model.check <- FALSE
   }
   if (!panel$model0[1] & any(panel$model0[-1])) {
      rp.messagebox("The overall mean must be included if other terms are present",
                    "in the new model.")
      panel$model0.check <- FALSE
   }
   if (panel$model0[4] & !all(panel$model0[2:3])) {
      rp.messagebox("The main effects must be included if the interaction term",
                    "is present in the new model.")
      panel$model0.check <- FALSE
   }

   x     <- panel$x
   y     <- panel$y
   z     <- panel$z
   dfrm  <- data.frame(y, x, z)
   names(dfrm) <- c("y", panel$term.names[1:2])
   
   form <- paste('y ~ ', paste(panel$term.names, collapse = ' + '))
   mdl.max <- lm(as.formula(form), na.action = na.exclude, data = dfrm)
   panel$labels.max <- names(mdl.max$coefficients[-1])

   if (panel$model.check) {
     form <- "y ~ 1"
     trms <- panel$term.names[panel$model[-1]]
     if (length(trms) > 0) {
       for (i in 1:length(trms))
         form <- paste(form, trms[i], sep = " + ")
       form <- sub('1 + ', '', form, fixed = TRUE)
     }
     mdl1        <- lm(as.formula(form), na.action = na.exclude, x = TRUE, data = dfrm)
     panel$mdl1  <- mdl1
     panel$form1 <- form
     panel$df1   <- mdl1$df.residual
     panel$sigma <- summary(mdl1)$sigma
     rss1        <- sum(mdl1$residuals^2)
   }

   if (panel$model0.check) {
     form <- "y ~ 1"
     trms <- panel$term.names[panel$model0[-1]]
     if (length(trms) > 0)
       for (i in 1:length(trms))
         form <- paste(form, trms[i], sep = " + ")
     mdl0       <- lm(as.formula(form), na.action = na.exclude, data = dfrm)
     panel$mdl0 <- mdl0
     panel$df0  <- mdl0$df.residual
     rss0       <- sum(mdl0$residuals^2)
   }

   if (panel$model.check & panel$model0.check)
      panel$fstat <- (abs(rss0 - rss1) / abs(panel$df1 - panel$df0)) / panel$sigma^2
   else
      panel$fstat <- NULL
   
   panel
}
 
rp.ancova.draw <- function(panel) {
      
   if (panel$style == "ggplot") {
      dfrm <- data.frame(x = panel$x, y = panel$y, z = panel$z, fvals = fitted(panel$mdl1))
      plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y, group = z, col = z)) +
         ggplot2::geom_point() +
         ggplot2::labs(x = panel$xlab, y = panel$ylab, col = panel$glab)
      if (panel$model.check & any(panel$model)) {
         if (panel$model[3]) plt <- plt + ggplot2::geom_line(ggplot2::aes(y = fvals))
         else                plt <- plt + ggplot2::geom_line(ggplot2::aes(y = fvals,
                                                             col = NULL, group = NULL))
         form <- panel$form
      }
      else
         form <- 'none'
      plt <- plt + ggplot2::ggtitle(paste("Model:", form))
      print(plt)
   }
   
   if (panel$style == "new") {
      with(panel, {
         n.groups <- length(levels(z))
         par(mar = c(5, 4, 2, 2) + 0.1)
         plot(x, y, type = "n", xlab = xlab, ylab = ylab)
         for (i in 1:n.groups)
            points(x[z == levels(z)[i]],
                   y[z == levels(z)[i]], col = i, pch = i)
         ind   <- (!is.na(x) & !is.na(y) & !is.na(z))
         x <- x[ind]
         y <- y[ind]
         z <- z[ind]
         if (model.check) {
            if (all(model == c(TRUE, FALSE, FALSE, FALSE)))
               abline(h = coef(mdl1))
            else if (all(model == c(TRUE, TRUE, FALSE, FALSE)))
               abline(coef(mdl1))
            else if (any(model)) {
               for (i in 1:n.groups) {
                  ind  <- (z == levels(z)[i])
                  xgp  <- x[ind]
                  fgp  <- fitted(mdl1)[ind]
                  ind1 <- order(xgp)
                  lines(xgp[ind1[range(ind1)]], fgp[ind1[range(ind1)]], col = i, lty = i, lwd = 2)
               }
            }
         }
      })
   }
   
   panel
}

rp.ancova.fplot <- function(panel) {
   with(panel, {
      if (model.check & model0.check & any(model) & any(model0) & !all(model == model0)) {
         clr  <- paste("grey", round(seq(100, 0, length = 101)), sep = "")
         pct  <- qf(0.99, abs(df0 - df1), min(df0, df1))
         xlim <- max(pct * 1.5, fstat * 1.1)
         grd  <- seq(0, xlim, length = 100)
         del  <- diff(grd)[1] / 2
         grd  <- grd[-1] - del
         ind  <- cut(df(grd, abs(df0 - df1), min(df0, df1)), length(clr), labels = FALSE)
         par(mar = c(1, 1, 1, 1), oma = rep(0, 4), tcl = -0.2, xaxs = "i", mgp = c(1, 0, 0))
         plot(c(0, xlim), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
         axis(1, cex.axis = 0.7)
         lines(par()$usr[1:2], rep(par()$usr[3], 2))
         rect(grd - del, 0.05, grd + del, 1, col = clr[ind], border = clr[ind])
         # denstrip(grd, df(grd, abs(df0 - df1), min(df0, df1)), 0.5, 0.9, colmax = "black")
         points(fstat, 0.525, col = "red", pch = 16)
         title(paste("p-value:", round(1 - pf(fstat, abs(df0 - df1), min(df0, df1)), 3)),
               cex.main = 0.8, font.main = 1)
      }
      else {
         par(mar = c(0, 0, 0, 0) + 0.1, bg = bgdcol)
         plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
      }
   })
   panel
}

rp.ancova.effectsplot <- function(panel) {
   with(panel, {
      if (model.check & sum(model) > 1 )
         print(rp.regression(mdl1, ci = ci, labels = labels.max))
      else {
         par(mar = c(0, 0, 0, 0) + 0.1, bg = bgdcol)
         plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
      }
   })
   panel
}

rp.ancova.redraw <- function(panel) {
   rp.do(panel, rp.ancova.fit)
   rp.tkrreplot(panel, plot)
   rp.tkrreplot(panel, fplot)
   panel
}

#-----------------------------------------------------------------------------

rp.ancova.old <- function(x, y, group, panel = TRUE, panel.plot = TRUE, model = "None",
                          xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
                          hscale = NA, vscale = hscale) {

if (any(is.na(model))) model <- "None"

rp.ancova.draw <- function(panel) {
   with(panel, {
      group    <- factor(group)
      n.groups <- length(levels(group))
      plot(x, y, type = "n", xlab = xlab, ylab = ylab)
      for (i in 1:n.groups)
         points(x[group == levels(group)[i]],
                y[group == levels(group)[i]], col = i, pch = i)
      ind   <- (!is.na(x) & !is.na(y) & !is.na(group))
      x <- x[ind]
      y <- y[ind]
      group <- group[ind]
      if      (model == "Single mean")     lm.model <- lm(y ~ 1)
      else if (model == "Single line")     lm.model <- lm(y ~ x)
      else if (model == "Parallel lines")  lm.model <- lm(y ~ group + x)
      else if (model == "Different lines") lm.model <- lm(y ~ group * x)
      title.text <- paste("Model:", model)
      if (model == "Single mean")
         abline(h = coef(lm.model))
      else if (model == "Single line")
      abline(coef(lm.model))
      else if (!(model == "None")) {
         if (model == "Parallel lines") {
           pval <- drop1(lm.model, test = "F")[["Pr(>F)"]][2]
           pval <- round(pval, 3)
           title.text <- paste(title.text, "\n", "Test of equal groups:", pval)
         }
         if (model == "Different lines") {
           pval <- drop1(lm.model, test = "F")[["Pr(>F)"]][2]
           pval <- round(pval, 3)
           title.text <- paste(title.text, "\n", "Test of parallelism:", pval)
         }
         for (i in 1:n.groups) {
            ind  <- (group == levels(group)[i])
            xgp  <- x[ind]
            fgp  <- fitted(lm.model)[ind]
            ind1 <- order(xgp)
            lines(xgp[ind1[range(ind1)]], fgp[ind1[range(ind1)]], col = i, lty = i, lwd = 2)
         }
      }
      title(title.text, cex.main = 1)
   })
   panel
}

rp.ancova.redraw <- function(panel) {
   rp.tkrreplot(panel, plot)
   panel
   }

   if (panel) {
      panel <- rp.control("One-way ancova", y = y, x = x, group = group,
                                 xlab = xlab, ylab = ylab)
      if (panel.plot) {
         rp.tkrplot(panel, plot, rp.ancova.draw, pos = "right",
                    hscale = hscale, vscale = vscale)
         action.fn <- rp.ancova.redraw
         }
      else
         action.fn <- rp.ancova.draw
      rp.radiogroup(panel, model,
         c("None", "Single mean", "Single line", "Parallel lines", "Different lines"),
         action = action.fn)
      rp.do(panel, action.fn)
      }
   else {
      panel <- list(x = x, y = y, group = group, xlab = xlab, ylab = ylab, model = model)   
      rp.ancova.draw(panel)
      invisible()
      }
   invisible()
}

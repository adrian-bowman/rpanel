#     A general function for linear models

rp.lmsmall <- function(x, panel = TRUE, xlab, ylab, glab, ...) {
   
   # Deal with formula or model inputs
   if ('formula' %in% class(x))
      model <- lm(x, ...)
   else if ('lm' %in% class(x))
      model <- x
   else
      stop('x is not a formula or a linear model object.')
   if (!('model' %in% names(model))) {
      model <- update(model, model = TRUE)
      message("model refitted with 'model = TRUE'.")
   }

   mf           <- model$model
   response.ind <- attr(model$terms, 'response')
   var.types    <- attr(model$terms, 'dataClasses')
   numeric.ind  <- which(var.types == 'numeric')
   numeric.ind  <- numeric.ind[numeric.ind != response.ind]
   factor.ind   <- which(var.types == 'factor')
   
   # Variable labels
   if (missing(xlab)) xlab <- names(var.types)[numeric.ind]
   if (missing(ylab)) ylab <- names(var.types)[response.ind]
   if (missing(glab)) glab <- names(var.types)[factor.ind]
   # Deal with xlab specified but only one variable for regression with two
   if (length(numeric.ind) == 2 & length(factor.ind) == 0 & length(xlab) == 1)
      stop('xlab is of length 1 but length 2 is needed.')
      
   if (length(numeric.ind) + length(factor.ind) > 2)
      stop('this function deals with no more than two predictor variables.')
   
   # Simple linear regression
   if (length(numeric.ind) == 1 & length(factor.ind) == 0)
      return(rp.regression(mf[ , numeric.ind], mf[ , response.ind],
                           panel = panel, xlab = xlab, ylab = ylab))

   # Linear regression with two covariates
   if (length(numeric.ind) == 2 & length(factor.ind) == 0)
      return(rp.regression2(mf[ , response.ind],
                           mf[ , numeric.ind[1]], mf[ , numeric.ind[2]],
                           x1lab = xlab[1], x2lab = xlab[2], ylab  = ylab,
                           panel = panel))
   
   # Analysis of covariance (one factor and one covariate)
   if (length(numeric.ind) == 1 & length(factor.ind) == 1)
      return(rp.ancova(mf[ , numeric.ind], mf[ , response.ind],
                       mf[ , factor.ind],
                       xlab  = xlab, ylab  = ylab, glab  = glab,
                       panel = panel))

   # One-way anova
   if (length(numeric.ind) == 0 & length(factor.ind) == 1)
      return(rp.anova(mf[ , response.ind],
                      mf[ , factor.ind],
                      xlab = names(var.types)[factor.ind[1]],
                      ylab = names(var.types)[response.ind],
                      panel = panel))
   
   # Two-way anova
   if (length(numeric.ind) == 0 & length(factor.ind) == 2)
      return(rp.anova(mf[ , response.ind],
                      mf[ , factor.ind[1]],
                      mf[ , factor.ind[2]],
                      ylab = names(var.types)[response.ind],
                      xlab = names(var.types)[factor.ind[1]],
                      zlab = names(var.types)[factor.ind[2]],
                      panel = panel))
   
}
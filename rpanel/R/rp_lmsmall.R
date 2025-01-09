#     A general function for linear models

rp.lmsmall <- function(x, panel = TRUE, ...) {
   
   # Deal with formula or model inputs
   if ('formula' %in% class(x))
      model <- lm(x, ...)
   else if ('lm' %in% class(x))
      model <- x
   else
      stop('x is not a formula or a linear model object.')
   if (!('model' %in% names(model)))
         stop("the model object should be created with 'model = TRUE'.")
   
   mf           <- model$model
   response.ind <- attr(model$terms, 'response')
   var.types    <- attr(model$terms, 'dataClasses')
   numeric.ind  <- which(var.types == 'numeric')
   numeric.ind  <- numeric.ind[numeric.ind != response.ind]
   factor.ind   <- which(var.types == 'factor')
   
   if (length(numeric.ind) + length(factor.ind) > 2)
      stop('this function deals with no more than two predictor variables.')
   
   # Simple linear regression
   if (length(numeric.ind) == 1 & length(factor.ind) == 0)
      return(rp.regression(mf[ , numeric.ind], mf[ , response.ind], panel = panel,
                           xlab = names(var.types)[numeric.ind],
                           ylab = names(var.types)[response.ind]))

   # Linear regression with two covariates
   if (length(numeric.ind) == 2 & length(factor.ind) == 0)
      return(rp.regression2(mf[ , response.ind],
                           mf[ , numeric.ind[1]], mf[ , numeric.ind[2]],
                           x1lab = names(var.types)[numeric.ind[1]],
                           x2lab = names(var.types)[numeric.ind[2]],
                           ylab  = names(var.types)[response.ind],
                           panel = panel))
   
   # Analysis of covariance (one factor and one covariate)
   if (length(numeric.ind) == 1 & length(factor.ind) == 1) {
      xname <- names(var.types)[numeric.ind]
      yname <- names(var.types)[response.ind]
      gname <- names(var.types)[factor.ind]
      assign(xname, mf[ , numeric.ind])
      assign(yname, mf[ , response.ind])
      assign(gname, mf[ , factor.ind])
      return(rp.ancova(eval(parse(text = xname)),
                       eval(parse(text = yname)),
                       eval(parse(text = gname)),
                       xlab  = names(var.types)[numeric.ind],
                       ylab  = names(var.types)[response.ind],
                       glab  = names(var.types)[factor.ind],
                       panel = panel))
   }
   
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
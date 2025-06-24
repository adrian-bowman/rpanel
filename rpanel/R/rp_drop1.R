#     Anova on a regression model

rp.drop1 <- function(model, subset.terms, p.reference = c(0.05, 0.01), col) {
   
   tbl     <- drop1(model, test = 'F')[-1, ]
   # if (!missing(subset.terms) | is.null(subset.terms)) {
   if (!missing(subset.terms)) {
      if ((is.character(subset.terms)))
         subset.terms <- match(subset.terms, rownames(tbl))
      tbl <- tbl[subset.terms, ]
   }
   if (missing(col)) col <- '#FFB56B'
   tbl$df  <- tbl$Df
   tbl$Df  <- paste('Model terms with', tbl$Df, 'df')
   fmax    <- 1.1 * max(qf(1 - p.reference, tbl$df, model$df.residual), tbl$'F value')
   ngrid   <- 100
   fgrid   <- seq(0, fmax, length = ngrid)
   fht     <- numeric(0)
   udf     <- unique(tbl$df)
   for (degf in udf)
     fht <- c(fht, df(fgrid, degf, model$df.residual))
   maxfht  <- max(fht[!is.infinite(fht)])
   fht     <- fht * 0.75 / maxfht
   fhtlo   <- -0.75
   fhthi   <-  0.75
   fht     <- fhtlo + fht * (fhthi - fhtlo) / maxfht
   p.reference <- p.reference[!is.null(p.reference)]
   p.reference <- p.reference[!is.na(p.reference)]
   qvals   <- if (length(p.reference) == 0) 0.95 else 1 - p.reference
   curv.df <- data.frame(fht, fgrid = rep(fgrid, length(udf)),
                         Df = paste('Model terms with', rep(udf, each = ngrid), 'df'))
      
   plt <- ggplot2::ggplot(tbl, ggplot2::aes(`F value`, rownames(tbl))) +
      ggplot2::geom_ribbon(ggplot2::aes(x = fgrid, ymin = fhtlo, ymax = fht),
                           fill = col, col = col,
                           inherit.aes = FALSE, data = curv.df) +
      ggplot2::geom_point() +
      ggplot2::ylab('Model terms') +
      ggplot2::xlim(0, fmax) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank())
   
   # Add a p-value scale and mark reference points
   if (!is.null(p.reference)) {
      ypos <- seq(0.8, 0.9, length = length(p.reference))
      p.df <- data.frame(Df = paste('Model terms with', rep(udf, each = length(qvals)), 'df'),
                         qvals = rep(qvals, length(udf)))
      qtls <- qf(p.df$qvals, rep(udf, each = length(qvals)), model$df.residual)
      plt <- plt +
         ggplot2::geom_vline(xintercept = qtls, col = grey(0.7), linetype = 2) +
         ggplot2::annotate('text', x = qtls, y = ypos * fhtlo + (1 - ypos) * fhthi,
                           label = paste('p:', as.character(p.reference), ' ', sep = ''),
                           col = grey(0.7), size = 3, hjust = 1)
      }
   
   # Use facets when there are multiple degrees of freedom
   if (length(unique(tbl$Df)) > 1) {
      # Use the ggforce package, if available, to scale the height of the rows
      if (requireNamespace('ggforce'))
         plt <- plt + ggforce::facet_col(Df ~ ., scales = 'free_y', space = 'free')
      else
         plt <- plt + ggplot2::facet_wrap(Df ~ ., scales = 'free_y') 
   }
   
   print(plt)
   plt

}

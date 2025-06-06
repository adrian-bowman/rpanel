#     Anova on a regression model

rp.drop1 <- function(model, subset.terms, p.reference = c(0.05, 0.01)) {
   
   tbl     <- drop1(model, test = 'F')[-1, ]
   # if (!missing(subset.terms) | is.null(subset.terms)) {
   if (!missing(subset.terms)) {
      if ((is.character(subset.terms)))
         subset.terms <- match(subset.terms, rownames(tbl))
      tbl <- tbl[subset.terms, ]
   }
   tbl$df  <- tbl$Df
   tbl$Df  <- paste('Model terms with', tbl$Df, 'df')
   fmax    <- 1.1 * max(qf(0.99, tbl$df, model$df.residual), tbl$'F value')
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
   qvals   <- if (length(p.reference) == 0) 0.95
              else 1 - p.reference
   curv.df <- data.frame(fht, fgrid = rep(fgrid, length(udf)),
                         Df = paste('Model terms with', rep(udf, each = ngrid), 'df'))
      
   plt <- ggplot2::ggplot(tbl, ggplot2::aes(`F value`, rownames(tbl))) +
          ggplot2::geom_ribbon(ggplot2::aes(x = fgrid, ymin = fhtlo, ymax = fht),
                               fill = grey(0.7), col = grey(0.7),
                               inherit.aes = FALSE, data = curv.df) +
          ggplot2::geom_point() +
          # ggplot2::geom_text(ggplot2::aes(label = paste('(p=',signif(p, 2),')', sep = '')),
          #                    nudge_y = -0.2, size = 3) +
          ggplot2::ylab('Model terms') +
          ggplot2::xlim(0, fmax)
   
   # Add a p-value scale and mark reference points
   if (!is.null(p.reference)) {
      p.df <- data.frame(Df = paste('Model terms with', rep(udf, each = length(qvals)), 'df'),
                         qvals = rep(qvals, length(udf)))
      p.df$qtls <- qf(p.df$qvals, rep(udf, each = length(qvals)), model$df.residual)
      plt <- plt +
      #    ggplot2::scale_x_continuous('F ratio',
      #              sec.axis = ggplot2::dup_axis(name = 'p',
      #                          breaks = p.df$qtls, labels = as.character(1 - p.df$qvals))) +
         ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor.x = ggplot2::element_blank()) +
         ggplot2::geom_vline(xintercept = p.df$qtls, col = grey(0.6), linetype = 2) +
         ggplot2::annotate('text', x = p.df$qtls, y = -0.5,
                           label = c(paste('p=', 1 - p.df$qvals[1], sep = ''),
                                     as.character(1 - p.df$qvals[-1])),
                           col = 'black', size = 3)
      # plt <- plt + ggplot2::geom_text(ggplot2::aes(x = c(1, qtls), y = 0.1 * fhtlo + 0.9 * fhthi,
      #                                           label = c('p:', as.character(1 - qvals))),
      #                              col = grey(0.5), size = 3, inherit.aes = FALSE, data = p.df)
   # ggplot2::geom_segment(ggplot2::aes(x = qtls,  xend = qtls,
                   #                       y = fhtlo, yend = (fhtlo + fhthi) * 0.5),
                   #          linetype = 'dashed', col = grey(0.5),
                   #          inherit.aes = FALSE, data = p.df)
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

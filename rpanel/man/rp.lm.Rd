\name{rp.lm}

\alias{rp.lm}

\title{Graphical display of linear models with no more than two explanatory variables}

\description{
This function is primarily intended to provide graphical representations of linear models with no more than two explanatory variables. These can be covariates or factors or a mixture of the two. When there are more than two explanatory variables the model is passed on to the \code{\link{rp.coefficients}} or \code{\link{rp.drop1}} functions.
}

\usage{
rp.lm(x, ylab, xlab, zlab, ci = TRUE,
      panel = TRUE, panel.plot = TRUE, plot.nodes.only = FALSE,
      hscale = 1, vscale = hscale,
      display.model, residuals.showing, ...)
}

\arguments{
\item{x}{a formula defining the linear model or a fitted linear model object.}
\item{ylab, xlab, zlab}{character variables used for the axis labels.}
\item{ci}{a logical value which is passed to the \code{rp.coefficients} function when this is used to display model coefficients.}
\item{panel}{a logical value which determines whether a panel is created to allow interactive control of the fitted models.}
\item{panel.plot}{a logical value which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).}
\item{add.layers}{a logical value which indicates whether additional \code{ggplot} layers are to added to the \code{rp.lm} call to alter features of the plot. When this is \code{TRUE}, \code{rp.lm} does not produce a plot, as this will be constructed when the additional layers are added.}
\item{plot.nodes.only}{a logical value, relevant only when the function is called in non-interactive mode (\code{panel = FALSE}), which determines whether the lattive of model nodes is plotted or the plot of the data and fitted model is returned. In the latter case, the returned object is a \code{ggplot} object.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1.}
\item{display.model}{a one sided formula which determines the specific model to be displayed. This can be a reduced model which omits some of the terms present in the model defined by \code{x}, or an expanded model which includes interaction terms.}
\item{comparison.model}{a one sided formula which determines the specific model to be compared with the displayed model. This can be a reduced model which omits some of the terms present in the model defined by \code{x} or, when at least one factore is present, an expanded model which includes an interaction term.}
\item{residuals,showing}{a logical value which determines whether residuals are displayed when there are two covariates.}
\item{linewidth}{a numerical value which sets the \code{linewidth} parameter in the case of one covariate and one factor.}
\item{...}{other arguments passed on the \code{lm} when \code{x} is a formula rather than a fitted model.}
}

\details{
The display of the data is adapted to the form of the explanatory variables. When \code{panel = TRUE} a lattice of potential models is displayed in graphical form. When a node of the lattice is clicked the corresponding fitted model is displayed. When a connection between adjacent models is clicked, a comparison of the two adjacent models is displayed, either as model coefficients (\code{display = 'coefficients'}) or model terms (\code{display = 'coefficients'}).

When the model defined by \code{x} contains two explanatory variables, where at least one is a factor, the models which can be fitted include the interaction term, even where this is not explicitly included in \code{x}.

When there are exactly two covariates in \code{x}, the function will not consider an interaction between these, even when \code{x} includes this. 
}

\value{
  Nothing is returned.
  }

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  y <- rnorm(50, mean = 10, sd = 0.5)
  rp.normal(y)
  }}

\keyword{iplot}
\keyword{dynamic}

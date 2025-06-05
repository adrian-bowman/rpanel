\name{rp.lm}

\alias{rp.lm}

\title{Graphical display of linear models with no more than two explanatory variables}

\description{
This function provides graphical representations of linear models with mo more than twoexplanatory variables. These can be covariates or factors or a mixture of the two.
}

\usage{
rp.lm(x, ylab, xlab, zlab, ci = TRUE,
      panel = TRUE, panel.plot = TRUE,
      hscale = 1, vscale = hscale,
      display.model, residuals.showing, ...)
}

\arguments{
\item{x}{a formula defining the linear model.}
\item{ylab, xlab, zlab}{character variables used for the axis labels.}
\item{ci}{a logical value which is passed to the \code{rp.coefficients} function when this is used to display model coefficients.}
\item{panel}{a logical value which determines whether a panel is created to allow interactive control of the fitted models.}
\item{panel.plot = TRUE}{a logical value which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1.}
\item{display}{a one sided formula which determines the specific model to be displayed. This can be a reduced model which omits some of the terms present in the model defined by \code{x}, or an expanded model which includes interaction terms.}
\item{residuals,showing}{a logical value which determines whether residuals are displayed when there are two covariates.}
\item{...}{other arguments passed on the \code{lm} when \code{x} is a formula rather than a fitted model.}
}

\details{
The display of the data is adapted to the form of the explanatory variables. When \code{panel = TRUE} a lattice of potential models is displayed in graphical form. When a node of the lattice is clicked the corresponding fitted model is displayed. When a connection between adjacent models is clicked, a comparison of the two adjacent models is displayed, either as model coefficients (\code{display = 'coefficients'}) or model terms (\code{display = 'coefficients'}).
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

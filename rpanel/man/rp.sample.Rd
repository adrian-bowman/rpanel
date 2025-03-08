\name{rp.sample}

\alias{rp.sample}

\title{Interactive demonstration of sampling variation}

\description{
Plots sample from a normal distribution to illustrate the variation which results.  The population mean and the range of mean +/- 2 standard deviations can be superimposed, in the latter case to demonstrate that nearly all the data lie within this range.  The position of the sample mean can also be indicated in a separate plot where the mean and +/- 2 standard errors can be superimposed.
}

\usage{
rp.sample(mu = 0, sigma = 1, n = 25,
          ggplot = TRUE, display = 'histogram',
          panel.plot = TRUE, hscale = NA, vscale = hscale, pause = 0.01)
}

\arguments{
\item{mu}{the mean of the normal distribution from which samples are taken.}
\item{sigma}{the standard deviation of the normal distribution.}
\item{n}{the size of the sample.}
\item{ggplot}{a logical value which determines whether the \code{ggplot2} package is used to create the graphical displays. When this argument is set ot \code{FALSE}, or when the \code{ggplot2} package is not available, standard graphics are used.}
\item{display}{a logical value which determines the form of graphical display. Possible values are \code{'histogram'}, \code{'density'} or \code{'violin'}.}
\item{panel.plot}{a logical value which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1.}
\item{pause}{a time delay, in seconds, for the insertion of components into the control panel. See Details.}
}

\details{
When \code{display} is set to \code{density} or \code{violin}, density estimates are constructed using a bandwidth which is optimal for a normal distribution. For small samples this provides a stable and conservative estimate which is not unduly influenced by features which may well simply be due sampling variation. As the sample size increases, the estimate will still converge to the true density function.

The visual effect of the animation is assisted by holding the axes constant.  This means that there may occasionally be observations outside the displayed horizontal range, or a histogram height which exceeds the displayed vertical range.  In both these cases, the existence of the unseen data is signalled by red lines in the appropriate positions.

The speed of some computing systems can create a panel which doess not expand in time to contain all its components. The \code{pause} argument adds a short delay to each component to avoid this.
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
   rp.sample()
}}

\keyword{iplot}
\keyword{dynamic}

\name{rp.sample}

\alias{rp.sample}

\title{Interactive demonstration of sampling variation}

\description{
  Plots sample from a normal distribution to illustrate the variation which results.  The population mean and the range of mean +/- 2 standard deviations can be superimposed, in the latter case to demonstrate that nearly all the data lie within this range.  The position of the sample mean can also be indicated in a separate plot where the mean and +/- 2 standard errors can be superimposed.
}

\usage{
   rp.sample(mu = 0, sigma = 1, n = 25, display = "shiny",
             panel.plot = TRUE, hscale = NA, vscale = hscale)
}

\arguments{
\item{mu}{the mean of the normal distribution.}
\item{sigma}{the standard deviation of the normal distribution.}
\item{n}{the size of the sample.}
\item{display}{a logical parameter which can take values \code{"shiny"} or \code{"tcltk"} determines whether a \code{shiny} or \code{tcltk} app is launched when \code{panel} is set to \code{TRUE}.}
\item{panel.plot}{a logical parameter, relevant to the \code{tcltk} case only, which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).}
\item{hscale, vscale}{scaling parameters, relevant to the \code{tcltk} case only, for the size of the plot when \code{panel} is set to \code{TRUE}.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
}

\details{
The visual effect of the animation is assisted by holding the axes constant.  This means that there may occasionally be observations outside the displayed horizontal range, or a histogram height which exceeds the displayed vertical range.  In both these cases, the existence of the unseen data is signalled by red lines in the appropriate positions.
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

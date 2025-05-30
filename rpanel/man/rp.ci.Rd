\name{rp.ci}

\alias{rp.ci}

\title{Simulations of normal-based confidence intervals}

\description{
  This function shows simulated confidence intervals for the mean of
  a normal distribution.  It also creates a panel which controls the 
  mean and standard deviation of the population and the size of the 
  simulated sample.
  }

\usage{
   rp.ci(mu = 0, sigma = 1, sample.sizes = c(30, 50, 100, 200, 500),
         confidence = 0.95, panel = TRUE, display = "tcltk",
         panel.plot = TRUE, hscale = NA, vscale = hscale)
  }

\arguments{
\item{mu, sigma}{the population mean and standard deviation.}
\item{sample.sizes}{the available sample sizes (\code{30}, \code{50}, \code{100}, \code{200}, \code{500}) for simulated data.}
\item{confidence}{the available confidence levels (\code{0.90}, \code{0.95}, \code{0.99}).}
\item{panel}{a logical value determining whether an interactive panel is created or a static display is produced.}
\item{display}{a logical parameter which can take values \code{"shiny"} or \code{"tcltk"} determines whether a \code{shiny} or \code{tcltk} app is launched when \code{panel} is set to \code{TRUE}.}
\item{panel.plot}{a logical parameter, relevant to the \code{tcltk} case only, which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).}
\item{hscale, vscale}{scaling parameters, relevant to the \code{tcltk} case only, for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1.}
}

\details{A button is provided to sample repeatedly from the current settings.  Confidence intervals which cover the population mean are coloured blue while those which miss are coloured red.  Repeated simulations illustrate the property of confidence intervals to capture the true value with probability determined by the confidence level (which here is set to 0.95).}

\value{Nothing is returned.}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  rp.ci()
}}

\keyword{iplot}
\keyword{dynamic}

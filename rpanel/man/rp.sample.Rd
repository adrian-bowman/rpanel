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
\item{ggplot}{a logical value which determines whether the \code{ggplot2} package is used to create the graphical displays. When this argument is set to \code{FALSE}, or when the \code{ggplot2} package is not available, standard graphics are used. In this case the display options are simpler, along the lines of the function provided in version 1.1-5 of the package.}
\item{panel}{a logical value which determines whether the function runs in interactive mode. See Details.}
\item{nbins}{an integer value which sets the number of bins used in histograms.}
\item{display}{a logical value which determines the form of graphical display used initially or in non-interactive mode. Possible values are \code{'histogram'} (the default), \code{'density'} or \code{'violin'}.}
\item{display.sample}{a logical value which controls options for graphical display of the data, used initially or in non-interactive mode. See Details.}
\item{display.sample}{a logical value which controls options for graphical display of the sample means, used initially or in non-interactive mode. See Details.}
\item{nsim}{an integer value which the number of accumulated mean values which are plotted when the function runs in non-interactive mode. See Details.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1.}
\item{pause}{a time delay, in seconds, for the insertion of components into the control panel. The speed of some computing systems can create a panel which does not expand in time to contain all its components. The \code{pause} argument adds a short delay to each component to avoid this.}
}

\details{
When \code{display} is set to \code{density} or \code{violin}, density estimates are constructed using a bandwidth which is optimal for a normal distribution. For small samples this provides a stable and conservative estimate which is not unduly influenced by features which may well simply be due sampling variation. As the sample size increases, the estimate will still converge to the true density function.

When the size of the sample is less than 10, a histogram or density estimate is not a very effective display. This also causes issues of scaling the vertical axis. So in this case individual points are displayed instead.

When \code{display} is set to \code{'density'} or \code{'violin'}, individual points are plotted, with a random vertical position. This is suppressed when the number of points exceeds 5000.

The \code{display.data} and \code{display.mean} arguments control the details of what is displayed initially and, more usefully, when the function operates in non-interactive mode. Each argument is a logical vector with named values. \code{display.data} has the default setting \code{c(data = TRUE, population = FALSE, mean = FALSE, '+/- 2 st.dev.' = FALSE)} while the default for \code{display.mean} is \code{c('sample mean' = FALSE, 'accumulate' = FALSE, '+/- 2 se' = FALSE, 'distribution' = FALSE)}. Any elements of these arguments which are not explicitly identified are set to the default values.

The principal use of the function is in interactive mode, when \code{panel} is set to \code{TRUE}. If \code{panel} is set to \code{FALSE} then interactive mode is switched off. In this case, if the \code{ggplot2} package is available and the \code{ggplot} argument is set to \code{TRUE}, the function returns plots of a sample of data and of accumulated means as components \code{plotdata} and \code{plotmean} of the returned object. The number of accumulated means is set by the \code{nsim} argument.

The visual effect of the animation is assisted by holding the axes constant.  This means that there may occasionally be observations outside the displayed horizontal range, or a histogram height which exceeds the displayed vertical range.
}

\value{
When the function operates in inteactive mode, with \code{panel} set to \code{TRUE}, nothing is returned. When \code{panel} is set to \code{FALSE}, plots of a sample of data and of accumulated means are provided as components \code{plotdata} and \code{plotmean} of the returned object.
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

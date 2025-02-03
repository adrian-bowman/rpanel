\name{rp.onesample}

\alias{rp.onesample}

\title{Graphical display for inference on a single sample}

\description{
A single sample of data is plotted along with a graphical display which indicates the uncertainty in the location of the true mean. This is done by plotting a shaded representation of a normal distribution in 'violin plot' style. This can also be compared with a reference mean, representing the null hypothesis in a t-test.
}

\usage{
rp.onesample(x, xlab, data_plot = "sina", reference)
}

\arguments{
\item{x}{a vector of data on a continuous scale.}
\item{xlab}{an optional character name used for the data sample.}
\item{data_plot}{a character variable which determines the form of data display. The setting \code{"jitter"|} invokes the \code{ggplot} geometry \code{geom_jitter} while the setting \code{"sina"} invokes the geometry \code{geom_sina} from the \pkg{ggforce} if this is available. The latter adds random noise to the vertical posiotion of the data points, with scale proportional to the estimated density.}
\item{reference}{a numerical value representing a reference of interest, for
                 example a null hypothesis in a formal test.}
\item{scale}{a logical value which determines whether a standard error scale is
             added to the plot.}
\item{height}{the height of the uncertainty representation as a proportion of
              the height of the plot.}
\item{col}{the col of the uncertainty representation.}
\item{notch}{the position of notches on the uncertainty representation,
             in units of standard error.}
\item{notchcol}{the col of the notches on the uncertainty representation.}
\item{refheight}{the height of the reference line, when this is present.}
\item{refcol}{the col of the reference line, when this is present.}
\item{seed}{a seed which can be used to reproduce the jittering of y.}
}

\details{
The graphics are created through the \pkg{ggplot2} package so this needs to be installed. If \code{data_plot} is set to \code{"sina"} then the \pkg{ggforce} packagfe is also required.
}

\value{
A \code{ggplot} object is returned. This allows further alterations and annotations to be made.
  }

\references{
   Bowman, A.W. (2019). Graphics for uncertainty (with discussion).
      Journal of the Royal Statistics Society, Series A,
      Statistics \& Society, 182, 403-418.
   }

\examples{
\dontrun{
x <- rnorm(50)
rp.onesample(x)
rp.onesample(x, reference = 0)
plt <- rp.onesample(x, data_plot = "sina")
plt + ggplot2::ggtitle("One-sample inference")
}}

\keyword{iplot}
\keyword{dynamic}

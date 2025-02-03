\name{rp.ttest}

\alias{rp.ttest}

\title{Graphical display for inference in comparing the means of one or two samples}

\description{
The sample(s) of data are plotted along with a graphical display which
indicates the uncertainty in the location of the true means and in their
difference. This is done by plotting a shaded representation of a normal
distribution in 'violin plot' style. This can also be compared with a reference
mean, representing the null hypothesis in a t-test.
}

\usage{
rp.ttest <- function(x, y = NULL, mu = NULL,
                     uncertainty = 'sample mean',
                     scale = FALSE, col = '#86B875', refcol = '#E495A5',
                     height = 0.2, refheight = 1.5 * height, seed, ...)
}

\arguments{
\item{x}{a vector of data on a continuous scale.}
\item{mu}{a numerical value representing a reference mean of interest, for
          example a null hypothesis in a formal test. This is the same parameter
          as \code{mu} in \code{t.test} but its default value is \code{NULL}
          so that a reference value will be display only if requested.}
\item{violin}{a logical value which determines whether uncertainties are added
in symmetric fashion, as in a violin plot.}
\item{scale}{a logical value which determines whether a standard error scale is
             added to the plot.}
\item{height}{the height of the uncertainty representation as a proportion of
              the height of the plot.}
\item{mnheight}{the height of the lines marking mean values, when this is present.}
\item{col}{the col of the uncertainty representation.}
\item{notch}{the position of notches on the uncertainty representation,
             in units of standard error.}
\item{notchcol}{the col of the notches on the uncertainty representation.}
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

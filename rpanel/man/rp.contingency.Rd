\name{rp.contingency}

\alias{rp.contingency}

\title{Visualisation and testing of two-way contingency tables}

\description{
A two-way contingency table is displayed graphically and the uncertainties associated with a chi-squared test may be visualised.
}

\usage{
rp.contingency(x, style = "mosaic", values = "observed",
               uncertainty = 'none', proportion.scale = 'fixed', cols)
}

\arguments{
\item{x}{a two-way table or matrix of observed counts.}
\item{style}{a character value, either \code{'mosaic'} or \code{'separated'}, which determines the form of display.}
\item{values}{a character value, either \code{'observed'}, \code{'proportions'} or \code{'expected'}, which controls what is printed inside each cell of the display.}
\item{uncertainty}{a character value which controls whether uncertainties associated with a chi-squared test are displayed for each cell, in \code{'shading'} or \code{'violin'} style.}
\item{proportion.scale}{a character value specifying whether the proportion scale is \code{'fixed'} or \code{'free'}.}
\item{cols}{a named vector of characters specifying colours to be used in the plot. See \code{help(rp.colours)} for details.}
}

\details{
The table is displayed in a form which stratifies by columns. If it more appropriate to stratify by the rows of \code{x} then two options are available. One is to apply the function to \code{t(x)}. The other is to add the \code{ggplot} layer \code{ggplot2::coord_flip()} to the \code{ggplot} object returned by the function.

Missing data are not allowed.
}

\value{
A \code{ggplot} object is returned. This allows some features of the display to be modified, for example by adding a title.
}

\examples{
\dontrun{
   x <- matrix(c(19, 41, 32, 28), ncol = 2,
            dimnames = list(c("non-smoker", "smoker"),
                            c("cases", "controls")))
   rp.contingency(x)
   rp.contingency(x, style = "aligned")
   rp.contingency(x, uncertainty = TRUE)
   rp.contingency(x, uncertainty = TRUE) + ggplot2::coord_flip()
}}

\keyword{iplot}
\keyword{dynamic}

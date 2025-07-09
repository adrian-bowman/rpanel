\name{rp.t_test}

\alias{rp.t_test}

\title{Graphical display for inference in comparing the means of one or two samples}

\description{
The sample(s) of data are plotted along with a graphical display which
indicates the uncertainty in the location of the true means and in their
difference. This is done by plotting a shaded representation of a normal
distribution in 'violin plot' style. This can also be compared with a reference
mean, representing the null hypothesis in a t-test.
}

\usage{
rp.t_test(x, y = NULL, mu = NULL, display = 'density',
         uncertainty = 'sample mean', scale = TRUE, zoom = FALSE,
         col = '#86B875', refcol = '#E495A5',
         height = 0.2, refheight = 1.5 * height, ...)
}

\arguments{
\item{x}{a vector of data on a continuous scale or a formula with a response variable and a single prediuctor variable which is a factor with two levels.}
\item{y}{an optional second vector of data on a continuous scale, to allow two-sample or paired-sample analyses. Alternatively, \code{y} may be a character vector or a factor with two levels, defining the groups. If \code{x} is a formula then \code{y} is ignored.}
\item{mu}{a numerical value representing a reference mean of interest, for
            example a null hypothesis in a formal test. This is the same parameter
            as \code{mu} in \code{t.test} but its default value is \code{NULL}
            so that a reference value will be display only if requested.}
\item{display}{a character value which controls the form of the data and
            uncertainty displays. Valid options are \code{'histogram'},
            \code{'density'} or \code{'violin'}.}
\item{uncertainty}{a character value which controls the form of uncertainty to
            be displayed. Valid options are \code{'none'}, \code{'sample mean'} or \code{'reference'}.}
\item{scale}{a logical value which determines whether a standard error scale is
            added to the plot.}
\item{zoom}{a logical value which determines whether the plot contains only the 
            uncertainty display.}
\item{height}{the height of the uncertainty representation as a proportion of
            the height of the plot.}
\item{mnheight}{the height of the lines marking mean values, when this is present.}
\item{col}{the col of the uncertainty representation.}
\item{notch}{the position of notches on the uncertainty representation,
            in units of standard error.}
\item{notchcol}{the col of the notches on the uncertainty representation.}
\item{refcol}{the col of the reference line, when this is present.}
\item{...}{further named argument, which will be passed on to the \code{t.test}
            function. Useful arguments include \code{paired} and
            \code{var.equal} which both take logical values.}
}

\details{
The graphics are created through the \pkg{ggplot2} package so this needs to be installed. If \code{data_plot} is set to \code{"sina"} then the \pkg{ggforce} package is also required.

The function aims to follow the general pattern of the \code{t.test} function and the arguments of \code{t.test} described in its help file can also be passed to \code{rp.t_test}.
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
rp.t_test(x)
rp.t_test(x, mu = 0)
rp.t_test(x, uncertainty = 'sample mean')
rp.t_test(x, uncertainty = 'reference')

g <- factor(rep(1:2, each = 25))
plt <- rp.t_test(x ~ g)
plt + ggplot2::ggtitle("Two-sample inference")
}}

\keyword{iplot}
\keyword{dynamic}

\name{rp.assign}

\alias{rp.assign}

\title{Send variables into an rpanel}

\description{
  This function sets up variables within the rpanel object. This is used to send further parameters needed 
  after the initial assignment carried out by \code{\link{rp.control}}.
}

\usage{
rp.assign(panel, ...)
}

\arguments{
  \item{panel}{the panel on which the text should appear.}
  \item{\dots}{arguments which are treated as variable initialisations and are stored within the rpanel object.}
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }
   
\note{
 Don't forget the assignments - e.g. "b=b, d=d" not "b, d".
}   

\seealso{
\code{\link{rp.control}}
}

\examples{
\dontrun{
   panel <- rp.control()
   rp.assign(panel, b=1, d=10)
   print(rp()$panel$b)
   print(rp()$panel$d)
}}

\keyword{iplot}
\keyword{dynamic}

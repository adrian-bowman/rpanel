\name{rp.env}

\alias{rp}
\alias{rp.env}

\title{Returns the rpanel environment}

\description{
This function returns the rpanel environment into which parameters are passed by rp.control and rp.assign or w.window and 
w.assign. Many of the widgets created by rp. functions also create a variable within the environment.
}

\usage{
rp.env()
}

\value{
  The rpanel environment is returned. This, if used with a 'with' statement, can be used to alter variables within the
  environment.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{ \code{\link{rp.assign}}, \code{\link{rp.control}} }

\examples{
\dontrun{
  w <- w.window(x=1)
  callback <- function()
  {
    with(rp(), {
      w.text.change(t2, x)
      x = x+1
    })
  }
  t1 <- w.text(w,"This is a test")
  t2 <- w.text(w,"And so is this", font="Arial", foreground="white", background="navy", action=callback)
  t3 <- w.text(w,"Here is some more text, this time across several lines.\nHere is some more text, this time across several lines.\nHere is some more text, this time across several lines.")
}}

\keyword{iplot}
\keyword{dynamic}

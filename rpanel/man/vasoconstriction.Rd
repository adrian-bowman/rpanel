\name{flour_beetles}

\alias{flour_beetles}

\title{Toxins for the eradication of flour beetles}

\description{
These data are from an experiment, reported by Gilliatt (1948), on vaso-constriction of fingers after a single deep breath. The measurement process was not sufficiently precise to determine the degree of vaso-constriction, simply its presence or absence. Interest lay in how the presence of vaso-constriction relates to the volume and flow rate of air during the intake of breath.
}

\details{
Finney (1947) reported the data and discussed a variety of models in some depth.
Note that the table of data in this paper seems to contain a misprint, with observation 32 showing a rate of 0.03 while the plots indicate this to be 0.30. This dataframe incorporates this correction.
}

\references{
Finney, D. (1947). The estimation from individual records of the relationship between dose and quantal response. Biometrika 34, 320-334.
Gilliatt, R. W. (1948). Vaso-constriction in the finger after deep inspiration. Journal of Physiology 107, 76-88.
}

\examples{
\dontrun{
with(vasoconstriction, {
  plot(log(Rate), log(Volume), type = 'n')
  text(log(Rate), log(Volume), as.character(Vasoconstriction))
})
}}

\keyword{iplot}
\keyword{dynamic}


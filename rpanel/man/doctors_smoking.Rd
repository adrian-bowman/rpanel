\name{flour_beetles}

\alias{flour_beetles}

\title{Toxins for the eradication of flour beetles}

\description{
These data are from a famous study, initiated by Doll & Hill, which surveyed UK doctors and related their smoking habits to subsequent coronary-related deaths. The data in a form reduced to person-years exposure was reported by Breslow (1985).
}

\details{

}

\references{
Breslow, N.E. (1985) Cohort Analysis in Epidemiology. In A Celebration of Statistics A.C. Atkinson and S.E. Fienberg (editors), 109–143. Springer-Verlag.
Doll, R. and Hill, A.B. (1966) Mortality of British doctors in relation to smoking: Observations on coronary thrombosis. National Cancer Institute Monograph, 19, 205-268.
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


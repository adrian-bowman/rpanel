\name{rp.coefficients}

\alias{rp.coefficients}

\title{Graphical display of the effects of the coefficients in a linear model}

\description{
  The effects of the coefficients of a linear model are displayed in graphical
  form. The effects are made comparable by viewing the change in the mean
  value of the response as each predictor variable changes across its full
  range.
}

\usage{
rp.coefficients(model, style = 'density', ci = TRUE, point.estimate = !ci,
                 se.scale = FALSE, marks = c(-2, 2), labels, subset, col,
                 ngrid = 200)
}

\arguments{
\item{model}{a linear model produced by the \code{lm} function.}
\item{style}{a character variables controlling whether effect uncertainties are
         displayed as densities (\code{'density'}) or shaded density strips
         (\code{'shading.'})}
\item{ci}{a logical value controlling whether uncertainty is placed around the
         estimated effect in confidence interval style or around 0 in the format
         of a hypothesis test.}
\item{point.estimate}{a logical value controlling whether the estimated effect
         is displayed.}
\item{se.scale}{a logical value controlling whether a standard error scale is
         superimposed on the uncertainty display.}
\item{marks}{a vector of reference values to be highlighted on the standard
         error scale.}
\item{labels}{a character vector which allows amended labels to be used for each
         model coefficient. See Details.}
\item{subset}{an integer vector which specifies which model coefficients are
         included in the display. The vector refers to the coefficients as
         listed in the output of the summary function on the fitted model (with
         the intercept, if present, as number 1). If this is not specified then
         all model coefficients are included. In all cases, the intercept is
         excluded.}
\item{col}{the colour of the uncertainty display.}
\item{ngrid}{the number of grid points used in the construction of the
         uncertainty display.}
}

\details{
If the length of \code{labels} matches the number of coefficients in the model (after the 
application of the \code{subset} argument described below) then these labels
are used in the display. The length of the labels vector can also be larger than
the number of coefficients in the model to allow a consistent axis when models with
different terms are viewed successively. In this case \code{labels} must include
the names of all the effects in the model.
         
The covariate range information is be added to the labels by the function.
}

\value{
A \code{ggplot} object is returned. This allows further annotation of the display.
}

\examples{
\dontrun{
   model <- lm(Giving ~ Employ + Elect + Attend + 1, data = CofE)
   rp.coefficients(model)
   }
}

\name{rp.geosim}

\alias{rp.geosim}

\title{Interactive visualisation of spatially correlated random fields}

\description{
This function allows Gaussian random fields to be simulated and visualised, using graphical controls for a variety of parameter settings.}

\usage{
rp.geosim(max.Range = 0.5, max.pSill = 1, max.Nugget = 1, max.Kappa = 10, 
          max.aniso.ratio = 5, min.ngrid = 10, max.ngrid = 25,
          hscale = NA, vscale = hscale, smgrid = 40, ngrid = 15,
          Range = 0.1, pSill = 0.5, Nugget = 0, kappa = 4,
          aniso.angle = 0, aniso.ratio = 1,
          col.palette = terrain.colors(40), panel = TRUE)
}

\arguments{
\item{max.Range, max.pSill, max.Nugget}{the maximum values of the range, sill and nugget parameters. These define the end-points of the corresponding slider scales.}
\item{max.Kappa}{The maximum value of the kappa parameter in the Matern family of spatial covariance functions.}
\item{max.aniso.ratio}{The maximum value of the anisotropy ratio parameter, which controls the degree of anisotropy in the simulated field.}
\item{min.ngrid, max.ngrid}{the minimum and maximum values of the grid size for sampling points.}
\item{hscale, vscale}{horizontal and vertical scaling factors for the size of the plots. It can be useful to adjust these for different screen resolutions or for projection in a lecture setting. The default values are 1.}
\item{smgrid}{the number of grid points on each dimension of the displayed process. The default is 40.}
\item{ngrid}{the number of grid points in each dimension of the sampled locations. The default is 15.}
\item{Range, pSill, Nugget}{the initial values of the range, partial sill and nugget parameters. the defaults values are 0.1, 0.5 and 0 respectively.}
\item{kappa}{the initial value of the smoothness parameter of the Matern covariance function. The default is 4.}
\item{aniso.angle, aniso.ratio}{the initial values of the anisotropy and and ratio parameters. the defaulkt values are 0 and 1 respectively.}
\item{col.palette}{the colour palette used to display the random fields.}
\item{panel}{a logical variable which determines whether an interactive panel is created.}
}

\details{
The aim of the tool is to allow the generation of repeated simulated fields without the distraction of re-executing code explicitly.  This can help to gain an intuitive understanding of the nature of spatial data.  In particular, interactive control of parameters can help greatly in understanding the meaning and effects of parameter values.  Nugget effects can be added and sampled points displayed.  Two-dimensional contour plots are produced.  Three-dimensional plots are also produced if the \pkg{rgl} package is available.

The use of the function is discussed in the paper paper by Bowman et al. (2008) referenced below.

The \pkg{geoR} and \pkg{fields} packages are used to generate the data.

Note that the Matern covariance function is parameterised in the form described by Handcock & Wallis (1994) which separates the effects of the shape and range parameters.
}

\value{
  Nothing is returned.
}

\references{
   Adler, D. (2005).
   rgl: 3D Visualization Device System (OpenGL).
   \url{https://cran.r-project.org}.

   Bowman, A.W., Crawford, E., Alexander, G. Gibson and Bowman, R.W. (2007).   
   rpanel: Simple interactive controls for R functions using the tcltk package.
   Journal of Statistical Software, 17, issue 9.
      
   Bowman, A.W., Gibson, I., Scott, E.M. and Crawford, E. (2008).
   Interactive Teaching Tools for Spatial Sampling.
   Journal of Statistical Software, 36, 13, 1--17.

   Diggle, P.J. and Ribiero, P.J. (2008).
   Model-based Geostatistics.
   Springer, New York.
   
   Handcock, M.S. and Wallis, J.R. (1994).
   An approach to statistical spatial-temporal modeling of meteorological fields.
   Journal of the American Statistical Association, 89, 368-378.
   }

\seealso{\code{\link{rp.firth}}, \code{\link{rp.mururoa}}}

\examples{
\dontrun{
   rp.geosim()
}}

\keyword{iplot}
\keyword{dynamic}

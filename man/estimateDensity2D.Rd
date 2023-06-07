\name{estimateDensity2D}
\alias{estimateDensity2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
estimateDensity2D
}
\description{
Estimates densities for two-dimensional data with the given estimation type
}
\usage{
estimateDensity2D(X, Y, DensityEstimation = "SDH",

SampleSize, na.rm = FALSE, NoBinsOrPareto = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
[1:n] numerical vector of first feature
}
  \item{Y}{
 [1:n] numerical vector of second feature
}
  \item{DensityEstimation}{
 Either "PDE","SDH" or "kde2d"
}
  \item{SampleSize}{
 Sample Size in case of big data
}
  \item{na.rm}{
 Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE
}
  \item{NoBinsOrPareto}{
Density specifc parameters, for PDEscatter(ParetoRadius) or SDH (nbins)) or kde2d(bins)
}
}
\details{
Each two-dimensional data point is defined by its corresponding X and Y value.
}
\value{
List V with
 \item{X }{[1:m] numerical vector of first feature, m<=n depending if all values are finite an na.rm parameter}
\item{Y }{[1:m] numerical vector of second feature, m<=n depending if all values are finite an na.rm parameter}
\item{Densities }{the density of each two-dimensional data point}
}
\references{
[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, In Baier, D. & Werrnecke, K. D. (Eds.), Innovations in classification, data science, and information systems, (Vol. 27, pp. 91-100), Berlin, Germany, Springer, 2005.

[Eilers/Goeman, 2004]  Eilers, P. H., & Goeman, J. J.: Enhancing scatterplots with smoothed densities, Bioinformatics, Vol. 20(5), pp. 623-628. 2004
}
\author{
Luca Brinkman and Michael Thrun
}

\examples{
X=runif(100)
Y=rnorm(100)
#V=estimateDensity2D(X,Y)
}
\keyword{estimateDensity2D}
\concept{estimate densities in 2D}

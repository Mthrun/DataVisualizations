\name{SmoothedDensitiesXY}
\alias{SmoothedDensitiesXY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smoothed Densities X with Y
}
\description{
Density is the smothed histogram density at [X,Y] of [Eilers/Goeman, 2004]
}
\usage{
SmoothedDensitiesXY(X, Y, nbins, lambda, Xkernels, Ykernels, PlotIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Numeric vector [1:n], first feature (for x axis values)
}
  \item{Y}{
Numeric vector [1:n], second feature (for y axis values),
nbins= nxy      => the nr of bins in x and y is nxy
nbins = c(nx,ny) => the nr of bins in x is nx and for y is ny
}
  \item{nbins}{
 number of bins, nbins =200 (default)
}
  \item{lambda}{
smoothing factor used by the density estimator or c()
default: lambda = 20 which roughly  means that the smoothing is over 20 bins around a given point.
}
  \item{Xkernels}{
bin kernels in x direction are given
}
  \item{Ykernels}{
bin kernels y direction are given
}
  \item{PlotIt}{
FALSE: no plotting, TRUE: simple plot
}
}
\details{
lambda has to chosen by the user and is a sensitive parameter.
}
\value{
 List of:
   \item{Densities}{numeric vector [1:n] is the smothed density in 3D  }
  \item{Xkernels}{numeric vector [1:nx], nx defined by \code{nbins}, such that mesh(Xkernels,Ykernels,F) form the ( not NaN) smothed densisties}
  \item{Ykernels}{numeric vector [1:ny], nx defined by \code{nbins}, such that mesh(Xkernels,Ykernels,F) form the ( not NaN) smothed densisties}
  \item{hist_F_2D}{matrix [1:nx,1:ny] beeing the smoothed 2D histogram }
  \item{ind}{an index such that \code{Densities = hist_F_2D[ind]}}

}
\references{
[Eilers/Goeman, 2004]  Eilers, P. H., & Goeman, J. J.: Enhancing scatterplots with smoothed densities, Bioinformatics, Vol. 20(5), pp. 623-628. 2004.
}
\author{
Michael Thrun, reimplemented from Matlab (Alfred Ultsch)
}


\seealso{
\code{\link{DensityScatter}}
}
\examples{
data("ITS")
data("MTY")
Inds=which(ITS<900&MTY<8000)
V=SmoothedDensitiesXY(ITS[Inds],MTY[Inds])
}
\keyword{scatter}
\keyword{SDH}
\concept{Density Estimation}
\concept{scatter plot}
\concept{scatter density plot}

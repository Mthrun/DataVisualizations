\name{plot3D}
\alias{plot3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D plot of points
}
\description{
A wrapper for Data with systematic clustering colors for a x,y,z plot combined with a classification
}
\usage{
plot3D(Data,Cls,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] matrix with either d=2 or d=3, if d>3 only the first 3 dimensions are taken
}
  \item{Cls}{
[1:n] numeric vector of the classification of data
}
  \item{\dots}{
further arguments to be processed by \code{\link[rgl]{plot3d}} or \code{\link{plot}}
}
}
\details{
if d<3 \code{points} function is called with \code{pch=19}
}

\references{
RGL in \url{https://cran.r-project.org/web/packages/rgl/vignettes/rgl.html}

Spin3D in \url{https://www.uni-marburg.de/fb12/arbeitsgruppen/datenbionik/software-en}
}
\author{
Michael Thrun
}

\note{
Uses either  \code{\link{plot}} for 2D or \code{\link[rgl]{plot3d}} for 3D
}

\examples{
#Spin3D similar output
data(Lsun3D)
plot3D(Lsun3D$Data,Lsun3D$Cls,type='s',radius=0.1,box=F,aspect=T)
\donttest{
rgl::grid3d(c("x", "y", "z"))
}
}


\keyword{plot3}% use one of  RShowDoc("KEYWORDS")
\keyword{plot3d}% __ONLY ONE__ keyword per line
\keyword{plot3D}% __ONLY ONE__ keyword per line
\keyword{zplot}% __ONLY ONE__ keyword per line
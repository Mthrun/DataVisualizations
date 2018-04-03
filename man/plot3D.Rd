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
[1:n,1:d] matrix with d=3, if d>3 only the first 3 dimensions are taken
}
  \item{Cls}{
[1:n] numeric vector of the classification of data
}
  \item{\dots}{
further arguments to be processed by \code{rgl::plot3d}
}
}
\details{
if d<3 \code{points} function is called with \code{pch=19}
}

\references{
Spin3D in \url{https://www.uni-marburg.de/fb12/arbeitsgruppen/datenbionik/software-en}
}
\author{
Michael Thrun
}

\keyword{plot3}% use one of  RShowDoc("KEYWORDS")
\keyword{plot3d}% __ONLY ONE__ keyword per line
\keyword{plot3D}% __ONLY ONE__ keyword per line
\keyword{zplot}% __ONLY ONE__ keyword per line
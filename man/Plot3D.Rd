\name{Plot3D}
\alias{Plot3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D plot of points
}
\description{
A wrapper for Data with systematic clustering colors for either a 2D (x,y) or 3D (x,y,z) plot combined with a classification
}
\usage{
Plot3D(Data,Cls,UniqueColors,size=2,na.rm=FALSE,\dots)
}

\arguments{
  \item{Data}{
[1:n,1:d] matrix with either \code{d=2} or \code{d=3}, if \code{d>3} only the first 3 dimensions are taken
}
  \item{Cls}{
[1:n] numeric vector of the classification of data with \code{k} classes
}
  \item{UniqueColors}{
[1:k] character vector of colors, if not given DataVisualizations::DefaultColorSequence is used
}

  \item{size}{
size of points
}
  \item{na.rm}{
if \code{na.rm=TRUE}, then missing values are removed
}
  \item{\dots}{
further arguments to be processed by \code{\link[rgl]{plot3d}} or \code{\link[ggplot2]{geom_point}}
}
}
\details{
For \code{\link[ggplot2]{geom_point}} only \code{size} and \code{na.rm} is available as further arguments.
}

\references{
RGL vignette in \url{https://cran.r-project.org/package=rgl}

Spin3D in \url{https://www.uni-marburg.de/fb12/arbeitsgruppen/datenbionik/software-en}
}
\author{
Michael Thrun
}

\note{
Uses either \code{\link[ggplot2]{geom_point}} for 2D or \code{\link[rgl]{plot3d}} for 3D
}

\examples{
#Spin3D similar output
data(Lsun3D)
Plot3D(Lsun3D$Data,Lsun3D$Cls,type='s',radius=0.1,box=FALSE,aspect=TRUE)
\donttest{rgl::grid3d(c("x", "y", "z"))}

#Projected Points with Classification
Data=cbind(runif(500,min=-3,max=3),rnorm(500))

# Classification
Cls=ifelse(Data[,1]>0,1,2)
Plot3D(Data,Cls,UniqueColors = DataVisualizations::DefaultColorSequence[c(1,3)],size=2)

\donttest{
#Points with Non-Overlapping Labels
#require(ggrepel)
Data=cbind(runif(30,min=-1,max=1),rnorm(30,0,0.5))
Names=paste0('VeryLongName',1:30)
ggobj=Plot3D(Data)
ggobj +  geom_text_repel(aes(label=Names), size=3)
}
}


\keyword{plot3}% use one of  RShowDoc("KEYWORDS")
\keyword{plot3d}% __ONLY ONE__ keyword per line
\keyword{plot3D}% __ONLY ONE__ keyword per line
\keyword{zplot}% __ONLY ONE__ keyword per line

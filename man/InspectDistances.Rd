\name{InspectDistances}
\alias{InspectDistances}

\title{
Inspection of Distance-Distribution
}
\description{
Visualizes the distances between objects in the data matrix 
}
\usage{
InspectDistances(DataOrDistances,method= "euclidean",sampleSize = 50000,...)
}
\arguments{
  \item{DataOrDistances}{
  [1:n,1:d] data cases in rows, variables in columns, if not symmetric
	
	or
	
	[1:n,1:n] distance matrix, if symmetric
}
  \item{method}{Optional, 
if Data[1:n,1:d]
  see \code{dist} for distance method
}
  \item{sampleSize}{double value defining the size of the sample for large distance matrizes, see \code{InspectVariable}
}
  \item{\dots}{further arguments passed on to \code{InspectVariable}
}
}
\details{
For an interpretation of the distribution analysis of the distance please read [Thrun, 2018, p. 27, 185].
}

\author{
Michael Thrun
}
\note{
uses \code{InspectVariable}
}
\examples{
data("Lsun3D")
Data=Lsun3D$Data
\donttest{
InspectDistances(as.matrix(dist(Data)))
}
\dontshow{
InspectDistances(as.matrix(dist(Data[1:50,])))
}
}
\references{
[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.
}
\keyword{distance}% use one of  RShowDoc("KEYWORDS")
\keyword{Distances}% use one of  RShowDoc("KEYWORDS")
\keyword{InputDistances}% use one of  RShowDoc("KEYWORDS")
\keyword{InspectDistances}% __ONLY ONE__ keyword per line
\keyword{distribution analysis}% __ONLY ONE__ keyword per line
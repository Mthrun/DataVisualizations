\name{Heatmap}
\alias{Heatmap}

\title{
Heatmap for Clustering
}
\description{
Heatmap of Distances of Data sorted by Cls
}
\usage{
Heatmap(DataOrDistances,Cls,method='euclidean',LowLim=0,HiLim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistances}{
  [1:n,1:d] data cases in rows, variables in columns, if not symmetric
	
	or
	
	[1:n,1:n] distance matrix, if symmetric
}
  \item{Cls}{
   numeric vector, [1:n,1]  classified data 
}

  \item{method}{Optional, 
if Data[1:n,1:d]
  see \code{dist} for distance method
}
  \item{LowLim}{ Optional: limits for the color axis }
  \item{HiLim}{ Optional: limits for the color axis }
}
\details{
Clustering algorithms provide a Classifcation of data, 
where the labels are defined as a numeric vector \code{Cls}

Then, a typical cluster-respectively group structure is displayed by the \code{HeatMap} function.
At the margin of the heatmap a dendrogram can be shown, if hierarchical cluster algorithms are used.
Here the dendrogram has to be shown separately and only the heatmap itself is displayed [Wilkinson,2009].

More details in [Thrun, 2018, p. 29]
}
\value{
object of ggplot2
}
\author{
Michael Thrun
}

\seealso{
\code{\link{PixelMatrixPlot}}
}
\examples{
data("Lsun3D")
Cls=Lsun3D$Cls
Data=Lsun3D$Data
\donttest{
Heatmap(as.matrix(dist(Data)),Cls = Cls)
}
\dontshow{
Heatmap(as.matrix(dist(Data[1:50,])),Cls = Cls[1:50])
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Heatmap}% use one of  RShowDoc("KEYWORDS")
\keyword{heatmap}% __ONLY ONE__ keyword per line
\keyword{heat map}
\references{
[Wilkinson,2009] Wilkinson, L., & Friendly, M.: The history of the cluster heat map, The American Statistician, Vol. 63(2), pp. 179-184. 2009.

[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.
}
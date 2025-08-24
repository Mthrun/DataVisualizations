\name{PlotGraph2D}
\alias{PlotGraph2D}
\title{PlotGraph2D}
\description{
plots a neighborhood graph in two dimensions given the 2D coordinates of the points
}
\usage{
PlotGraph2D(AdjacencyMatrix, Points, Cls, Colors, xlab = "X", ylab = "Y", xlim,
ylim, Plotter = "native", LineColor = "grey", pch, lwd, main = "",
mainSize,SampleSize=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{AdjacencyMatrix}{[1:n,1:n] numerical matrix consting of binary values. 1 indicates that two points have an edge, zero that they do not}
\item{Points}{[1:n,1:2] numeric matrix of two feature}
\item{Cls}{[1:n] numeric vector of k classes, if not set per default every point is in first class}
\item{Colors}{Optional, string defining the k colors, one per class}
\item{xlab}{Optional, string for xlabel}
\item{ylab}{Optional, string for ylabel}
\item{xlim}{Optional, [1:2] vector of x-axis limits}
\item{ylim}{Optional, [1:2] vector of y-axis limits}
\item{Plotter}{Optional, either \code{"native"} or \code{"plotly"}}
\item{LineColor}{Optional, color of edges}
\item{pch}{Optional, for \code{Plotter="native"} shape of point, usally can be in a range from zero to 25, see pch of plot for details; for \code{Plotter="Plotly"} the size of point}
\item{lwd}{width of the lines}
\item{main}{Optional, string for the title of plot}
\item{mainSize}{Optional, scalar for the size of the title of plot}
\item{SampleSize}{Optional, scalar for the size of sample of vertices that will be plotted}
}
\details{
The points are the vertices of the graph. the adjacency matrix defines the edges. Via adjacency matrix various graphs, like from deldir package, can be used.

Sometimes the amount of vertices is high in which take plotting take very long. For such a case use \code{SampleSize}.
}
\value{
native plot or plotly object depending on input argument \code{Plotter}
}
\references{
Lecture of Knowledge Discovery II
}
\author{
Michael Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pch}}
}
\examples{
N=10
x=runif(N)
y=runif(N)
Euklid=as.matrix(dist(cbind(x,y)))
Radius=quantile(as.vector(Euklid),0.5)
RKugelGraphAdjMatrix = matrix(0, ncol = N, nrow = N)
for (i in 1:N) {
  RInd = which(Euklid[i, ] <= Radius, arr.ind = TRUE)
  RKugelGraphAdjMatrix[i, RInd] = 1
}
PlotGraph2D(RKugelGraphAdjMatrix,cbind(x,y))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

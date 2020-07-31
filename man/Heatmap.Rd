\name{Heatmap}
\alias{Heatmap}

\title{
Heatmap for Clustering
}
\description{
Heatmap of Distances of Data sorted by Cls. Clustering algorithms provide a Classifcation of data,  where the labels are defined as a numeric vector \code{Cls}. Then, a typical cluster-respectively group structure is displayed by the \code{Heatmap} function.
At the margin of the heatmap a dendrogram can be shown, if hierarchical cluster algorithms are used.
Here the dendrogram has to be shown separately and only the heatmap itself is displayed [Wilkinson,2009].
}
\usage{
Heatmap(DataOrDistances,Cls,method='euclidean',

LowLim=0,HiLim,LineWidth=0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistances}{
  if not symmetric, then the function assumes a [1:n,1:d] numeric matrix of n data cases in rows amd d variables in columns. In this case, the distance metric specifed in \code{method} will be used.
	
	Otherwise,
	
	[1:n,1:n] distance matrix that is symmetric
}
  \item{Cls}{
   [1:n] numerical vector of numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers for k clusters that represent the arbitrary labels of the clustering, assuming a descending order of 1 to k. If not ordered please use \code{\link[FCPS]{ClusterRenameDescendingSize}}. Otherwise x and y label will be incorrect.
}

  \item{method}{Optional, 
if \code{DataOrDistances} is a [1:n,1:d] not symmetric numerical matrix, please 
  see \code{\link[parallelDist]{parDist}} for accessible distance methods, default is Euclidean
}
  \item{LowLim}{ Optional: limits for the color axis }
  \item{HiLim}{ Optional: limits for the color axis }
    \item{LineWidth}{Width of lines seperating the clusters in the heatmap}
}
\details{
“Cluster heatmaps are commonly used in biology and related fields to reveal hierarchical clusters in data matrices. Heatmaps visualize a data matrix by drawing a rectangular grid corresponding to rows and columns in the matrix and coloring the cells by their values in the data matrix. In their most basic form, heatmaps have been used for over a century {Wilkinson, 2012 #475}. In addition to coloring cells, cluster heatmaps reorder the rows and/or columns of the matrix based on the results of hierarchical clustering. (…). Cluster heatmaps have high data density, allowing them to compact large amounts of information into a small space {Weinstein, 2008 #1352}.”{Engle, 2017 #1351}.”
For distance matrices, the procedure can be performed as described above. Here, the color scale is chosen such that pixels of low distances have blue and teal colors, pixels of middle distances yellow colors, and pixels of high distances have orange and red colors. The distances are ordered by the clustering and the clusters are divided by black lines. A clustering is valid if the intra-cluster distances are distinctively smaller that inter-cluster distances in the heatmap. For another example, please see {Thrun, 2018 #560} (Fig. 3.7, p. 31).
}
\value{
object of ggplot2
}
\author{
Michael Thrun
}

\seealso{
\code{\link{Pixelmatrix}}
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

[Engle et al., 2017]  Engle, S., Whalen, S., Joshi, A., & Pollard, K. S.: Unboxing cluster heatmaps, BMC bioinformatics, Vol. 18(2), pp. 63. 2017.

[Weinstein, 2008]  Weinstein, J. N.: A postgenomic visual icon, Science, Vol. 319(5871), pp. 1772-1773. 2008.

[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \url{https://doi.org/10.1007/978-3-658-20540-9}, 2018. 
}
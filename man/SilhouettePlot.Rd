\name{Silhouetteplot}
\alias{Silhouetteplot}

\title{
Silhouette plot of classified data.
}
\description{

   Silhouette plot of cluster silhouettes for the n-by-d data
   matrix Data or distance matrix where the clusters are defined in the vector Cls.
}
\usage{
Silhouetteplot(DataOrDistances, Cls, method='euclidean', PlotIt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistances}{  [1:n,1:d] data cases in rows, variables in columns, if not symmetric
	
	or
	
	[1:n,1:n] distance matrix, if symmetric
	}
  \item{Cls}{   numeric vector, [1:n,1]  classified data  }
  \item{method}{Optional if Datamatrix is used, 
       one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". Any unambiguous substring can be given, see \code{dist}
  }
  \item{PlotIt}{Optional, Default:TRUE, FALSE to supress the plot}
}
\value{

\item{silh}{Silhouette values in a N-by-1 vector}

}
\details{
"The Silhouette plot is a common unsupervised index for visual evaluation of a clustering [L. R. Kaufman/Rousseeuw, 2005] [introduced in  [Rousseeuw, 1987]]. A reasonable clustering is characterized by a silhouette width of greater than 0.5, and an average width below 0.2 should be interpreted as indicating a lack of any substantial cluster structure [Everitt et al., 2001, p. 105]. However, it is evident that silhouette scores assume clusters that are spherical or Gaussian in shape [Herrmann, 2011, pp. 91-92]" [Thrun, 2018, p. 29].
}

\author{
Onno Hansen-Goos, Michael Thrun
}
\examples{
data("Lsun3D")
Cls=Lsun3D$Cls
Data=Lsun3D$Data
#clear cluster structure
plot(Data[,1:2],col=Cls)
#However, the silhouette plot does not indicate a very good clustering in cluster 1 and 2
Silhouetteplot(Data,Cls = Cls)

}

\references{

[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.

[Rousseeuw, 1987]   Rousseeuw, Peter J.: Silhouettes: a Graphical Aid to the Interpretation and Validation of Cluster Analysis, Computational and Applied Mathematics, 20, p.53-65, 1987.
}
\keyword{Silhouettes}
\keyword{Silhouette}
\keyword{Silhouette plot}
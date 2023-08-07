\name{ShepardDensityscatter}
\alias{ShepardDensityScatter}
\alias{ShepardDensityPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shepard PDE scatter}
\description{
Draws ein Shepard Diagram (scatterplot of distances) with an two-dimensional PDE density estimation .
}
\usage{

ShepardDensityScatter(InputDists, OutputDists, Plotter= "native", Type = "DDCAL",
DensityEstimation="SDH", Marginals = FALSE, xlab='Input Distances',
ylab='Output Distances',main='ProjectionMethod', sampleSize=500000)
}
\arguments{
  \item{InputDists}{[1:n,1:n] with n cases of data in d variables/features:
Matrix containing the distances of the inputspace.
}
  \item{OutputDists}{[1:n,1:n] with n cases of data in d dimensionalites of the projection method variables/features: 
Matrix containing the distances of the outputspace.
}
  \item{Plotter}{
Optional, either \code{"native"} or \code{"plotly"}
}
  \item{Type}{
Optional, either \code{"DDCAL"} which creates a special hard color transition sensitive to density-based structures or \code{"Standard"} which creates a standard continuous color transition which is proven to be not very sensitive for complex density-based structures.
}
  \item{DensityEstimation}{
Optional, use either \code{"SDH"} or \code{"PDE"} for data density estimation.
}
  \item{Marginals}{
Optional, either TRUE (draw Marginals) or FALSE (do not draw Marginals)
}
    \item{xlab}{
Label of the x axis in the resulting Plot.
}
  \item{ylab}{
Label of the y axis in the resulting Plot.
}
  \item{main}{
Title of the Shepard diagram
}
\item{sampleSize}{Optional, default(500000),  reduces a.ount of data for density estimation, if too many distances given}
}
\details{
Introduced and described in [Thrun, 2018, p. 63] with examples in [Thrun, 2018, p. 71-72]}
  
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN:  978-3-658-20540-9, Heidelberg, 2018.
}
\author{Michael Thrun}
\examples{
data("Lsun3D")
Cls=Lsun3D$Cls
Data=Lsun3D$Data
InputDist=as.matrix(dist(Data))
res = stats::cmdscale(d = InputDist, k = 2, eig = TRUE, 
        add = FALSE, x.ret = FALSE)
\donttest{
ProjectedPoints = as.matrix(res$points)
ShepardDensityScatter(InputDist,as.matrix(dist(ProjectedPoints)),main = 'MDS')
ShepardDensityScatter(InputDist[1:100,1:100],

as.matrix(dist(ProjectedPoints))[1:100,1:100],main = 'MDS')
}
}
\keyword{ShepardDensityPlot}
\keyword{DensityPlot}
\keyword{Shepard}
\keyword{ShepardDiagram}
\keyword{ShepardDensityScatter}
\concept{Shepard Density Scatter}
\concept{Shepard Density Plot}
\concept{Density Estimation}
\concept{scatter plot}
\concept{scatter density plot}
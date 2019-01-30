\name{Sheparddiagram}
\alias{Sheparddiagram}
\title{
Draws a Shepard Diagram
}
\description{
This function plots a Shepard diagram  which is a scatter plot of InputDist and OutputDist
}
\usage{
Sheparddiagram(InputDists, OutputDists, xlab = "Input Distances",

                 ylab= "Output Distances", fancy = F,

				 main = "ProjectionMethod", gPlot = ggplot())
}

\arguments{
  \item{InputDists}{[1:n,1:n] with n cases of data in d variables/features: 
Matrix containing the distances of the inputspace.
}
  \item{OutputDists}{[1:n,1:n] with n cases of data in d dimensionalites of the projection method variables/features: 
Matrix containing the distances of the outputspace.
}
  \item{xlab}{
Label of the x axis in the resulting Plot.
}
  \item{ylab}{
Label of the y axis in the resulting Plot.
}
  \item{fancy}{
Set FALSE for PC and TRUE for publication
}
  \item{main}{
Title of the Shepard diagram
}
  \item{gPlot}{
ggplot2 object to plot upon.
}
}
\value{
ggplot2 object containing the plot.
}
\author{
Michael Thrun
}
\examples{
data("Lsun3D")
Cls=Lsun3D$Cls
Data=Lsun3D$Data
InputDist=as.matrix(dist(Data))
res = stats::cmdscale(d = InputDist, k = 2, eig = TRUE, 
        add = FALSE, x.ret = FALSE)
ProjectedPoints = as.matrix(res$points)
\donttest{

Sheparddiagram(InputDist,as.matrix(dist(ProjectedPoints)),main = 'MDS')
}
\dontshow{

Sheparddiagram(InputDist[1:100,1:100],as.matrix(dist(ProjectedPoints))[1:100,1:100],main = 'MDS')
}
}

\keyword{ShepardScatterPlot}
\keyword{ScatterPlot}
\keyword{Shepard}
\keyword{ShepardDiagram}
\keyword{Shepard diagram}
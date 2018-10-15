\name{DataVisualizations-package}
\alias{DataVisualizations-package}
\alias{DataVisualizations}
\docType{package}
\title{
\packageTitle{DataVisualizations}
}
\description{
\packageDescription{DataVisualizations}
}
\details{
For a brief introduction to \pkg{DataVisualizations} please see the vignette \href{../doc/DataVisualizations.html}{A Quick Tour in Data Visualizations}.

Please see \url{http://www.deepbionics.org/}. 
Depending on the context please cite either [Thrun, 2018] regarding visualizations in the context of clustering or [Thrun/Ultsch, 2018] for other visualizations.

For the Mirrored Density Plot (PDE optimized violin plot) please cite [Thrun et al.,2018].

\packageIndices{DataVisualizations}
}
\author{
Michael Thrun, Felix Pape, Onno Hansen-Goos, Alfred Ultsch

Maintainer: \packageMaintainer{DataVisualizations}
}
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \url{https://doi.org/10.1007/978-3-658-20540-9}, 2018. 

[Thrun/Ultsch, 2018] Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 

[Thrun et al.,2018]	Thrun, M. C., Pape, F., & Ultsch, A. : Benchmarking Cluster Analysis Methods using PDE-Optimized Violin Plots, Proc. European Conference on Data Analysis (ECDA), Paderborn, Germany, 2018. 
}
\examples{


data("Lsun3D")
Data=Lsun3D$Data
\donttest{
PixelMatrixPlot(Data)
}

\donttest{
InspectDistances(as.matrix(dist(Data)))
}

data("ITS")
data("MTY")
Inds=which(ITS<900&MTY<8000)
plot(ITS[Inds],MTY[Inds],main='Bimodality is not visible in normal scatter plot')
\donttest{
PDEscatter(ITS[Inds],MTY[Inds],xlab = 'ITS in EUR',

ylab ='MTY in EUR' ,main='Pareto Density Estimation indicates Bimodality' )
}

MAlist=MAplot(ITS,MTY)

data("Lsun3D")
Cls=Lsun3D$Cls
Data=Lsun3D$Data
#clear cluster structure
plot(Data[,1:2],col=Cls)
#However, the silhouette plot does not indicate a very good clustering in cluster 1 and 2
\donttest{
SilhouettePlot(Data,Cls = Cls)
}
\donttest{
Heatmap(as.matrix(dist(Data)),Cls = Cls)
}
}

\keyword{clustering}
\keyword{visualization}
\keyword{Dimensionality Reduction}
\keyword{projection}
\keyword{Pie chart}
\keyword{slope chart}
\keyword{fanplot}
\keyword{Bland-Altman plot}
\keyword{PDE}
\keyword{violin}
\keyword{violin plot}
\keyword{beanplot}
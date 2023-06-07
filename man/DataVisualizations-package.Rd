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

Please see \url{https://www.deepbionics.org/}. 
Depending on the context please cite either [Thrun, 2018] regarding visualizations in the context of clustering or [Thrun/Ultsch, 2018] for other visualizations.

For the Mirrored Density Plot (MD plot) please cite [Thrun et al., 2020] and see the extensive vignette in \url{https://md-plot.readthedocs.io/en/latest/index.html}. The MD plot is also available in Python \url{https://pypi.org/project/md-plot/}

\packageIndices{DataVisualizations}
}
\author{
Michael Thrun, Felix Pape, Onno Hansen-Goos, Alfred Ultsch

Maintainer: \packageMaintainer{DataVisualizations}
}
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \doi{10.1007/978-3-658-20540-9}, 2018. 

[Thrun/Ultsch, 2018] Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 

[Thrun et al., 2020]	Thrun, M. C., Gehlert, T. & Ultsch, A.: Analyzing the Fine Structure of Distributions, PLoS ONE, Vol. 15(10), pp. 1-66, DOI 10.1371/journal.pone.0238835, 2020. 

}
\examples{


data("Lsun3D")
Data=Lsun3D$Data
\donttest{
Pixelmatrix(Data)
}

\donttest{
InspectDistances(as.matrix(dist(Data)))
}

MAlist=MAplot(ITS,MTY)

data("Lsun3D")
Cls=Lsun3D$Cls
Data=Lsun3D$Data
#clear cluster structure
plot(Data[,1:2],col=Cls)
#However, the silhouette plot does not indicate a very good clustering in cluster 1 and 2
\donttest{
Silhouetteplot(Data,Cls = Cls)
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
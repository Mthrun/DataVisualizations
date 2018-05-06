\name{DataVisualizations-package}
\alias{DataVisualizations-package}

\docType{package}
\title{
The Data Visualizations Package
}
\description{
'DataVisualizations' contains a collection of various visualizations methods. 
With regards to high-dimensional data the heat map and silhouette plot for grouped data,
 visualizations of the distribution of distances, the scatter-density plot for two variables,
 the Shepard density plot as well as the Bland-Altman plot are presented here. 
 For a classification of countries, a map of the world or Germany can be visualized. 
 'DataVisualizations' makes it possible to inspect the distribution of 
 each feature of a dataset visually through the combination of four methods.
 More detailed explanations can be found in the book of 
 Thrun, M.C.:"Projection-Based Clustering through Self-Organization and Swarm Intelligence" (2018) <DOI:10.1007/978-3-658-20540-9>. 
 Furthermore, for categorical features the ABCanalysis improved Pie charts, 
 slope charts and fan plots are usable. Additionally, for measurements across 
 a geographic area an easy to use function for a Choropleth map is presented here.


}
\details{

Please see \url{http://www.deepbionics.org/}. 
Depending on the context please cite either [Thrun, 2018] regarding visualizations in the context of clustering or [Thrun/Ultsch, 2018] for other visualizations.
}
\note{
In the next version I will try to add choroplethr based country maps using german zip codes. As well as a world map of countries colored by classification labels.
}
\author{
Michal Thrun

Maintainer: Michael Thrun <m.thrun@gmx.net>
}
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \url{https://doi.org/10.1007/978-3-658-20540-9}, 2018. 

[Thrun/Ultsch, 2018] Thrun, M. C. & Ultsch A. : Effects of the payout system of income taxes to municipalities in Germany, 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, Foundation of the Cracow University of Economics, Zakopane, Poland, 2018. 

}
\examples{
data(categoricalVariable)
fanPlot(categoricalVariable)

pieChart(categoricalVariable)

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

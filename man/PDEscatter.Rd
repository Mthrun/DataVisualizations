\name{PDEscatter}
\alias{PDEscatter}
\title{Scatter Density Plot}
\description{
Pareto density estimation (PDE) [Ultsch, 2005] used for a scatter density plot. 
}
\usage{
PDEscatter(x,y,na.rm=FALSE,paretoRadius=0,sampleSize=5000,

NrOfContourLines=20,Plotter='native', DrawTopView = T,

xlab="X", ylab="Y", main="PDEscatter",

xlim, ylim, Legendlab_ggplot="value")
}
\details{
The \code{PDEscatter} function generates the density of the xy data as a z coordinate. Afterwards xyz will be plotted either as a contour plot or a 3d plot. 
This function plots the PDE on top of a scatterplot. Variances of x and y should not differ by extreme numbers, otherwise calculate the percentiles on both first.
The method was succesfully used in [Thrun, 2018; Thrun/Ultsch 2018].
}
\arguments{
  \item{x}{Numeric vector, first feature (for x axis values)}
  \item{y}{Numeric vector, second feature (for y axis values)}
  \item{na.rm}{Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE}
  \item{paretoRadius}{Numeric, the Pareto Radius. If omitted (or 0), calculate by paretoRad.}
  \item{sampleSize}{Numeric, maximum size of the sample used for calculation. High values increase runtime significantly.}
  \item{NrOfContourLines}{Numeric, number of contour lines to be drawn. 20 by default.}
  \item{Plotter}{String, name of the plotting backend to use. Possible values are: "native", "ggplot", "plotly"}
  \item{DrawTopView}{Boolean, True means contur is drawn, otherwise a 3D plot is drawn. Default: True}
    \item{xlab}{String, title of the x axis. Default: "X",   see \code{plot()} function}
  \item{ylab}{String, title of the y axis. Default: "Y",   see \code{plot()} function}
    \item{main}{
    string, the same as "main" in \code{plot()} function
  }
  \item{xlim}{
  see \code{plot()} function
  }
    \item{ylim}{
    see \code{plot()} function
  }
    \item{Legendlab_ggplot}{String, in case of \code{Plotter="ggplot"} label for the legend. Default: "value"}
}
\note{
If \code{DrawTopView=FALSE} only the plotly option is currently available. If another option is chosen, the method switches automatically there.
}

\value{
 List of:
  \item{AnzInPSpheres}{Number of points within the ParetoRadius of each point}
  \item{ParetoRadius}{ParetoRadius used for PDEscatter}
  \item{Handle}{Handle of the plot object. Information-string if native R plot is used.}

}
\references{

[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, (Ultsch, A. & Huellermeier, E. Eds., 10.1007/978-3-658-20540-9), Doctoral dissertation, Heidelberg, Springer, ISBN: 978-3658205393, 2018.

[Thrun/Ultsch, 2018]  Thrun, M. C., & Ultsch, A.: Effects of the payout system of income taxes to municipalities in Germany, 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, Vol. accepted, Foundation of the Cracow University of Economics, Zakopane, Poland, 2018.

[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, In Baier, D. & Werrnecke, K. D. (Eds.), Innovations in classification, data science, and information systems, (Vol. 27, pp. 91-100), Berlin, Germany, Springer, 2005.


}

\examples{
#taken from [Thrun/Ultsch, 2018]
data("ITS")
data("MTY")
Inds=which(ITS<900&MTY<8000)
plot(ITS[Inds],MTY[Inds],main='Bimodality is not visible in normal scatter plot')
\donttest{
PDEscatter(ITS[Inds],MTY[Inds],xlab = 'ITS in EUR',

ylab ='MTY in EUR' ,main='Pareto Density Estimation indicates Bimodality' )
}
\dontshow{

PDEscatter(ITS[1:10],MTY[1:10],xlab = 'ITS in EUR',

ylab ='MTY in EUR' ,main='Pareto Density Estimation indicates Bimodality' )
}
}

\author{Felix Pape}
\keyword{scatter}
\keyword{scatter plot}
\keyword{scatter density plot}
\keyword{PDE}
\keyword{Density Estimation}
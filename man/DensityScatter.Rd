\name{DensityScatter}
\alias{DensityScatter}
\title{Scatter Density Plot}
\description{
Density estimation (PDE) [Ultsch, 2005] or "SDH" [Eilers/Goeman, 2004] used for a scatter density plot. 
}
\usage{
DensityScatter(X,Y, DensityEstimation="SDH", 

SampleSize, na.rm=FALSE,PlotIt=TRUE,
                              
NrOfContourLines=20,Plotter='native', DrawTopView = TRUE,
                              
xlab, ylab, main="DensityScatter",
                              
xlim, ylim, Legendlab_ggplot="value",

AddString2lab="",NoBinsOrPareto=NULL,\dots)
}
\details{
The \code{DensityScatter} function generates the density of the xy data as a z coordinate. Afterwards xyz will be plotted either as a contour plot or a 3d plot. It assumens that the cases of x and y are mapped to each other meaning that a \code{cbind(x,y)} operation is allowed.
This function plots the Density on top of a scatterplot. Variances of x and y should not differ by extreme numbers, otherwise calculate the percentiles on both first. If \code{DrawTopView=FALSE} only the plotly option is currently available. If another option is chosen, the method switches automatically there.

\code{PlotIt=FALSE} is usefull if one likes to perform adjustements like axis scaling prior to plotting with \pkg{ggplot2} or  \pkg{plotly}. In the case of "\code{native}"" the handle returns \code{NULL} because the basic R functon \code{plot}() is used
}
\arguments{
  \item{X}{Numeric vector [1:n], first feature (for x axis values)}
  \item{Y}{Numeric vector [1:n], second feature (for y axis values)}
   \item{DensityEstimation}{\code{"SDH"} is very fast but maybe not correct, \code{"PDE"} is slow but proably more correct, third alternativ is the typical R density estimation with \code{"kde2d"} which is sensitive to parameters}
   
  \item{SampleSize}{Numeric, positiv scalar, maximum size of the sample used for calculation. High values increase runtime significantly. The default is that no sample is drawn}
  \item{na.rm}{Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE}

    \item{PlotIt}{\code{TRUE}: plots with function call
    
    \code{FALSE}: Does not plot, plotting can be done using the list element \code{Handle}
    }

  \item{NrOfContourLines}{Numeric, number of contour lines to be drawn. 20 by default.}
  \item{Plotter}{String, name of the plotting backend to use. Possible values are: "\code{native}", "\code{ggplot}", "\code{plotly}"}
  \item{DrawTopView}{Boolean, True means contur is drawn, otherwise a 3D plot is drawn. Default: TRUE}
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
        \item{AddString2lab}{adds the same string of information to x and y axis label, e.g. usefull for adding SI units}
      \item{NoBinsOrPareto}{Density specifc parameters, for \code{PDEscatter(ParetoRadius)} or SDH (nbins)) or kde2d(bins)}
      
      \item{\dots}{further plot arguments}
}
\note{
MT contributed with several adjustments
}

\value{
 List of:
   \item{X}{Numeric vector [1:m],m<=n, first feature used in the plot or the kernels used}
  \item{Y}{Numeric vector [1:m],m<=n, second feature used in the plot or the kernels used}
  \item{Densities}{Number of points within the ParetoRadius of each point, i.e. density information}
  \item{Handle}{Handle of the plot object. Information-string if native R plot is used.}

}
\references{

[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, (Ultsch, A. & Huellermeier, E. Eds., 10.1007/978-3-658-20540-9), Doctoral dissertation, Heidelberg, Springer, ISBN: 978-3658205393, 2018.

[Thrun/Ultsch, 2018] Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 

[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, In Baier, D. & Werrnecke, K. D. (Eds.), Innovations in classification, data science, and information systems, (Vol. 27, pp. 91-100), Berlin, Germany, Springer, 2005.

[Eilers/Goeman, 2004]  Eilers, P. H., & Goeman, J. J.: Enhancing scatterplots with smoothed densities, Bioinformatics, Vol. 20(5), pp. 623-628. 2004.


}

\examples{
#taken from [Thrun/Ultsch, 2018]
data("ITS")
data("MTY")
Inds=which(ITS<900&MTY<8000)
plot(ITS[Inds],MTY[Inds],main='Bimodality is not visible in normal scatter plot')
\donttest{
DensityScatter(ITS[Inds],MTY[Inds],DensityEstimation="SDH",xlab = 'ITS in EUR',

ylab ='MTY in EUR' ,main='Smoothed Densities histogram indicates Bimodality' )

DensityScatter(ITS[Inds],MTY[Inds],DensityEstimation="PDE",xlab = 'ITS in EUR',

ylab ='MTY in EUR' ,main='PDE indicates Bimodality' )
}
\dontshow{

DensityScatter(ITS[1:10],MTY[1:10],xlab = 'ITS in EUR',

ylab ='MTY in EUR' ,main='Pareto Density Estimation indicates Bimodality' )
}
}

\author{Felix Pape}

\keyword{scatter}
\keyword{PDE}
\keyword{SDH}
\concept{Density Estimation}
\concept{scatter plot}
\concept{scatter density plot}
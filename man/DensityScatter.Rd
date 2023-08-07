\name{DensityScatter}
\alias{DensityScatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scatter plot with densities
}
\description{
Density estimation is performed by (PDE) [Ultsch, 2005] or "SDH" [Eilers/Goeman, 2004] and visualized in a  density scatter plot [Brinkmann et al., 2023] in which the points are colored by their density.
}

\usage{
DensityScatter(X,Y,DensityEstimation="SDH",

Type="DDCAL", Plotter = "native",Marginals = FALSE,

SampleSize,na.rm=FALSE, xlab, ylab, 

main="DensityScatter", AddString2lab="",
                        
xlim, ylim,NoBinsOrPareto=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Numeric vector [1:n], first feature (for x axis values)
}
  \item{Y}{
Numeric vector [1:n], second feature (for y axis values)
}
  \item{DensityEstimation}{
  (Optional), \code{"SDH"} is very fast but maybe not correct, \code{"PDE"} is slow but proably more correct, third alternativ is the typical R density estimation with \code{"kde2d"} which is sensitive to parameters
}
  \item{Type}{
(Optional), \code{"DDCAL"} uses a new density to point color matching by DDCAL algorithm [Lux/Rinderle-Ma, 2023] , \code{"native"} uses a simple density to point color matching
}
 \item{Plotter}{
in case of \code{Type="DDCAL"}, (Optional) String, name of the plotting backend to use. Possible values are: "\code{native}","\code{plotly}", or "\code{ggplot2}"
}
  \item{Marginals}{
(Optional) Boolean, if TRUE the marginal distributions of X and Y will be plotted together with the 2D density of X and Y. Default is FALSE
}
  \item{SampleSize}{
(Optional), Numeric, positiv scalar, maximum size of the sample used for calculation. High values increase runtime significantly. The default is that no sample is drawn
}
  \item{na.rm}{
(Optional), Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE
}

  \item{xlab}{
(Optional), String, title of the x axis. Default: "X",   see \code{plot()} function
}
  \item{ylab}{
(Optional), String, title of the y axis. Default: "Y",   see \code{plot()} function
}
  \item{main}{
(Optional), string, the same as "main" in \code{plot()} function
}

  \item{AddString2lab}{
(Optional), adds the same string of information to x and y axis label, e.g. usefull for adding SI units
}
  \item{xlim}{
(Optional), in case of \code{Type="natuive"}, see \code{plot()} function
}
  \item{ylim}{
in case of \code{Type="natuive"}, see \code{plot()} function
}
  \item{NoBinsOrPareto}{
(Optional), in case of \code{Type="natuive"}, Density specifc parameters, for \code{PDEscatter(ParetoRadius)} or SDH (nbins)) or kde2d(bins)
}
  \item{\dots}{
(Optional), further arguments either to ScatterDenstiy::DensityScatter.DDCAL or to plot()
}
}
\details{
The \code{DensityScatter} function generates the density of the xy data as a z coordinate. Afterwards xy points will be plotted as a scatter plot, where the z values defines the coloring of the xy points. It assumens that the cases of x and y are mapped to each other meaning that a \code{cbind(x,y)} operation is allowed.
This function plots the Density on top of a scatterplot. Variances of x and y should not differ by extreme numbers, otherwise calculate the percentiles on both first.

}
\value{
 List of:
  \item{X}{Numeric vector [1:m],m<=n, first feature used in the plot or the kernels used}
  \item{Y}{Numeric vector [1:m],m<=n, second feature used in the plot or the kernels used}
  \item{Densities}{Number of points within the ParetoRadius of each point, i.e. density information}
}
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, (Ultsch, A. & Huellermeier, E. Eds., 10.1007/978-3-658-20540-9), Doctoral dissertation, Heidelberg, Springer, ISBN: 978-3658205393, 2018.

[Thrun/Ultsch, 2018] Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 

[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, In Baier, D. & Werrnecke, K. D. (Eds.), Innovations in classification, data science, and information systems, (Vol. 27, pp. 91-100), Berlin, Germany, Springer, 2005.

[Eilers/Goeman, 2004]  Eilers, P. H., & Goeman, J. J.: Enhancing scatterplots with smoothed densities, Bioinformatics, Vol. 20(5), pp. 623-628. 2004

[Lux/Rinderle-Ma, 2023]  Lux, M. & Rinderle-Ma, S.: DDCAL: Evenly Distributing Data into Low Variance Clusters Based on Iterative Feature Scaling, Journal of Classification vol. 40, pp. 106-144, 2023.

[Brinkmann et al., 2023]  Brinkmann, L., Stier, Q., & Thrun, M. C.: Computing Sensitive Color Transitions for the Identification of Two-Dimensional Structures, Proc. Data Science, Statistics & Visualisation (DSSV) and the European Conference on Data Analysis (ECDA), p.109, Antwerp, Belgium, July 5-7, 2023. 
}
\author{
Felix Pape
}
\note{
MT contributed with several adjustments
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
DensityScatter(ITS[1:10],MTY[1:10],xlab = 'ITS in EUR',Type="native",

ylab ='MTY in EUR' ,main='Pareto Density Estimation indicates Bimodality' )
}
}


\keyword{scatter}
\keyword{PDE}
\keyword{SDH}
\concept{Density Estimation}
\concept{scatter plot}
\concept{density plot}
\concept{scatter density plot}

\name{InspectCorrelation}
\alias{InspectCorrelation}

\title{
Inspect the Correlation
}
\description{
Inspects the correlation between two given features using density scatter plots.
}
\usage{
InspectCorrelation(X, Y, DensityEstimation = "SDH",

CorMethod = "spearman", na.rm = TRUE,

SampleSize = round(sqrt(5e+08), -3),

NrOfContourLines = 20, Plotter = "native",

DrawTopView = T, xlab, ylab,

main = "Spearman correlation coef.:", xlim, ylim, 

Legendlab_ggplot = "value", ...)
}

\arguments{
 \item{X}{Numeric vector [1:n], first feature (for x axis values)}
  \item{Y}{Numeric vector [1:n], second feature (for y axis values)}
   \item{DensityEstimation}{"SDH" is very fast but maybe not correct, "PDE" is slow but proably more correct.}
   
  \item{CorMethod}{
method of correlation of the cor function, One of "pearson" (default), "kendall", or "spearman
}
  \item{SampleSize}{Numeric, positiv scalar, maximum size of the sample used for calculation. High values increase runtime significantly. The default is that no sample is drawn}
  \item{na.rm}{Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE}
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
    
      \item{\dots}{Density specifc parameters, for \code{PDEscatter()} or SDH (nbins,lambda,Xkernels,Ykernel))}
}

\details{
Example shows that features with high correlation coefficient do not correlate because of bimodality.
}
\value{
plotting handler
}
\references{

[Thrun/Ultsch, 2018] Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 
}
\author{
Michael Thrun
}

\seealso{
\code{\link{DensityScatter}}
}
\examples{
data(ITS)
data(MTY)
Inds=which(ITS<900&MTY<8000)
\donttest{
InspectCorrelation(ITS[Inds],MTY[Inds])
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cor}% use one of  RShowDoc("KEYWORDS")
\keyword{correlation}% __ONLY ONE__ keyword per line

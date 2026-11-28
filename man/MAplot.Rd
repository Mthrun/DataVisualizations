\name{MAplot}
\alias{MAplot}
\title{
Minus versus Add plot
}
\description{
Bland-Altman plot [Altman/Bland, 1983].
}
\usage{
MAplot(X,Y,islog=TRUE,LoA=FALSE,CI=FALSE,

densityplot=FALSE,main,xlab,ylab,

Cls,lwd=2,ylim=NULL,\dots)
}
\arguments{
  \item{X}{[1:n] numerical vector of a feature/variable
}
  \item{Y}{[1:n] another numerical vector of a feature/variable
}
  \item{islog}{Optional, TRUE: MAplot, FALSE: M=x-y versus a=0.5(x+y)
}
  \item{LoA}{Optional, if TRUE: limits of agreement are plottet as lines if densityplot=FALSE
}
  \item{CI}{Optional, if TRUE: confidence intervals for LoA, see [Stockl et al., 2004], if  densityplot=FALSE
}
  \item{densityplot}{Optional, FALSE: Scatterplot using \code{ \link{Classplot}}, TRUE: density scatter plot with \code{\link{DensityScatter}}
}
  \item{main}{Optional, see \code{plot}
}  
  \item{xlab}{Optional, see \code{plot}
}
  \item{ylab}{
  Optional, see \code{plot}
}
  \item{Cls}{
	Optional, prior Classification as a numeric vector.
}
  \item{lwd}{
	Optional, if \code{LoA=TRUE} or \code{CI=TRUE} the width of the lines, otherwise input argument is ignored
}
  \item{ylim}{
	Optional, default \code{=NULL} sets this parameter automatically, otherwise see \code{Classplot}.
}

  \item{\dots}{
for example, \code{ylim}, Please see either\code{ \link{Classplot}} in the mode \code{Plotter="native"}, or \code{\link{DensityScatter}} for further arguments depending on \code{densityplot}, see also details
}

}
\details{
For \code{islog=FALSE} and \code{densityplot=FALSE}, the vertical coordinate is
\eqn{M = X - Y} and the horizontal coordinate is \eqn{A = 0.5(X + Y)}.
A positive difference means that \code{X} exceeds \code{Y}.
Consequently, \code{MAplot(X=Reference, Y=Prediction, islog=FALSE)} displays
reference minus prediction. To display prediction minus reference, pass
\code{X=Prediction} and \code{Y=Reference}. Custom axis labels do not change
this calculation.

The returned columns \code{MA[, "M_y"]} and \code{MA[, "A_x"]} contain these
differences and means. When \code{LoA=TRUE} or \code{CI=TRUE},
\code{Statistics$Mean_value} uses the same difference direction. Swapping
\code{X} and \code{Y} negates the differences and their mean, leaves the means
on the horizontal axis and the standard deviation unchanged, and exchanges
the negated lower and upper limits of agreement.
With \code{islog=TRUE}, the default, \code{\link{SignedLog}} is applied to each
input before calculating the difference and mean in the same argument order.

Bland-Altman plot [Altman/Bland, 1983] for visual representation of genomic data or in order to decorrelate data.

"The limits of agreement (LoA) are defined as the mean difference +- 1.96 SD of differences. If these limits do not exceed the maximum allowed difference between methods (the differences within mean +- 1.96 SD are not clinically important), the two methods are considered to be in agreement and may be used interchangeably." cited as in URL. Please note, that the underyling assumption is the normal distribution of the differences. Input argument \code{LoA=TRUE} shows the mean of the difference in blue and +- 1.96 SD in green. Input argument \code{CI=TRUE} shows the mean of the difference in blue and the confidence intervall as red dashed lines similar to the cited URL.

In case of \code{densityplot=FALSE}, the function \code{ \link{Classplot}} is always called with \code{Plotter="native"}. Then, the input argument "\code{Colors}"" of points can only be set in \code{ \link{Classplot}} if "\code{Cls}"" is given in this function, otherwise the points are always black. The input argument "\code{Size}"" sets the size of points in \code{\link{Classplot}}. 
}
\value{
  \item{MA}{[1:n,2] Matrix of Minus component of two features and Add component of two features
}
  \item{Handle}{see \code{\link{DensityScatter}} for output options, if densityplot=TRUE, otherwise NULL
}
  \item{Statistics}{Named list of four element, each consisting of one value depending on input parameters \code{LoA} and \code{CI}, of this function. If not specifically set each list element is \code{NULL}. The elements are  \code{Mean_value} - mean allowed difference,  \code{SD_value} - standard deviation of difference,  \code{LoA_value} - Limits of agreement=1.96*SD,  \code{CI_value} - confidence intervall, i.e., maximum allowed difference
}
}
\references{
[Altman/Bland, 1983]  Altman D.G., Bland J.M.: Measurement in medicine: the analysis of method comparison studies, The Statistician, Vol. 32, p. 307-317, doi:10.2307/2987937, 1983.

\url{https://www.medcalc.org/en/manual/bland-altman-plot.php}

[Stockl et al., 2004]  Stockl, D., Rodriguez Cabaleiro, D., Van Uytfanghe, K., & Thienpont, L. M.: Interpreting method comparison studies by use of the Bland-Altman plot: reflecting the importance of sample size by incorporating confidence limits and predefined error limits in the graphic, Clinical chemistry, Vol. 50(11), pp. 2216-2218. 2004.
}
\author{
Michael Thrun
}
\examples{
data("ITS")
data("MTY")
MAlist=MAplot(ITS,MTY)
}
\keyword{MAplot}
\keyword{MA}
\keyword{MA plot}
\keyword{Bland-Altman plot}
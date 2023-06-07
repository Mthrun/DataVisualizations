
\name{ParetoDensityEstimation}
\alias{ParetoDensityEstimation}
\title{Pareto Density Estimation V3}
\usage{
ParetoDensityEstimation(Data, paretoRadius, kernels = NULL,
  MinAnzKernels = 100,PlotIt=FALSE,Silent=FALSE)
}
\arguments{
\item{Data}{[1:n] numeric vector of data.}

\item{paretoRadius}{Optional scalar, numeric value, see \code{\link{ParetoRadius}}.If not given it is estimated internally. Please do not set manually}

\item{kernels}{Optional,[1:m] numeric vector data values where pareto density is measured at. If 0 (by default) kernels will be computed.}

\item{MinAnzKernels}{Optional, minimal number of kernels, default MinAnzKernels==100}
  \item{PlotIt}{Optional, if TRUE: raw basic r plot of density estimation of debugging purposes. Usually please use \pkg{ggplot2} interface via \code{\link{PDEplot}} or \code{\link{MDplot}}}
  
    \item{Silent}{Optional, if TRUE: disables all warnings}
}
\value{
List With 
\describe{
  \item{kernels}{[1:m] numeric vector. data values at with Pareto Density is measured.}
  \item{paretoDensity}{[1:m] numeric vector containing the determined density by \code{paretoRadius}.}
  \item{paretoRadius}{numeric value of defining the radius}
 \item{kernels_internal}{Either NULL or internally estimated [1:p] numeric vector of kernels if input argument \code{kernels} was set by the user}
  \item{paretoDensity_internal}{Either NULL or internally estimated density if input argument \code{kernels} was set by the user}
}
}
\description{
This function estimates the Pareto Density for the distribution of one variable. In the default setting the functions estimates internally the appropriate number and position of kernels to estimate the density properly. However, the user can set the \code{kernels} manually. In this case density will only be estimated only around these values even if data exists outside the range of kernels or the internally estimated \code{paretoRadius} does not contain all datapoints between each kernel. See example for details.
}
\details{
Pareto Density Estimation (PDE) is a method for the estimation of probability density functions using hyperspheres. The Pareto-radius of the hyperspheres is derived from the optimization of information for minimal set size. It is shown, that Pareto Density is the best estimate for clusters of Gaussian structure. The method is shown to be robust when cluster overlap and when the variances differ across clusters.
This is the best density estimation to judge Gaussian Mixtures of the data see [Ultsch 2003].

If input argument \code{kernels} is set manually the output arguments \code{paretoDensity_internal} and \code{kernels_internal} provide the internally estimated density and kernels. Otherwise these arguments are NULL. The function provides a message if range of kernels and range of data does not overlap completly.

Typically it is not advisable to set \code{paretoRadius} manually. However in specific cases, the function \code{\link{ParetoRadius}} is used prior to calling this function. In such cases the input argument can use a priorly estimated \code{paretoRadius}.
}

\note{
This the second version of the function prior available in \pkg{AdaptGauss}
}
\examples{
   \donttest{
   #kernels are estimated internally
   data = c(rnorm(1000),rnorm(2000)+2,rnorm(1000)*2-1)
   pdeVal        <- ParetoDensityEstimation(data)
   plot(pdeVal$kernels,pdeVal$paretoDensity,type='l',xaxs='i',
   yaxs='i',xlab='Data',ylab='PDE')
   
   ##data exist outside of the range kernels
   kernels=seq(from=-3,to=3,by=0.01) 
   pdeVal        <- ParetoDensityEstimation(data,  kernels=kernels)
   plot(pdeVal$kernels,pdeVal$paretoDensity,type='l',xaxs='i',
   yaxs='i',xlab='Data',ylab='PDE')
   
   #data exists in-between kernels that is not measured
   pdeVal$paretoRadius#0.42
   kernels=seq(from=-8,to=8,by=1)
   pdeVal        <- ParetoDensityEstimation(data,  kernels=kernels)
   plot(pdeVal$kernels,pdeVal$paretoDensity,type='l',xaxs='i',
   yaxs='i',xlab='Data',ylab='PDE')
   }
   \dontshow{
     data = rnorm(100)
   pdeVal        <- ParetoDensityEstimation(data)
   }
}
\references{
Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.
}
\seealso{

\code{\link{ParetoRadius}}

\code{\link{PDEplot}}

\code{\link{MDplot}}

}
\author{
Michael Thrun
}

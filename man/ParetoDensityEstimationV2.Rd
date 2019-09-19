
\name{ParetoDensityEstimationV2}
\alias{ParetoDensityEstimationV2}
\title{Pareto Density EstimationV2}
\usage{
ParetoDensityEstimationV2(Data, paretoRadius, kernels = NULL,
  MinAnzKernels = 100,PlotIt=FALSE)
}
\arguments{
\item{Data}{numeric vector of data.}

\item{paretoRadius}{Optional, numeric value, see \code{\link{ParetoRadiusV2}}, Please do not set manually}

\item{kernels}{Optional, numeric vector. data values where pareto density is measured at. If 0 (by default) kernels will be computed.}

\item{MinAnzKernels}{Optional, minimal number of kernels, default MinAnzKernels==100}
  \item{PlotIt}{Optional, if TRUE: raw basic r plot of density estimation of debugging purposes. Usually please use \pkg{ggplot2} interface via \code{\link{PDEplot}} or \code{\link{MDplot}}}
}
\value{
List With 
\describe{
  \item{kernels}{numeric vector. data values at with Pareto Density is measured.}
  \item{paretoDensity}{numeric vector containing the determined density by ParetoRadius.}
  \item{paretoRadius}{numeric value of defining the radius}

}
}
\description{
This function estimates the Pareto Density for the distribution of one variable.
}
\details{
Pareto Density Estimation (PDE) is a method for the estimation of probability density functions using hyperspheres. The Pareto-radius of the hyperspheres is derived from the optimization of information for minimal set size. It is shown, that Pareto Density is the best estimate for clusters of Gaussian structure. The method is shown to be robust when cluster overlap and when the variances differ across clusters.
This is the best density estimation to judge Gaussian Mixtures of the data see [Ultsch 2003]
}

\note{
This the second version of the function prior available in \pkg{AdaptGauss}
}
\examples{

   data = c(rnorm(1000),rnorm(2000)+2,rnorm(1000)*2-1)
   pdeVal        <- ParetoDensityEstimationV2(data)
   plot(pdeVal$kernels,pdeVal$paretoDensity,type='l',xaxs='i',
   yaxs='i',xlab='Data',ylab='PDE')
   


}
\references{
Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.
}
\seealso{

\code{\link{ParetoRadiusV2}}

\code{\link{PDEplot}}

\code{\link{MDplot}}

}
\author{
Michael Thrun
}

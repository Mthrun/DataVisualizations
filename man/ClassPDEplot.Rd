\name{ClassPDEplot}
\alias{ClassPDEplot}
\title{
PDE Plot for all classes
}
\description{
PDEplot the data for all classes, weights the pdf with priors
}
\usage{
ClassPDEplot(Data, Cls, ColorSequence,

 ColorSymbSequence, PlotLegend = 1,

 SameKernelsAndRadius = 0, xlim, ylim, \dots)
}
\arguments{
  \item{Data}{
  The Data to be plotted
}
  \item{Cls}{
  Vector of class identifiers. Can be integers or NaN's, need not be consecutive nor positive
}
  \item{ColorSequence}{
  Optional: the sequence of colors used, Default: DefaultColorSequence
}
  \item{ColorSymbSequence}{
  Optional: the plot symbols used (theoretisch nicht notwendig, da erst wichtig, wenn mehr als 562 Cluster)
}
  \item{PlotLegend}{
  Optional: add a legent to plot (default == 1)
}
  \item{SameKernelsAndRadius}{
  Optional: Use the same PDE kernels and radii for all distributions (default == 0)
}
  \item{xlim}{
  Optional: range of the x axis
}
  \item{ylim}{
  Optional: range of the y axis
}
  \item{\dots}{
  further arguments passed to plot  
}
}
\value{
  Kernels of the Pareto density estimation in mode \code{invisible}
}
\author{
  Michael Thrun
}

\examples{
\dontshow{
# sampling is online done
# because otherwise the example takes too long
# in the CRAN check
data(ITS)
ind=sample(length(ITS),1000)

#clustering only for testing
Classification=c(rep(1,8805),rep(2,2389))
DataVisualizations::ClassPDEplot(ITS[ind],Classification[ind])
}

\donttest{
data(ITS)
model=AdaptGauss::AdaptGauss(ITS)
Classification=AdaptGauss::ClassifyByDecisionBoundaries(ITS,

DecisionBoundaries = AdaptGauss::BayesDecisionBoundaries(model$Means,model$SDs,model$Weights))

DataVisualizations::ClassPDEplot(ITS,Classification)$ggobject
}
}
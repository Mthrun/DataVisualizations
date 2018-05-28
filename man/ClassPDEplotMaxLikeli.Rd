\name{ClassPDEplotMaxLikeli}
\alias{ClassPDEplotMaxLikeli}
\title{
  Create PDE plot for all classes with maximum likelihood
}
\description{
PDEplot the data for allclasses, weight the Plot with 1 (= maximum likelihood)
}
\usage{
ClassPDEplotMaxLikeli(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence,

 ClassNames, PlotLegend = TRUE, MinAnzKernels = 0,PlotNorm,

 main = "Pareto Density Estimation (PDE)",

 xlab = "Data", ylab = "ParetoDensity", xlim, ylim, \dots)
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
  \item{ClassNames}{
  Optional: the names of the classes to be displayed in the legend
}
  \item{PlotLegend}{
  Optional: add a legent to plot (default == 1)
}
  \item{MinAnzKernels}{
  Optional: Minimum number of kernels
}
  \item{PlotNorm}{
  Optional:  ==1 => plot Normal distribuion on top , ==2 = plot robust normal distribution,; default:  PlotNorm= 0
}
  \item{main}{
  Optional: Title of the plot
}
  \item{xlab}{
  Optional: title of the x axis
}
  \item{ylab}{
  Optional: title of the y axis
}
  \item{xlim}{
  Optional: area of the x-axis to be plotted
  }
  \item{ylim}{
  Optional: area of the y-axis to be plotted
  }
  \item{\dots}{
  further arguments passed to plot  
}
}

\value{
\item{Kernels}{Kernels of the distributions}
\item{ClassParetoDensities}{Pareto densities for classes}
\item{ggobject}{ggplot2 plot object. This should be used to further modify the plot}
}

\author{
Felix Pape
}
\references{
Aubert, A. H., Thrun, M. C., Breuer, L., & Ultsch, A. : Knowledge discovery from high-frequency stream nitrate concentrations: hydrology and biology contributions, Scientific reports, Nature, Vol. 6(31536), pp. doi 10.1038/srep31536, 2016. 
}
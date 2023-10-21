\name{PDEnormrobust}
\alias{PDEnormrobust}

\title{PDEnormrobust}
\description{
This functions plots ParetoDensityEsrtimation (PDE) and robustly estimated Gaussian with empirical Mean and  Variance}
\usage{
PDEnormrobust(Data,xlab='PDE',ylab,main='PDEnormrobust',
                          PlotSymbolPDE='blue',PlotSymbolGauss= 'magenta',PlotIt=TRUE,...)
}

\arguments{
  \item{data}{numeric vector, data to be plotted.}
  \item{xlab}{Optional,see plot}
  \item{ylab}{Optional,see plot}
  \item{main}{Optional,see plot}
    \item{PlotSymbolPDE}{line color pdf}
	      \item{PlotSymbolGauss}{line color robust gauss}
		       \item{PlotIt}{TRUE:plots, FALSE does not} 
	  \item{\dots}{Further arguments for plot}
	  
}

\value{
  \item{Kernels }{numeric vector. The x points of the PDE function.}
  \item{ParetoDensity }{estimated pdf of data, numeric vector,  the PDE(x).}
  \item{ParetoRadius }{numeric value, the Pareto Radius used for the plot.}
    \item{Normaldist }{pdf based on rubstly estimated parameters}
}
\author{Michael Thrun}

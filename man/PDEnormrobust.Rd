\name{PDEnormrobust}
\alias{PDEnormrobust}

\title{PDEnormrobust}
\description{
This functions plots ParetoDensityEsrtimation (PDE) and robustly estimated Gaussian with empirical Mean and  Variance}
\usage{
PDEnormrobust(Data,xlab='PDE',ylab,main='PDEnormrobust',
                          PlotSymbolPDE='blue',
                          PlotSymbolGauss= 'magenta',PlotIt=TRUE,
                          Mark2Sigma=FALSE,Mark3Sigma=FALSE,
                          p_mean=10,p_sd=25,...)
}

\arguments{
  \item{Data}{numeric vector, data to be plotted.}
  \item{xlab}{Optional,see plot}
  \item{ylab}{Optional,see plot}
  \item{main}{Optional,see plot}
  \item{PlotSymbolPDE}{line color pdf}
	\item{PlotSymbolGauss}{line color robust gauss}
	\item{Mark2Sigma}{ TRUE: sets to vertical lines marking data outside M+-1.96SD } 
	\item{Mark3Sigma}{TRUE: sets to vertical lines marking data outside M+-2.576SD} 
	\item{p_mean}{scalar between 1-99, percent of the top- and bottomcut from x} 
	\item{p_sd}{scalar between 1-99, lowInnerPercentile for robustly estimated standard deviation} 	

	  \item{\dots}{Further arguments for plot}
	  
}

\value{
  \item{Kernels }{numeric vector. The x points of the PDE function.}
  \item{ParetoDensity }{estimated pdf of data, numeric vector,  the PDE(x).}
  \item{ParetoRadius }{numeric value, the Pareto Radius used for the plot.}
  \item{Normaldist }{pdf based on rubstly estimated parameters}
  \item{Pars }{  Named vector of robustly estimatated \code{Mean}, standard deviation \code{SD}, \code{Sigma2}=1.96*SD and \code{Sigma3}=2.576*SD}

}
\author{Michael Thrun}

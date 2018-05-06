\name{PDEplot}
\alias{PDEplot}
\title{PDE plot}
\description{
This function plots the Pareto probability density estimation  (PDE),
uses PDEstimationForGauss and ParetoRadius.}
\usage{
PDEplot(data, paretoRadius = 0, weight = 1, kernels = NULL,

                 LogPlot = F, PlotIt = TRUE, title =
                 "ParetoDensityEstimation(PDE)", color = "blue",
				 
                 xpoints = FALSE, xlim, ylim, xlab = "Data", ylab =
                 "PDE", ggPlot = ggplot(), sampleSize = 2e+05, lwd = 2)
				 }

\arguments{
  \item{data}{numeric vector, data to be plotted.}
  \item{paretoRadius}{numeric, the Pareto Radius. If omitted, calculate by paretoRad.}
  \item{weight}{numeric,  Weight*ParetoDensity  is plotted. 1 by default.}
  \item{kernels}{numeric vector of kernels. Optional}
	\item{LogPlot}{LogLog PDEplot if TRUE, xpoints has to be FALSE. Optional}
  \item{PlotIt}{logical, if plot. TRUE by default.}
  \item{title}{character vector, title of plot.}
  \item{color}{character vector, color of plot.}
  \item{xpoints}{logical, if TRUE only points are plotted. FALSE by default.}
  \item{xlim}{Arguments to be passed to the plot method. }
  \item{ylim}{Arguments to be passed to the plot method. }
  \item{xlab}{Arguments to be passed to the plot method. }
  \item{ylab}{Arguments to be passed to the plot method. }
	\item{ggPlot}{ggplot2 object to be plotted upon. Insert an exisiting plot to add a new PDEPlot to it. Default: empty plot}
		\item{sampleSize}{default(200000), sample size, if datavector is to big}
		\item{lwd}{linewidth, see \code{plot}}
	
}

\value{

  \item{kernels }{numeric vector. The x points of the PDE function.}
  \item{paretoDensity }{numeric vector,  the PDE(x).}
  \item{paretoRadius }{numeric value, the Pareto Radius used for the plot.}
  \item{ggPlot}{ggplot2 object. Can be used to further modify the plot or add other plots.}
}
\references{Ultsch, A.: Pareto Density Estimation: A Density Estimation for Knowledge Discovery, Baier D., Wernecke K.D. (Eds), In Innovations in Classification, Data Science, and Information Systems - Proceedings 27th Annual Conference of the German Classification Society (GfKL) 2003, Berlin, Heidelberg, Springer, pp, 91-100, 2005.}
\author{Michael Thrun}
\examples{
x <- rnorm(1000, mean = 0.5, sd = 0.5)
y <- rnorm(750, mean = -0.5, sd = 0.75)
\donttest{plt <- PDEplot(x, color = "red")$ggPlot
plt <- PDEplot(y, color = "blue", ggPlot = plt)$ggPlot
}
# Second Example
#  ggplotObj=ggplot()
#  for(i in 1:length(Variables))
#     ggplotObj=PDEplot(Data[,i],ggPlot = ggplotObj)$ggPlot
}
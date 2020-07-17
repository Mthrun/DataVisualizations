\name{MDplot}
\alias{MDplot}
\title{
Mirrored Density plot (MD-plot)
}
\description{
This function creates a MD-plot for each variable of the data matrix. 
The MD-plot is a visualization for a boxplot-like Shape of the PDF published in [Thrun et al., 2019].
It is an improvement of violin or so-called bean plots and posses advantages in comparison to the conventional well-known box plot [Thrun et al., 2019].

A complete guide about the MDplot can be found in \url{https://md-plot.readthedocs.io/en/latest/index.html}.
}
\usage{
MDplot(Data, Names, Ordering='Default', Scaling="None",

Fill='darkblue', RobustGaussian=TRUE, GaussianColor='magenta',

Gaussian_lwd=1.5, BoxPlot=FALSE,BoxColor='darkred', 

MDscaling='width', LineColor='black', LineSize=0.01,

QuantityThreshold=50, UniqueValuesThreshold=12,

SampleSize=5e+05,SizeOfJitteredPoints=1,OnlyPlotOutput=TRUE)
}

\arguments{
  \item{Data}{
    [1:n,1:d] Numerical Matrix containing the n cases of d variables. Each column is one variable. A data.frame is automatically transformed to a numerical matrix.}
  \item{Names}{
    Optional: [1:d] Names of the variables. If missing, the columnnames of data are used.
  }
      \item{Ordering}{
    Optional: string, either \code{Default}, \code{Columnwise}, \code{Alphabetical}, \code{Average}, \code{Bimodal}, \code{Variance}  or \code{Statistics}. Please see details for explanation.
  }
      \item{Scaling}{
    Optional, Default is \code{None}, \code{Percentalize}, \code{CompleteRobust}, \code{Robust} or \code{Log}, Please see details for explanation.
  }

    \item{Fill}{
    Optional: string, color with which MDs are to be filled with.
  }
    \item{RobustGaussian}{
    Optional: If TRUE: each MDplot of a variable is overlayed with a roubustly estimated unimodal Gaussian distribution in the range of this variable,
	if statistical testing does not yield a significant p.value. In this case the packages \pkg{moments}, \pkg{diptest} and \pkg{signal} are required.
  }
      \item{GaussianColor}{
    Optional: string, color of robustly estimated gaussian, only for \code{RobustGaussian=TRUE}.
  }
  \item{Gaussian_lwd}{
    Optional: numerical, line width of robustly estimated gaussian, only for \code{RobustGaussian=TRUE}.
  }
     \item{BoxPlot}{
    Optional: If TRUE: each MDplot is overlayed with a Box-Whisker Diagram.
  }
      \item{BoxColor}{
    Optional: string, color of Boxplot, only for \code{BoxPlot=TRUE}.
  }
    \item{MDscaling}{
    Optional: if "area", all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to  the number of observations. If "width" (default), all MDs have the same maximum width.
  }
     \item{LineColor}{
    Optional: string, color of line around the mirrored densities. \code{NA} disables this features which is usefull if ones wants to avoid vertical lines leading to outliers.
  }
    \item{LineSize}{
    Optional: numerical, linewidth of line around the mirrored densities.
  }
    \item{QuantityThreshold}{
    Optional: numeric value defining the threshold of the minimal amount of values in data. Below this threshold no density estimation is performed and a 1D scatter plot with jittered points is drawn. Only Data Science experts should change this value after they understand  how the density is estimated (see [Ultsch, 2005]).
  }
      \item{UniqueValuesThreshold}{
    Optional: numeric value defining the threshold of the minimal amount of unique values in data. Below this threshold no density estimation and statistical testing is performed and a 1D scatter plot with jittered points drawn. Only Data Science experts should change this value after they understand how the density is estimated (see [Ultsch, 2005]).
  }
      \item{SampleSize}{
    Optional: numeric value defining a threshold. Above this threshold uniform sampling of finite cases is performed in order to shorten computation time.If \pkg{rowr} is not installed, uniform sampling of all cases
    is performed. If required, \code{SampleSize=n} can be set to omit this procedure. 
  }
   \item{SizeOfJitteredPoints}{
	 Optional: scalar. If not enough unique values for density estimation are given, data points are jittered. This parameter defines the size of the points. 
  }
  \item{OnlyPlotOutput}{
   Optional: Default TRUE only a ggplot object is given back, if FALSE: Additinally, scaled data and ordering are the output of this function in a \code{list}.
  }
}

\value{

In the default case of \code{OnlyPlotOutput==TRUE}: The ggplot object of the MD-plot. 

Otherwise for \code{OnlyPlotOutput==FALSE}: A list of
      \item{ggplotObj}{
   The ggplot object of the MD-plot.
  }
        \item{Ordering}{
   The ordering of columns of data defined by  \code{Ordering}.
  }
        \item{DataOrdered}{
   [1:n,1:d] matrix of ordered and scaled data defined by \code{Ordering} and \code{Scaling}.
  }
  
  Note that the package \pkg{ggExtra} is not necessarily required but if given the feature names are automatically rotated. 
}

\author{
Michael Thrun, Felix Pape contributed with the idea to use ggplot2 as the basic framework.
}
\examples{
\dontshow{
x <- cbind(A = runif(200, 1, 3), B = c(rnorm(100,0,1),rnorm(100,2.4,1)))
MDplot(x)

MDplot(rpois(100,1000))
}
\donttest{
x = cbind(
    A = runif(20000, 1, 5),
    B = c(rnorm(10000, 0, 1), rnorm(10000, 2.6, 1)),
    C = c(rnorm(20000, 2.5, 1)),
    D = rpois(20000, 5)
  )
MDplot(x)
}

}

\note{
1.) One would assume that in the first of the two following cases ggplot only adjusts the plotting region but: 

\code{MDplot(MTY)+ylim(c(0,7000))} is equal to \code{MDplot(MTY[MTY<7000])}. 

This means in both cases the data is clipped and AFTERWARDS the density estimation is performed. 

2.) Because of a (sometimes) strange behavior of either ggplot2 or reshape2, numerical column names are changed to character by adding 'C_'.

3.) Overlaying MD-plots with robustly estimated gaussians seldomly will yield magenta  (or other \code{GaussianColor}) lines overlaying more than the violin plot they should overlay, because the width of the two plots is not the same (but I am unable to set it strictly in ggplot). In such a case just call the function again.
}

\details{
In short, the MD-plot can be described as a PDE optimized violin plot. The Pareto Density Estimation (PDE) is an approach to estimate the probability density function (pdf) [Ultsch, 2005].

The MD-plot is in the process of beeing peer-reviewed [Thrun/Ultsch, 2019]. 

Statistical testing is performed with \code{\link[diptest]{dip.test}} and \code{\link[moments]{agostino.test}}.

For the paramter \code{Ordering} the following options are possible:
\describe{
\item{\code{Default}}{Ordering of plots by convex/concav/unimodal/nonunimodal shapes. In this case the \pkg{signal} is required.}

\item{\code{Columnwise}}{Ordering of plots by the order of columns of \code{Data}.}

\item{\code{Alphabetical}}{Ordering of plots by the order of columns of \code{Data} sorted in alphabetical order by column names.}

\item{\code{Average}}{Ordering of plots by the order of columns of \code{Data} sorted in order of increasing column-wise average}

\item{\code{Bimodal}}{Ordering of plots by the order of columns of \code{Data} sorted in order of decreasing bimodality amplitude[Zhang et al., 2003]}

\item{\code{Variance}}{Ordering of plots by the order of columns of \code{Data} sorted in order of increasing inter-quartile range}

\item{\code{Statistics}}{Ordering of plots depending on the logarithm of the p-vlaues of statistical testing. In this case the packages \pkg{moments}, \pkg{diptest} and \pkg{signal} are required.}
}

For the paramter \code{Scaling} the following options are possible:
\describe{
\item{\code{None}}{No Scaling of data is done.}

\item{\code{Percentalize}}{Data is scaled between zero and 100.}

\item{\code{CompleteRobust}}{Data is first robustly scaled between zero and 1, then centered to zero and outliers are capped by a robustly formula described in the \pkg{DatabionicSwarm} package. }

\item{\code{Robust}}{Data is robustly scaled between zero and 1 by a formula described in the \pkg{DatabionicSwarm} package. }

\item{\code{Log}}{Data is transformed with a sgined log allowing for negative values to be transformed with a logarithm of base 10, please see \code{SignedLog} for details.}
}
}
\references{
[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in  Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.

[Thrun et al., 2019]	Thrun, M. C., Gehlert, Tino, & Ultsch, A. : Analyzing the Fine Structure of Distributions, arXiv:1908.06081, 2019. 

[Zhang et al., 2003] Zhang, C., Mapes, B., & Soden, B.: Bimodality in tropical water vapour, Quarterly Journalof the Royal Meteorological Society, 129(594), 2847-2866, 2003.
}
\seealso{
\url{https://md-plot.readthedocs.io/en/latest/index.html}

\code{\link{ClassMDplot}}

\url{https://pypi.org/project/md-plot/}
}
\keyword{violin plot}
\keyword{geom_violin}
\keyword{PDE}
\keyword{violin}
\keyword{MDplot}
\keyword{MD-plot}
\keyword{MD}
\keyword{pdf}
\keyword{Mirrored Density plot}
\keyword{Mirrored Density}
\keyword{Density}
\keyword{density}
\keyword{pdf}
\keyword{probability density function}
\keyword{bean plot}
\keyword{vase plot}
\keyword{multivariate}
\keyword{box plot}
\keyword{box whisker diagramm}
\keyword{schematic plot}
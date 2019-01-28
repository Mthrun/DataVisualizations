\name{MDplot}
\alias{MDplot}
\title{
Mirrored Density plot (MD-plot): Visualization for a Boxplot-like Shape of the PDF published in [Thrun/Ultsch, 2019].
}
\description{
This function creates a MD-plot for each variable of the data matrix. 
The MD-plot is an improvement of violin or so-called bean plots which posses advantages in comparison to the conventional well-known box plot [Thrun/Ultsch, 2019].
}
\usage{
MDplot(Data, Names, Ordering='Default', Scaling="None",

Fill='darkblue', RobustGaussian=TRUE, GaussianColor='magenta',

Gaussian_lwd=1.5, BoxPlot=FALSE, BoxColor='darkred',

MDscaling='width',Size=0.01,MinimalAmoutOfData=40, OnlyPlotOutput=TRUE)
}

\arguments{
  \item{Data}{
    [1:n,1:d] Numerical Matrix containing the n cases of d variables. Each column is one variable. A data.frame is automatically transformed to a numerical matrix.}
  \item{Names}{
    Optional: [1:d] Names of the variables. If missing, the columnnames of data are used.
  }
      \item{Ordering}{
    Optional: string, either \code{Default}, \code{Columnwise}, \code{Alphabetical} or \code{Statistics}. Please see details for explanation.
  }
      \item{Scaling}{
    Optional, Default is \code{None}, \code{Percentalize}, \code{CompleteRobust}, \code{Robust} or \code{Log}, Please see details for explanation.
  }

    \item{Fill}{
    Optional: string, color with which MDs are to be filled with.
  }
    \item{RobustGaussian}{
    Optional: If TRUE: each MDplot is overlayed with a roubustly estimated unimodal Gaussian distribution, if statistical testing does not yield a significant p.value.
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
    \item{Size}{
    Optional: numerical, linewidth of black line around the MDs.
  }
    \item{MinimalAmoutOfData}{
    Optional: numeric value defining a threshold. Below this threshold no density estimation is performed and a          Jitter plot with a median line is drawn. Only Data Science experts should change this value after they understand     how the density is estimated (see [Ultsch, 2005]).
  }
  \item{OnlyPlotOutput}{
  Default TRUE only a ggplot object is given back, if FALSE: Additinally Scaled Data and ordering are the output of this function in a \code{list}.
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
  
  Note that the package \pkg{ggExtra} is not necessarily required but if given the feauture names are automatically rotated. 
}

\author{
Michael Thrun, Felix Pape contributed with the idea to use ggplot2 as the basic framework.
}
\examples{
\dontshow{
x <- cbind(A = runif(200, 1, 3), B = c(rnorm(100,0,1),rnorm(100,2.4,1)))
MDplot(x)
}
\donttest{
x <- cbind(A = runif(20000, 1, 3), B = c(rnorm(10000,0,1),rnorm(10000,2.6,1)))
MDplot(x)
}

}

\note{
1.) One would assume that in the first of the two following case only the plotting region is adjusted but: 

\code{MDplot(MTY)+ylim(c(0,7000))} is equal to \code{MDplot(MTY[MTY<7000])}. 

This means in both cases the data is clipped and AFTERWARDS the density estimated. 

2.) Because of a (sometimes) strange behavior of either ggplot2 or reshape2, numerical column names are changed to character by adding 'C_'.
}

\details{
In short, the MD-plot can be described as a PDE optimized violin plot. The Pareto Density Estimation (PDE) is an approach to estimate the probability density function (pdf) [Ultsch, 2005].

MD plot was used in [Thrun et al.,2018] for the evaluation of stochastic clustering methods and used in [Thrun et al.,2018a] in order to simultaneously estimate variances of a high-dimensional data set. The MD-plot is in the process of beeing peer-reviewed [Thrun/Ultsch, 2019]. 

Statistical testing is performed with \code{\link[diptest]{dip.test}} and \code{\link[moments]{agostino.test}}.

For the paramter \code{Ordering} the following options are possible:
\describe{
\item{\code{Default}}{Ordering of plots by convex/concav/unimodal/nonunimodal shapes.}

\item{\code{Columnwise}}{Ordering of plots by the order of columns of \code{Data}.}

\item{\code{Alphabetical}}{Ordering of plots by the order of columns of \code{Data} sorted in alphabetical order by column names.}

\item{\code{Statistics}}{Ordering of plots depending on the logarithm of the p-vlaues of statistical testing. In this case the packages \pkg{moments} and \pkg{diptest} are required.}
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

[Thrun/Ultsch, 2019]	Thrun, M. C., & Ultsch, A. : Analyzing the Fine Structure of Distributions, Technical Report prior to submission, Philipps-University Marburg, Germany, 2019. 


[Thrun et al.,2018a]	Thrun, M. C., Breuer, L., & Ultsch, A. : Knowledge discovery from low-frequency stream nitrate concentrations: hydrology and biology contributions, Proc. European Conference on Data Analysis (ECDA), pp. 46-47, Paderborn, Germany, 2018. 

[Thrun et al.,2018b]	Thrun, M. C., Pape, F., & Ultsch, A. : Benchmarking Cluster Analysis Methods using PDE-Optimized Violin Plots, Proc. European Conference on Data Analysis (ECDA), p. 26, Paderborn, Germany, 2018. 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{violin plot}
\keyword{geom_violin}
\keyword{PDE}
\keyword{violin}
\keyword{MDplot}
\keyword{MD-plot}
\keyword{MD}
\keyword{Mirrored Density plot}
\keyword{Mirrored Density}
\keyword{Density}
\keyword{pdf}
\keyword{probability density function}
\keyword{bean plot}
\keyword{vase plot}
\keyword{box plot}
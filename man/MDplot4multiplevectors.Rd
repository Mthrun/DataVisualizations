\name{MDplot4multiplevectors}
\alias{MDplot4multiplevectors}
\title{
Mirrored Density plot (MD-plot)for Multiple Vectors
}
\description{
This function creates a MD-plot for multiple numerical vectors of various lenghts.
The MD-plot is a visualization for a boxplot-like Shape of the PDF published in [Thrun/Ultsch, 2019].
It is an improvement of violin or so-called bean plots and posses advantages in comparison to the conventional well-known box plot [Thrun/Ultsch, 2019].
}
\usage{
MDplot4multiplevectors(\dots, Names, Ordering='Default', Scaling="None",

Fill='darkblue', RobustGaussian=TRUE, GaussianColor='magenta',

Gaussian_lwd=1.5, BoxPlot=FALSE, BoxColor='darkred',

MDscaling='width',Size=0.01,MinimalAmoutOfData=40, OnlyPlotOutput=TRUE)
}

\arguments{
  \item{\dots}{d numerical vectors of different lengths}
   
   \item{Names}{
    Optional: [1:d] Names of the the numerical vectors. If missing, the columnnames are mumbered.
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
    Optional: If TRUE: each MDplot is overlayed with a roubustly estimated unimodal Gaussian distribution, if statistical testing does not yield a significant p.value. In this case the packages \pkg{moments}, \pkg{diptest} and \pkg{signal} are required.
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
Michael Thrun.
}
\examples{
\dontshow{
MDplot4multiplevectors(runif(200, 1, 3),c(rnorm(200,0,1),rnorm(100,2.4,1)))

MDplot4multiplevectors(rpois(100,1000))
}
\donttest{

MDplot4multiplevectors(runif(20000, 1, 5),c(rnorm(20000,0,1),

rnorm(20000,2.6,1)),c(rnorm(2000,2.5,1)),rpois(25000,5),

Names=c('A','B','C','D'))
}

}


\details{
Please see \code{\link{MDplot}} for details.
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
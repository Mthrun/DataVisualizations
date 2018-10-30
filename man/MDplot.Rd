\name{MDplot}
\alias{MDplot}
\title{
Mirrored Density plot (MD-plot): Visualization for a Boxplot-like Shape of the PDF
}
\description{
This function creates a MD-plot for each variable of the data matrix. 
The MD-plot is an improvement of violin or so-called bean plots which posses advantages in comparison to the conventional well-known box plot.
}
\usage{
MDplot(Data, Names,fill='darkblue',

RobustGaussian=TRUE,GaussianColor='Magenta',Gaussian_lwd=1.5,

BoxPlot=FALSE,BoxColor='darkred',

scale='width',size=0.01)
}

\arguments{
  \item{Data}{
    [1:n,1:d] Numerical Matrix containing the n cases of d variables. Each column is one variable. A data.frame is automatically transformed to a numerical matrix.}
  \item{Names}{
    Optional: [1:d] Names of the variables. If missing, the columnnames of data are used.
  }
    \item{fill}{
    Optional: string, color with which MDs are to be filled with
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
  
    \item{scale}{
    Optional: if "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to  the number of observations. If "width", all MDs have the same maximum width.
  }
    \item{size}{
    Optional: numerical, linewidth of black line around the MDs.
  }
}

\value{
The ggplot object of the MD-plot.
}

\author{
Michael Thrun, Felix Pape
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
\details{
In short, the MD-plot can be described as a PDE optimized violin plot. The Pareto Density Estimation (PDE) is an approach to estimate the probability density function (pdf) [Ultsch, 2005].

MD plot was used in [Thrun et al.,2018] for the evaluation of stochastic clustering methods and used in [Thrun et al.,2018a] in order to simultaneously estimate variances of a high-dimensional data set. The MD-plot is in the process of beeing published. 

Statistical testing is performed with \code{\link[diptest]{dip.test}} and \code{\link[moments]{agostino.test}}.
}
\references{
[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in  Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.


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
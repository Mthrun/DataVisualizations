\name{PDEviolinPlot}
\alias{PDEviolinPlot}
\title{
PDE optimized violoin plot for multiple variables
}
\description{
This function creates a PDE optimized violin plot for each variable of the data matrix. Each boxplot also has a point for the mean of the variable.
}
\usage{
PDEviolinPlot(Data, Names,fill='darkblue',scale='width',size=0.01)
}

\arguments{
  \item{Data}{
    Matrix containing the data. Each column is one variable.}
  \item{Names}{
    Optional: Names of the variables. If missing the columnnames of data are used.
  }
    \item{fill}{
    Optional: color with which violin is to be filled with, see \code{ggplot2} documentation for details
  }
    \item{scale}{
    Optional: if "area" (default), all violins have the same area (before trimming the tails). If "count", areas are scaled proportionally to  the number of observations. If "width", all violins have the same maximum width.
  }
    \item{size}{
    Optional: numerical, linewith of black line around the violin plot
  }
}

\value{
The ggplot object of the boxplots
}

\author{
Michael Thrun, Felix Pape
}
\examples{
\dontshow{
x <- cbind(A = runif(200, 1, 3), B = c(rnorm(100,0,1),rnorm(100,2.4,1)))
PDEviolinPlot(x)
}
\donttest{
x <- cbind(A = runif(20000, 1, 3), B = c(rnorm(10000,0,1),rnorm(10000,2.6,1)))
PDEviolinPlot(x)
}
#Check for significance
\donttest{
requireNamespace('diptest')
diptest::dip.test(x[,1])$p.value
diptest::dip.test(x[,2])$p.value
}
}
\details{
Published in [Thrun et al.,2018] for the evaluation of stochastic clustering methods and used in [Thrun et al.,2018a] in order to simultaneously estimate variances of a high-dimensional data set.
}
\references{
[Thrun et al.,2018a]	Thrun, M. C., Breuer, L., & Ultsch, A. : Knowledge discovery from low-frequency stream nitrate concentrations: hydrology and biology contributions, Proc. European Conference on Data Analysis (ECDA), pp. 46-47, Paderborn, Germany, 2018. 

[Thrun et al.,2018b]	Thrun, M. C., Pape, F., & Ultsch, A. : Benchmarking Cluster Analysis Methods using PDE-Optimized Violin Plots, Proc. European Conference on Data Analysis (ECDA), p. 26, Paderborn, Germany, 2018. 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{violin plot}
\keyword{PDE}
\keyword{violin}
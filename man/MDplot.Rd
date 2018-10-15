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
MDplot(Data, Names,fill='darkblue',scale='width',size=0.01)
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
MDplot(x)
}
\donttest{
x <- cbind(A = runif(20000, 1, 3), B = c(rnorm(10000,0,1),rnorm(10000,2.6,1)))
MDplot(x)
}
#Check for significance
\donttest{
#requireNamespace('diptest')
#diptest::dip.test(x[,1])$p.value
#diptest::dip.test(x[,2])$p.value
}
}
\details{
In short, the MD-plot can be described as a PDE optimized violin plot. The Pareto Density Estimation (PDE) is an approach to estimate the probability density function (pdf) [Ultsch, 2005].

MD plot was used in [Thrun et al.,2018] for the evaluation of stochastic clustering methods and used in [Thrun et al.,2018a] in order to simultaneously estimate variances of a high-dimensional data set. The MD-plot is in the process of beeing published. 
}
\references{
[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in  Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.


[Thrun et al.,2018a]	Thrun, M. C., Breuer, L., & Ultsch, A. : Knowledge discovery from low-frequency stream nitrate concentrations: hydrology and biology contributions, Proc. European Conference on Data Analysis (ECDA), pp. 46-47, Paderborn, Germany, 2018. 

[Thrun et al.,2018b]	Thrun, M. C., Pape, F., & Ultsch, A. : Benchmarking Cluster Analysis Methods using PDE-Optimized Violin Plots, Proc. European Conference on Data Analysis (ECDA), p. 26, Paderborn, Germany, 2018. 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{violin plot}
\keyword{PDE}
\keyword{violin}
\keyword{MDplot}
\keyword{MD}
\keyword{Mirrored Density plot}
\keyword{Mirrored Density}
\keyword{Density}
\keyword{pdf}
\keyword{probability density function}
\keyword{bean plot}
\keyword{vase plot}
\keyword{box plot}
\name{PDEviolinplotData}
\alias{PDEviolinplotData}
\title{
PDE optimized violoin plot for multiple variables
}
\description{
This function creates a PDE optimized violin plot for each variable of the data matrix. Each boxplot also has a point for the mean of the variable.
}
\usage{
PDEviolinplotData(Data, Names)
}

\arguments{
  \item{Data}{
    Matrix containing the data. Each column is one variable.}
  \item{Names}{
    Optional: Names of the variables. If missing the columnnames of data are used.
  }
}

\value{
The ggplot object of the boxplots
}

\author{
Michael Thrun, Felix Pape
}
\examples{
x <- cbind(A = rnorm(200, 1, 3), B = rnorm(100, -2, 5))
PDEviolinplotData(x)
}
\details{
Published in [Thrun et al.,2018] for the evaluation of stochastic clustering methods and used in [Thrun et al.,2018a] in order to simultaneously estimate variances of a high-dimensional data set.
}
\references{
[Thrun et al.,2018a]	Thrun, M. C., Breuer, L., & Ultsch, A. : Knowledge discovery from low-frequency stream nitrate concentrations: hydrology and biology contributions, Proc. European Conference on Data Analysis (ECDA), accepted, Paderborn, Germany, 2018. 

[Thrun et al.,2018b]	Thrun, M. C., Pape, F., & Ultsch, A. : Benchmarking Cluster Analysis Methods using PDE-Optimized Violin Plots, Proc. European Conference on Data Analysis (ECDA), accepted, Paderborn, Germany, 2018. 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{violin plot}
\keyword{PDE}
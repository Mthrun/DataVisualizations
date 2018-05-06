\name{InspectScatterOfData}
\alias{InspectScatterOfData}

\title{
Pairwise scatterplots and optimal histograms
}
\description{
Pairwise scatterplots and optimal histograms of all features stored as columns of data are plotted
}
\usage{
InspectScatterOfData(Data,Names=colnames(Data))
}

\arguments{
  \item{Data}{ [1:n,1:d] Data cases in rows (n), variables in columns (d) }
    \item{Names}{
    Optional: Names of the variables. If missing the columnnames of data are used.
  }
}
\details{

For two features, \code{PDEscatter} function should be used to isnpect modalities [Thrun/Ultsch, 2018]. For many features the function takes too lang. In such a case this function can be used. See [Thrun/Ultsch, 2018] for optimal histogram description.
}

\references{
[Thrun/Ultsch, 2018]  Thrun, M. C., & Ultsch, A.: Effects of the payout system of income taxes to municipalities in Germany, 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, Vol. accepted, Foundation of the Cracow University of Economics, Zakopane, Poland, 2018.
}
\author{
Michael Thrun
}


\examples{
Data=cbind(rnorm(100, mean = 2, sd = 3  ),rnorm(100,mean = 0, sd = 1),rnorm(100,mean = 6, sd = 0.5))
#InspectScatterOfData(Data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{pairs}% use one of  RShowDoc("KEYWORDS")
\keyword{scatter}% __ONLY ONE__ keyword per line
\keyword{scatterplot}% __ONLY ONE__ keyword per line
\keyword{scatter plot}% __ONLY ONE__ keyword per line
\keyword{pairwise scatter plot}% __ONLY ONE__ keyword per line
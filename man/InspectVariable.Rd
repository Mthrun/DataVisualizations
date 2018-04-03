\name{InspectVariable}
\alias{InspectVariable}
\title{Visualization of Distribution of one variable}
\usage{
InspectVariable(Feature, N = "Feature", i = 1, xlim, ylim,

 sampleSize =1e+05, main)
 }
\arguments{
\item{Feature}{[1:n] Variable/Vector of Data to be plotted}
\item{N}{Optional, string, for x label}
\item{i}{Optional, No. of variable/feature, an integer of the for lope}
\item{xlim}{[2] Optional, range of x-axis for PDEplot }
\item{ylim}{[2] Optional, range of y-axis for PDEplot }
\item{sampleSize}{Optional, default(100000), sample size, if datavector is to big}
\item{main}{string for the title if other than what is desribed in \code{N}}
}


\description{
Enables distribution inspection by visualization as described in [Thrun, 2018] and for example used in 
 }
\author{
Michael Thrun
}

\references{
[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.

[Thrun/Ultsch, 2018]  Thrun, M. C., & Ultsch, A.: Effects of the payout system of income taxes to municipalities in Germany, 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, Vol. accepted, Foundation of the Cracow University of Economics, Zakopane, Poland, 2018.
}

\examples{
data("ITS")
InspectVariable(ITS,N='Income in EUR',main='ITS')
}


\keyword{distribution}
\keyword{estimation}
\keyword{distribution analysis}
\keyword{distribution visualization}
\keyword{pdf}
\keyword{probability density function}
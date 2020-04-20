\name{BimodalityAmplitude}
\alias{BimodalityAmplitude}
\title{Bimodality Amplitude}
\usage{
BimodalityAmplitude(x, PlotIt=FALSE)
}
\arguments{
\item{x}{Data vector.}

\item{PlotIt}{FALSE, TRUE if  a figure with the antimodes and peaks is plotted}

}
\description{
Computes the Bimodality Amplitude of [Zhang et al., 2003]}
\details{
This function calculates the Bimodality Ampltiude of a data vector.
This is a measure of the proportion of bimodality and the existence
of bimodality. The value lies between zero and one (that is: [0,1])
where the value of zero implies that the data is unimodal and the 
value of one implies the data is two point masses.
}
\examples{
#Example 1
data<-c(rnorm(299,0,1),rnorm(299,5,1))
BimodalityAmplitude(data,TRUE)

#Example 2
dist1<-rnorm(2100,5,2)
dist2<-dist1+11
data<-c(dist1,dist2)

BimodalityAmplitude(data,TRUE)

#Example 3
dist1<-rnorm(210,-15,1)
dist2<-rep(dist1,3)+30
data<-c(dist1,dist2)

BimodalityAmplitude(data,TRUE)

#Example 4
data<-runif(1000,-15,1)

BimodalityAmplitude(data,TRUE)
}
\references{
Zhang, C., Mapes, B., & Soden, B.:  Bimodality in tropical water vapour, Quarterly Journal of the Royal Meteorological Society, Vol. 129(594), pp. 2847-2866, 2003.
}
\keyword{Bimodality}
\keyword{antinode}
\keyword{peaks}

\author{
Michael Thrun
}
\note{
function was rewritten after the flow of a function of Sathish Deevi because the original function was incorrect.
}
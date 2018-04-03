\name{slopeChart}
\alias{slopeChart}

\title{
Slope Chart
}
\description{
ABC analysis improved slope chart
}
\usage{
slopChart(FirstDatavector,SecondDatavector,

Names,Labels,MaxNumberOfSlices,

TopLabels=c('FirstDatavector','SecondDatavector'),

main='Comparision of Descending Frequency')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FirstDatavector}{
}
  \item{SecondDatavector}{
}
  \item{Names}{
}
  \item{TopLabels}{
}
  \item{main}{
}
  \item{FirstDatavector}{
}
}
\details{
to be filled..
}
\value{
silent output by calling \code{invisible} of a list with
  \item{Percentages}{
  [1:k] percent values visualized in fanplot
}
  \item{Labels}{
  [1:k] see input \code{Labels}, only relevant ones
}
}
\references{
[Gohil, 2015]  Gohil, Atmajitsinh. R data Visualization cookbook. Packt Publishing Ltd, 2015.
}
\author{
Michael Thrun
}


\seealso{
\code{\link{pieChart}}, \code{\link{fanPlot}}
}
\examples{
## will follow
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{slopechart}% use one of  RShowDoc("KEYWORDS")
\keyword{slope chart}% __ONLY ONE__ keyword per line

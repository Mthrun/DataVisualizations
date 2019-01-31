\name{Slopechart}

\alias{Slopechart}

\title{
Slope Chart
}
\description{
ABC analysis improved slope chart
}
\usage{
Slopechart(FirstDatavector,

SecondDatavector,

Names,

Labels,

MaxNumberOfSlices,

TopLabels=c('FirstDatavector','SecondDatavector'),

main='Comparision of Descending Frequency')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FirstDatavector}{
    [1:n] a vector of n non unique values - a features
}
  \item{SecondDatavector}{
    [1:m] a vector of n non unique values - a second feature
}
  \item{Labels}{Optional,
  [1:k] Labels if they are specially named, if not Names are used.
}
  \item{Names}{
    [1:k] names to search for in Datavector, if not set \code{unique} of Datavector is calculated.
}
    \item{MaxNumberOfSlices}{Default is k, integer value defining how many labels will be shown. Everything else will be summed up to \code{Other}.

}
  \item{TopLabels}{
  Labels of of feature names
}
  \item{main}{
  title of the plot
}

}
\details{
still experimental.
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
\code{\link{Piechart}}, \code{\link{Fanplot}}
}
\examples{
## will follow
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{slopechart}% use one of  RShowDoc("KEYWORDS")
\keyword{slope chart}% __ONLY ONE__ keyword per line

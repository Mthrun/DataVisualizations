\name{fanPlot}
\alias{fanPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The fan plot
}
\description{
The better alternative to the pie chart represents amount of values given in data.
}
\usage{
fanPlot(Datavector,Names,Labels,MaxNumberOfSlices,main='',col,

MaxPercentage=FALSE,ShrinkPies=0.05,Rline=1.1)
}
\arguments{
  \item{Datavector}{
  [1:n] a vector of n non unique values
}
  \item{Names}{Optional, 
  [1:k] names to search for in Datavector, if not set \code{unique} of Datavector is calculated.
}
  \item{Labels}{Optional,
  [1:k] Labels if they are specially named, if not Names are used.
}
    \item{MaxNumberOfSlices}{Default is k, integer value defining how many labels will be shown. Everything else will be summed up to \code{Other}.

}
  \item{main}{
  Optional, title below the fan pie, see \code{plot}
}
  \item{col}{Optional, default as other colors in this packages, else the same as in \code{plot}
}
  \item{MaxPercentage}{
  default FALSE; if true the biggest slice is 100 percent instead of the biggest procentual count
}
  \item{ShrinkPies}{
  Optional, distance between biggest and smallest slice of the pie
}
  \item{Rline}{Optional, the distance between text and pie is defined here as the length of the line in numerical numbers

}
}
\details{
A normal pie plot is dificult to interpret for a human observer, because humans are not trained well to observe angles [Gohil, 2015, p. 102]. Therefore, the fan plot is used. As proposed in [Gohil 2015] the \code{fan.plot}() of the \code{plotrix} package is used to solve this problem.
If Number of Slices is higher than MaxNumberOfSlices then \code{ABCanalysis} is applied (see [Ultsch/Lotsch, 2015]) and group A chosen.
If Number of Slices in group A is higher than MaxNumberOfSlices, then the most important ones out of group A are chosen.
If MaxNumberOfSlices is higher than Slices in group A, additional slices are shown depending on the percentage (from high to low).
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

[Ultsch/Lotsch, 2015]   Ultsch. A ., Lotsch J.: Computed ABC Analysis for Rational Selection of Most Informative Variables in Multivariate Data, PloS one, Vol. 10(6), pp. e0129767. doi 10.1371/journal.pone.0129767, 2015.

}
\author{
Michael Thrun
}

\examples{
data(categoricalVariable)
fanPlot(categoricalVariable)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fan}% use one of  RShowDoc("KEYWORDS")
\keyword{fan plot}% __ONLY ONE__ keyword per line
\keyword{pie}% __ONLY ONE__ keyword per line
\keyword{pie chart}% __ONLY ONE__ keyword per line

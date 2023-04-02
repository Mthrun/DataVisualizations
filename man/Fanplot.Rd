\name{Fanplot}
\alias{Fanplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The fan plot
}
\description{
The better alternative to the pie chart represents amount of values given in data.
}
\usage{
Fanplot(Datavector,Names,Labels,MaxNumberOfSlices,main='',col,

MaxPercentage=FALSE,ShrinkPies=0.05,Rline=1.1,lwd=2,\dots)
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
  \item{col}{Optional, the default are the first [1:k] colors of the default color sequence used in this package, otherwise a character vector of [1:k] specifying the colors analog to \code{plot}
}
  \item{MaxPercentage}{
  default FALSE; if true the biggest slice is 100 percent instead of the biggest procentual count
}
  \item{ShrinkPies}{
  Optional, distance between biggest and smallest slice of the pie
}
  \item{Rline}{Optional, the distance between text and pie is defined here as the length of the line in numerical numbers}
    \item{lwd}{Optional, The line width, a positive number, defaut is 2}
    \item{\dots}{Further arguments to \code{\link[plotrix]{fan.plot}} like circumferential positions for the labels \code{labelpos} or additional arguments passed to \code{\link{polygon}}
}
}
\details{
A normal pie plot is dificult to interpret for a human observer, because humans are not trained well to observe angles [Gohil, 2015, p. 102]. Therefore, the fan plot is used. As proposed in [Gohil 2015] the \code{fan.plot}() of the \code{plotrix} package is used to solve this problem.
If Number of Slices is higher than MaxNumberOfSlices then \code{ABCanalysis} is applied (see [Ultsch/Lotsch, 2015]) and group A chosen.
If Number of Slices in group A is higher than MaxNumberOfSlices, then the most important ones out of group A are chosen.
If MaxNumberOfSlices is higher than Slices in group A, additional slices are shown depending on the percentage (from high to low).

Color sequence is automatically shortened to the MaxNumberOfSlices used in the fan plot. 
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
\seealso{
\code{\link[plotrix]{fan.plot}}
\code{\link{Piechart}}
}

\examples{
data(categoricalVariable)
Fanplot(categoricalVariable)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fan}% use one of  RShowDoc("KEYWORDS")
\keyword{fan plot}% __ONLY ONE__ keyword per line
\keyword{pie}% __ONLY ONE__ keyword per line
\keyword{pie chart}% __ONLY ONE__ keyword per line

\name{Piechart}
\alias{Piechart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The pie chart
}
\description{
the pie chart represents amount of values given in data.
}
\usage{
Piechart(Datavector,Names,Labels,MaxNumberOfSlices,
main='',col,Rline=1,...)
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

  \item{Rline}{Optional, the radius of the pie in numerical numbers
}
  \item{\dots}{Optional, further arguments passed on to \code{\link{plot}}
}


}
\details{
If Number of Slices is higher than MaxNumberOfSlices then \code{ABCanalysis} is applied (see [Ultsch/Lotsch, 2015]) and group A chosen.
If Number of Slices in group A is higher than MaxNumberOfSlices, then the most important ones out of group A are chosen.
If MaxNumberOfSlices is higher than Slices in group A, additional slices are shown depending on the percentage (from high to low).
Parameters of visualization a set as in [Schwabish, 2014] defined.

Color sequence is automatically shortened to the MaxNumberOfSlices used in the pie chart.
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

[Schwabish, 2014]	Schwabish, Jonathan A. An Economist's Guide to Visualizing Data. Journal of Economic Perspectives, 28 (1): 209-34. DOI: 10.1257/jep.28.1.209, 2014.

[Ultsch/Lotsch, 2015]   Ultsch. A ., Lotsch J.: Computed ABC Analysis for Rational Selection of Most Informative Variables in Multivariate Data, PloS one, Vol. 10(6), pp. e0129767. doi 10.1371/journal.pone.0129767, 2015.


}
\author{
Michael Thrun
}
\note{
You see in the example below that a pie chart does not visualize such data well contrary to the \code{fanPlot}.
}
\examples{
data(categoricalVariable)
Piechart(categoricalVariable)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{pie}% __ONLY ONE__ keyword per line
\keyword{pie chart}% __ONLY ONE__ keyword per line

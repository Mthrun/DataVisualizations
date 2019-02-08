\name{Crosstable}
\alias{Crosstable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Crosstable plot
}
\description{
Presents a heatmap with values and a cross table of given Data matrix of two features and a bin width or percentualized values. In this approach the bin width is fixes. A more general way to approach this is the kernel density estimation plot of \code{\link{PDEscatter}}.
}
\usage{
Crosstable(Data, xbins = seq(0, 100, 5), ybins = xbins, 

NormalizationFactor = 1, PlotIt = TRUE, main='Cross Table',

PlotText=TRUE,TextDigits=0,TextProbs=c(0.05,0.95))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:2] matrix of two features from which the cross table should be generated from
}
  \item{xbins}{
[1:k] start of k bins as a vector generated with \code{\link{seq}} of the first feature of data. Default setting assumes percentiled values between zero and 100.
}
  \item{ybins}{
[1:k] start of k bins as a vector generated with \code{\link{seq}} of the second feature of data. Normally the same for both features, other settings are only possible if the length \code{k} is equal.
}
  \item{NormalizationFactor}{
Optional, Data feautures can be seen as regular time series, e.g. 1 measurement for a minute, in this case it is useful to normalize the output, e.g. to hours, then \code{NormalizationFactor=60}
}
  \item{PlotIt}{
Optional, Plots the heatmap if \code{TRUE}.
The first feature is on the x-axis (left to right) and the second on y-axis (bottom to top).
}
  \item{main}{
In case of for \code{PlotIt=TRUE}: title of plot, see \code{\link{title}}
}

  \item{PlotText}{
In case of for \code{PlotIt=TRUE}: Default TRUE: plots text in heatmap with the values of the crosstable
}
  \item{TextDigits}{
In case of for \code{TextDigits=TRUE}: integer indicating the number of decimal places to use in \code{\link{round}}.
}
  \item{TextProbs}{
In case of for \code{TextDigits=TRUE}: [1:2] numeric vector of two probabilities defining the thresholds for white text to grey text and grey text to black text, e.g. below the first threshold (Default 0.05) all values (5\% of values) will be printed in white because the lowest values of the heatmap are blue. The second value of 0.95 works well if cross table  has many  zeros; uses \code{\link{quantile}} internally.
}
}
\details{
The interval in each bin is closed to the left and opened to the right. The cross table can be seen as a two-dimensional histogram. The idea to add histograms to the table is taken from [Charpentier. 2014].
}
\value{
The cross table in \code{invisible} mode which depicts the number of values (frequency) in an specific range with regard to two features. 

The first feature is on the x-axis (left to right), and the second on y-axis (top to bottom) contrary to the plot where it is bottom to top.
}
\references{
[Charpentier. 2014]  Charpentier, Arthur, ed. Computational actuarial science with R. CRC Press, 2014.
}
\author{
Michael Thrun
}
\note{
For non percentiled values the \code{PlotText} part does not seem always to work, but I currently dont know why the text does not always overlap with the heatmap. 
}


\seealso{
\code{\link{table}}, \code{\link{image}}, \code{\link{PDEscatter}}
}
\examples{
data(ITS)
data(MTY)
#simple but not a good transformation
Data=(cbind(ITS/max(ITS),MTY/max(MTY)))*100
#choice for bins could be better
Crosstable(Data)
}

\keyword{cross table}% use one of  RShowDoc("KEYWORDS")
\keyword{plot table}% __ONLY ONE__ keyword per line

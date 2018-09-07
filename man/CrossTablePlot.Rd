\name{CrossTablePlot}
\alias{CrossTablePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CrossTablePlot
}
\description{
Presents a heatmap with values and a cross table of given Data matrix of two features and a bin width or percentualized values. In this approach the bin width is fixes. A more general way to approach this is the kernel density estimation plot of \code{\link{PDEscatter}}.
}
\usage{
CrossTablePlot(Data, xbins = seq(0, 100, 5), 

ybins = xbins, PlotIt = TRUE, NormalizationFactor = 1,

PlotText=TRUE,TextDigits=0)
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
  \item{PlotIt}{
Optional, Plots the heatmap if \code{TRUE}
}
  \item{NormalizationFactor}{
Optional, Data feautures can be seen as regular time series, e.g. 1 measurement for a minute, in this case it is useful to normalize the output, e.g. to hours, then \code{NormalizationFactor=60}
}
  \item{PlotText}{
Default TRUE: plots text in heatmap with the values of the crosstable
}
  \item{TextDigits}{
integer indicating the number of decimal places to use in \code{\link{round}} for \code{TextDigits=TRUE}.
}
}
\details{
The interval in each bin is closed to the left and opened to the right. The cross table can be seen as a two-dimensional histogram. The idea to add histograms to the table is taken from [Charpentier. 2014].
}
\value{
\code{invisible} Output of the cross table which the depicts the number of values (frequency) in an specific range with regard to two variables. 
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
#ToDo
}

\keyword{cross table}% use one of  RShowDoc("KEYWORDS")
\keyword{plot table}% __ONLY ONE__ keyword per line

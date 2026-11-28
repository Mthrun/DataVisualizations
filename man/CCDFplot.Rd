\name{CCDFplot}
\alias{CCDFplot}
\title{ plot Complementary Cumulative Distribution Function (CCDF) in Log/Log  uses ecdf, CCDF(x) = 1-cdf(x) }
\usage{
CCDFplot(Feature, LogLogPlot = TRUE, PlotIt = TRUE, pch = 0,
         xlab, ylab, main, ...)
}
\arguments{
\item{Feature}{Vector of data to be plotted, or a matrix with given probability density function in column 2 and/or a cumulative density function in column 3}
\item{pch}{Optional, default:  pch=0 for Line, other numbers see documentation about pch of plot}


\item{PlotIt}{Optional, if PlotIt==T (default) do a plot, otherwise return only values}
\item{LogLogPlot}{Optional, if LogLogPlot==T (default) do a log/log plot}
\item{xlab}{Optional,  xlab of plot}
\item{ylab}{Optional,  ylab of plot}
\item{main}{Optional,  main of plot}
\item{...}{Optional, further arguments for plot}
}
\value{V$CCDFuniqX,V$CCDFuniqY    CCDFuniqY= 1-cdf(CCDFuniqX), such that plot(CCDFuniqX,CCDFuniqY)...)}
\description{
 plot Complementary Cumulative Distribution Function (CCDF) in Log/Log  uses ecdf, CCDF(x) = 1-cdf(x) }
\details{
For two-column PDF input, the x values and densities are sorted together in
ascending x order. The CDF is computed by cumulative trapezoidal integration
from the smallest supplied x value, with CDF equal to zero there. The density
is used as supplied, without renormalization. Choose a grid whose lower end
has no probability mass below it; use three-column input with an explicit CDF
if the CDF at the first grid point is already nonzero.
The returned x values and CCDF values are ordered by decreasing x, as for the
other input forms. In three-column input the supplied CDF is used directly
and the PDF column is ignored.
}
\examples{
# Uniform density on [1, 3]; one CCDF value for every supplied x.
PDF = cbind(x = 1:3, density = rep(0.5, 3))
Result = CCDFplot(PDF, PlotIt = FALSE)
X = Result$CCDFuniqX
Y = Result$CCDFuniqY
X # 3, 2, 1
Y # 0, 0.5, 1
}
\author{
Michael Thrun}

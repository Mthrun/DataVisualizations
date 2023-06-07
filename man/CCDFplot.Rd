\name{CCDFplot}
\alias{CCDFplot}
\title{ plot Complementary Cumulative Distribution Function (CCDF) in Log/Log  uses ecdf, CCDF(x) = 1-cdf(x) }
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
\author{
Michael Thrun}

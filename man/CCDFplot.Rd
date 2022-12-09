\name{ccdfplot}
\alias{ccdfplot}
\title{ plot Complementary Cumulative Distribution Function (CCDF) in Log/Log  uses ecdf, CCDF(x) = 1-cdf(x) }
\arguments{
\item{Data}{Vector of data to be plotted, or a matrix with given probability density function in column 2 and/or a cumulative density function in column 3}
\item{LineType}{for plot default:  LineType=0 for Line, other numbers see documentation about pch}
\item{LogLogPlot}{if LogLogPlot==T (default) do a log/log plot}
}
\value{V$CCDFuniqX,V$CCDFuniqY    CCDFuniqY= 1-cdf(CCDFuniqX), such that plot(CCDFuniqX,CCDFuniqY)...)}
\description{
 plot Complementary Cumulative Distribution Function (CCDF) in Log/Log  uses ecdf, CCDF(x) = 1-cdf(x) }
\author{
MT 02/2015}

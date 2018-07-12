\name{DualAxisLineChart}
\alias{DualAxisLineChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DualAxisLineChart
}
\description{
A line chart with dual axisSS
}
\usage{
DualAxisLineChart(X, Y1, Y2, xlab = "X", y1lab = "Y1", y2lab = "Y2", main = "Dual Axis Line Chart", cols = c("black", "blue"), SaveIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{[1:n] vector, both lines require the same xvalues, e.g. the time of the time series, \code{POSIXlt} or \code{POSIXct} are accepted}
  \item{Y1}{
[1:n] vector of first line
}
  \item{Y2}{
[1:n] vector of second line
}
  \item{xlab}{
Optional, string for xlabel
}
  \item{y1lab}{
Optional, string for first ylabel
}
  \item{y2lab}{
Optional, string for second ylabel
}
  \item{main}{
Optional, title of plot
}
  \item{cols}{
Optional, color of two lines
}
  \item{SaveIt}{
Optional, default FALSE; TRUE if you want to save plot as html in \code{getwd()} directory
}
}
\details{
enables to visualize to lines in one plot overlaying them using ploty (e.g. two time series with two ranges of values)

}
\value{
\code{plotly} object
}
\author{
Michael Thrun
}

\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{DualAxisLineChart}% use one of  RShowDoc("KEYWORDS")
\keyword{Line Chart}% __ONLY ONE__ keyword per line
\keyword{Dual Axis Line Chart}
\keyword{Dual Axis}
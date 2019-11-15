\name{DualaxisLinechart}
\alias{DualaxisLinechart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DualaxisLinechart
}
\description{
A line chart with dual axisSS
}
\usage{
DualaxisLinechart(X, Y1, Y2, xlab = "X", 

y1lab = "Y1", y2lab = "Y2", main = "Dual Axis Line Chart",

cols = c("black", "blue"),Overlaying="y", SaveIt = FALSE)
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
  \item{Overlaying}{
Change only default in case of using \code{\link[plotly]{subplot}}
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
#subplot renames the numbering of subsequent plots
y1=runif(100,0,1)
y2=rnorm(100,m=5,s=1)
DualaxisLinechart(1:100, y1, y2,main="Random Time series")

\donttest{
y1=runif(100,0,1)
y2=(1:100*3+4)*runif(100,0,1)
p1=DualaxisLinechart(1:100, y1, y2,main="Random Time series",Overlaying="y2")

y3=1:100*(-2)+4
y4=rnorm(100,m=0,s=2)
p2=DualaxisLinechart(1:100, y3, y4,main="Random Time series",Overlaying="y4")
plotly::subplot(p1,p2)
}
}
\keyword{DualAxisLineChart}% use one of  RShowDoc("KEYWORDS")
\keyword{Line Chart}% __ONLY ONE__ keyword per line
\keyword{Dual Axis Line Chart}
\keyword{Dual Axis}

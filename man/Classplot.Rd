\name{Classplot}
\alias{Classplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classplot
}
\description{
Allows to plot one time series or feauture with a classification as a labeled scatter plot with a line.
The colors are the labels defined by the classification.
Usefull to see if temporal clustering has time dependent variations and for Hidden Markov Models (see Mthrun/RHmm on GitHub).
}
\usage{
Classplot(X, Y, Cls,Names=NULL,na.rm=FALSE,

xlab = "X", ylab = "Y",

main = "Class Plot", Colors,Size=8

LineColor = NULL, LineWidth = 1, LineType = NULL,

Showgrid = TRUE, PlotIt=TRUE,SaveIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
[1:n] numeric vector or time
}
  \item{Y}{
[1:n] numeric vector of feature
}
  \item{Names}{
[1:n] character vector of k classes, if not set perdefault Cls is used, if set, names the legend and the points
}
  \item{Cls}{
[1:n] numeric vector of k classes, if not set per default every point is in first class
}
  \item{na.rm}{Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE}
  
  \item{xlab}{
Optional, string
}
  \item{ylab}{
Optional, string
}
  \item{main}{
Optional, string
}
  \item{Colors}{
Optional, string
}
  \item{LineColor}{
Optional, name of color, then all points are connected by a curve
}
  \item{LineWidth}{
Optional, number defining the width of the curve
}
  \item{LineType}{
Optional, string defining the type of the curve
}
  \item{Showgrid}{
Optional, boolean
}
  \item{PlotIt}{
Optional, boolean, if false does not plot in order to change ploty layut by user
}
  \item{SaveIt}{
Optional, boolean, if true saves plot as html
}
}

\value{
plotly object
}

\author{
Michael Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DualaxisClassplot}}
}
\examples{
data(Lsun3D)
Classplot(Lsun3D$Data[,1],Lsun3D$Data[,2],Lsun3D$Cls)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Classplot}% use one of  RShowDoc("KEYWORDS")
\keyword{dotplot}% use one of  RShowDoc("KEYWORDS")
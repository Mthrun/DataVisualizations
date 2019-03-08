\name{DualaxisClassplot}
\alias{DualaxisClassplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dualaxis Classplot
}
\description{
Allows to plot two time series or features with one or two classification(a) as labeled  scatter plots.
The colors are the labels defined by the classification.
Usefull to see if temporal clustering has time dependent variations and for Hidden Markov Models (see Mthrun/RHmm on GitHub).
}
\usage{
DualaxisClassplot(X, Y1, Y2, Cls1,

Cls2, xlab = "X", y1lab = "Y1", y2lab = "Y2",

main = "Dual Axis Class Plot", Colors, Showgrid = TRUE, SaveIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
[1:n] numeric vector or time
}
  \item{Y1}{
[1:n] numeric vector of feauture
}
  \item{Y2}{
[1:n] numeric vector of feauture
}
  \item{Cls1}{
[1:n] numeric vector defining a classification of k1 classes
}
  \item{Cls2}{
Optional, [1:n] numeric vector defining a classification of k2 classes for \code{Y2}
}
  \item{xlab}{
Optional, string
}
  \item{y1lab}{
Optional, string
}
  \item{y2lab}{
Optional, string
}
  \item{main}{
Optional, string
}
  \item{Colors}{
[1:(k1+k2)] Colornames
}
  \item{Showgrid}{
Optional, boolean
}
  \item{SaveIt}{
Optional, boolean
}
}


\value{
plotly object
}
\author{
Michael Thrun
}



\seealso{
\code{\link{Classplot}}
}
\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{DualaxisClassplot}% use one of  RShowDoc("KEYWORDS")
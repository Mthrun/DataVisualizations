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
Classplot(X, Y, Cls,

xlab = "X", ylab = "Y",

main = "Class Plot", Colors,

LineColor = "grey", LineWidth = 1, LineType = NULL,

Showgrid = TRUE, SaveIt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
[1:n] numeric vector or time
}
  \item{Y}{
[1:n] numeric vector of feature
}
  \item{Cls}{
[1:n] numeric vector of k classes
}
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
Optional, name of color
}
  \item{LineWidth}{
Optional, number
}
  \item{LineType}{
Optional, string
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DualaxisClassplot}}
}
\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Classplot}% use one of  RShowDoc("KEYWORDS")

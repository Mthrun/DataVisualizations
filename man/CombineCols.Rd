\name{CombineCols}
\alias{CombineCols}
\alias{cbind_fill}

\title{
Combine vectors of various lengths
}
\description{
Combine arbitrary vectors of data, filling in missing rows with NaN
}
\usage{
CombineCols(\dots,na.rm=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
d vectors of arbitrary lengths, see example
}
  \item{na.rm}{
boolean: FALSE: fills with NaN
TRUE: filles with zeros
}
}
\details{
Robust alternative to \code{\link{cbind}} that fills missing values with nan instead of extending length of vector by duplicating elements
}
\value{
matrix of dimensionality of n x d with n beeing the length of the longest vector and d the number of vectors given as input
}

\author{
Craig Varrichio
}
\note{
special application by MCT of rowr cbind.fill which is now not on CRAN anymore
}

\seealso{
\code{CombineRows}
}

\examples{
CombineCols(c(1,2,3),c(1),c(2,3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cbind}% use one of  RShowDoc("KEYWORDS")
\keyword{cbind_fill}% __ONLY ONE__ keyword per line
\keyword{CombineCols}% _
\name{CombineRows}
\alias{CombineRows}
\alias{cbind_fill}
\title{
Combine matrices of various lengths
}
\description{
Combine arbitrary matrices of data, filling in missing columns with NaN
}
\usage{
CombineRows(\dots,na.rm=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{\dots}{
First argument is a matrix usually with named columns, thereafter either matrices or d vectors of arbitrary lengths, see example
}
\item{na.rm}{
boolean: FALSE: fills with NaN
TRUE: filles with zeros
}
}
\details{
Robust alternative to \code{\link{rbind}} that fills missing values with #NaN, tries to match given column names
if matrices are inserted otherwise fills up the missing columns at the end.

The first argument has to be a matrix. It is assumed that this matrix has to be filled up and other arguments or not of bigger size than d columns. Otherwiese the further elements stored in columns >d are ignored.
}
\value{
matrix of dimensionality of n x d with n beeing the number of rows of the first argument and d the number columns of the first argument given as input
}

\author{
Michael Thrun
}


\seealso{
\code{CombineRows}
}

\examples{
matrix_pattern=cbind(c(1,2,3),c(4,5,6),c(7,8,9))

CombineRows(matrix_pattern,c(1),c(2,3))


CombineRows(matrix_pattern,cbind(c(1,2,3),c(4,5,6)))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{rbind}% use one of  RShowDoc("KEYWORDS")
\keyword{rbind_fill}% __ONLY ONE__ keyword per line
\keyword{CombineRows}% _
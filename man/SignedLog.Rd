\name{SignedLog}
\alias{SignedLog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Signed Log
}
\description{
Computes the Signed Log if Data
}
\usage{
SignedLog(Data,Base="Ten")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] Data matrix with n cases and d variables
}
  \item{Base}{
Either "Ten", "Two", "Zero", or any number.
}
}
\details{
A neat transformation for data, it it has a better representation on the log scale.
}
\value{
Transformed Data
}
\references{
Prof. Dr. habil. A. Ultsch, Lectures in Knowledge Discovery, 2014.
}
\author{
Michael Thrun
}
\note{
Number Selections for \code{Base} currently experimental.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{log}}
}
\examples{
MDplot(SignedLog(cbind(ITS,MTY)*(-1),Base = "Ten"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SignedLog}% use one of  RShowDoc("KEYWORDS")
\keyword{slog}% __ONLY ONE__ keyword per line
\keyword{log}
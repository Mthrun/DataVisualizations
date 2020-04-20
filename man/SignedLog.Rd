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
Number Selections for \code{Base} for 2,10, "Two" or "Ten" add 1 to every datapoint as defined in the lectures.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{log}}
}
\examples{
# sampling is done
# because otherwise the example takes too long
# in the CRAN check
data('ITS')
ind=sample(length(ITS),1000)

MDplot(SignedLog(cbind(ITS[ind],MTY[ind])*(-1),Base = "Ten"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SignedLog}% use one of  RShowDoc("KEYWORDS")
\keyword{slog}% __ONLY ONE__ keyword per line
\keyword{log}
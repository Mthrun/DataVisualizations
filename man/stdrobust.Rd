\name{Stdrobust}
\alias{Stdrobust}
\alias{stdrobust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Standard Deviation Robust }
\description{
  Robust empirical estimation for standard deviation.NaNs are ignored.

}
\usage{
Stdrobust(x, lowInnerPercentile=25,na.rm=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numerical matrix }
  \item{lowInnerPercentile}{optional; default=25; standard deviation aproximated by percentilinterval.}
      \item{na.rm}{	a boolean evaluating to TRUE or FALSE indicating whether all non finite values should be stripped before the computation proceeds.}
}

\value{
  
  \item{out}{a vector with the calculated standard deviation for the column}

}
\author{ Zornitsa Manolova }

\seealso{\code{\link{sd}} 
\code{\link{quantile}} 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ std }
\keyword{ robust }

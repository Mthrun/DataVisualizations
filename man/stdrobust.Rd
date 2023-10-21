\name{stdrobust}
\alias{stdrobust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Standard Deviation Robust }
\description{
  Robust empirical estimation for standard deviation.NaNs are ignored.

}
\usage{
stdrobust(x, LowInnerPercentile=25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numerical matrix }
  \item{lowInnerPercentile=25}{ optianal; standard deviation aproximated by percentilinterval.}
}

\value{
  
  \item{out}{a vector with the calculated standard deviation for the column}

}
\author{ Zornitsa Manolova }


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ std }
\keyword{ robust }

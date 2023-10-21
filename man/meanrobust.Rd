\name{meanrobust}
\alias{meanrobust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Robust Empirical Mean Estimation }
\description{
  If the input is a matrix the mean value will be compute for every column.
}
\usage{
meanrobust(x, p=0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vetor or matrix }
  \item{p=0.1}{ default=0.1; percent of the top- and bottomcut from x}
}
\author{ Zornitsa Manolova }
\seealso{\code{\link{mean}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mean }
\keyword{ robust }% __ONLY ONE__ keyword per line

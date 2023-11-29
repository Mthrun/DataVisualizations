\name{Meanrobust}
\alias{Meanrobust}
\alias{meanrobust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Robust Empirical Mean Estimation }
\description{
  If the input is a matrix the mean value will be compute for every column.
}
\usage{
Meanrobust(x, p=0.1,na.rm=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vetor or matrix }
  \item{p}{default=0.1; percent of the top- and bottomcut from x}
    \item{na.rm}{	a boolean evaluating to TRUE or FALSE indicating whether all non finite values should be stripped before the computation proceeds.}
}
\author{ Zornitsa Manolova }
\seealso{\code{\link{mean}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mean }
\keyword{ robust }% __ONLY ONE__ keyword per line

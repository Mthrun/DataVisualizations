
\name{ParetoRadiusV2}
\alias{ParetoRadiusV2}
\title{ParetoRadius for distributions}
\usage{
ParetoRadiusV2(Data, maximumNrSamples = 10000,
  plotDistancePercentiles = FALSE)
}
\arguments{
\item{Data}{numeric data vector}

\item{maximumNrSamples}{Optional, numeric. Maximum number for which the distance calculation can be done. 1000 by default.}

\item{plotDistancePercentiles}{Optional, logical. If TRUE, a plot of the percentiles of distances is produced. FALSE by default.}
}
\value{
numeric value, the Pareto radius.
}
\description{
Calculation of the ParetoRadius i.e. the 18 percentiles of all mutual Euclidian distances in data.
}
\details{
The Pareto-radius of the hyperspheres is derived from the optimization of information for minimal set size.
ParetoRadius() is a kernel density estimation for variable intervals. It works only on Data without missing values (NA) or NaN. In other cases, please use ParetoDensityEstimation directly.
}
\note{
This the second version of the function prior available in \pkg{AdaptGauss}
}

\references{
Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.

\strong{See Also}

ParetoDensityEstimationV2, OptimalNoBins
}
\author{
Michael Thrun
}

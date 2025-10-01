
\name{ParetoRadius_fast}
\alias{ParetoRadius_fast}
\title{Fast ParetoRadius for distributions}
\usage{
ParetoRadius_fast(Data, maximumNrSamples = 10000,na.rm=TRUE)
}
\arguments{
\item{Data}{numeric data vector}

\item{maximumNrSamples}{Optional, numeric. Maximum number for which the distance calculation can be done. 1000 by default.}

\item{na.rm}{(Optional: Default=TRUE). Remove na.}
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

\references{
Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, in Baier, D.; Werrnecke, K. D., (Eds), Innovations in classification, data science, and information systems, Proc Gfkl 2003, pp 91-100, Springer, Berlin, 2005.

}
\author{
Michael Thrun
}

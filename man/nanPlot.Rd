\name{nanPlot}
\alias{nanPlot}
\title{Plot not finite values as a bar plot for each feature d}
\usage{
nanPlot(Data,Names)
}
\arguments{
\item{Data}{[1:n,1:d] data cases in rows, variables/features in columns}
\item{Names}{[1:d] optional vector of string describing the names of the features}
}
\description{
percentage are visualized as a bar plot. does not work well with to many features
}
\author{
Michael Thrun
}

\examples{
data("ITS")
data("MTY")

nanPlot(cbind(ITS,MTY),Names=c('ITS','MTY'))

}
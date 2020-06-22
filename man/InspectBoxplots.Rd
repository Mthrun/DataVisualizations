\name{InspectBoxplots}
\alias{InspectBoxplots}
\title{
Inspect Boxplots
}
\description{
Enables to inspect the boxplots for multiple variables in ggplot2 syntax. Each boxplot also has a point for the mean of the variable.
}
\usage{
InspectBoxplots(Data, Names,Means=TRUE)
}

\arguments{
  \item{Data}{
    Matrix containing the data. Each column is one variable.}
  \item{Names}{
    Optional: Names of the variables. If missing the columnnames of data are used.
  }
  \item{Means}{
    Optional: TRUE: with mean, FALSE: Only median.

}
}

\value{
The ggplot object of the boxplots
}

\author{
Felix Pape
}
\examples{
x <- cbind(A = rnorm(200, 1, 3), B = rnorm(100, -2, 5))
InspectBoxplots(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{boxplot}

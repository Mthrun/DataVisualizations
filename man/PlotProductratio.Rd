\name{PlotProductratio}
\alias{PlotProductratio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Product-Ratio Plot
}
\description{
The product-ratio plot as defined in [Tukey, 1977, p. 594].

}
\usage{
PlotProductratio(x, y, na.rm = FALSE, 

main='Product Ratio Analysis',xlab = "Log of Ratio",ylab = "Root of Product", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
[1:n] positive numerical vector, negativ values are removed automatically
}
  \item{y}{
[1:n] positive numerical vector, negativ values are removed automatically
}
  \item{na.rm}{
Function may not work with non finite values. If these cases should be automatically removed, set parameter TRUE
}
  \item{main}{
see \code{plot} 
}
  \item{ylab}{
see \code{plot} 
}
  \item{xlab}{
see \code{plot} 
}
  \item{\dots}{
further arguments passed on to \code{plot} 
}
}
\details{
In the case where there are many instances of very small values, but a small number of very large ones, this plot is usefull [Tukey, 1977, p. 615].
}
\value{
matrix[1:n,2] with sqrt(x*y) and log(x/y) as the two columns
}
\references{
[Tukey, 1977]  Tukey, J. W.: Exploratory data analysis, United States Addison-Wesley Publishing Company, ISBN: 0-201-07616-0, 1977.

}
\author{
Michael Thrun
}

\examples{
#Beware: The data does no fit ne requirements for this approach
data('ITS')
data(MTY)
PlotProductratio(ITS,MTY)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ProductRatioPlot}% use one of  RShowDoc("KEYWORDS")
\keyword{ProductRatioPlotAnalysis}% __ONLY ONE__ keyword per line
\keyword{Zipf}
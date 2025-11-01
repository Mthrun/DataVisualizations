\name{PDEstrip}
\alias{PDEstrip}
\title{
1D Density Strip based on Pareto Density Estimation (PDE)
}
\description{
This function renders a single variable's probability density as a horizontal "heat strip", 
where color encodes local density values. Low-density regions are shown in blue/green, 
medium-density regions in yellow, and high-density regions in orange/red. 
It provides a compact alternative to violin or box plots for visualizing the distribution 
of a single variable.
}
\usage{
PDEstrip(Feature, palette = c("blue","green","yellow","orange","red"))
}
\arguments{
  \item{Feature}{
  Numeric vector. Finite values are used for Pareto Density Estimation (PDE). 
  Non-finite values are ignored.}
  \item{palette}{
  Character vector of colors defining the low-to-high density gradient, 
  passed to \code{ggplot2::scale_fill_gradientn()}. 
  Default is a five-color palette ranging from blue (low density) to red (high density).}
}
\value{
A \code{ggplot} object showing a 1D density strip for the given feature.
}
\details{
Density is estimated via Pareto Density Estimation (PDE), a robust and adaptive 
approach to probability density estimation [Ultsch, 2005]. The returned 
density values are min-max normalized to [0,1] and mapped to the user-specified 
color gradient. The strip is drawn with \code{geom_tile()} at a fixed y=1, so that 
each feature is visualized as a single horizontal band. The feature name is 
automatically captured from the input expression and used as the x-axis label.
}
\note{
To match multi-variable strip plots visually, the theme and palette can be 
customized for consistency. If the feature contains very few unique values, 
the strip may appear coarse due to sparse kernel locations from PDE.
}
\author{
Michael Thrun
}
\examples{
set.seed(1)
x <- c(rnorm(800, 0, 1), rnorm(200, 3, 0.7))
PDEstrip(x)

# Alternative palette
PDEstrip(x, palette = c("navy","skyblue","yellow","orange","darkred"))
}
\references{Ultsch, A.: Pareto Density Estimation: A Density Estimation for Knowledge Discovery, Baier D., Wernecke K.D. (Eds), In Innovations in Classification, Data Science, and Information Systems - Proceedings 27th Annual Conference of the German Classification Society (GfKL) 2003, Berlin, Heidelberg, Springer, pp, 91-100, 2005.}

\keyword{density}
\keyword{visualization}
\concept{Pareto Density Estimation}
\concept{heat strip}
\concept{violin alternative}

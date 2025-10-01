\name{MDstrips}
\alias{MDstrips}
\title{High-dimensional Density Strips based on Pareto Density Estimation}
\description{
\code{MDstrips} visualizes the distribution of each variable in a dataset as a vertical strip of colored tiles. 
Density is estimated by Pareto Density Estimation (PDE). Low-density regions are shown in blue/green, 
mid-density regions in yellow, and high-density regions in orange/red. 
This is a strip-based alternative to \code{MDplot} for the high-dimensional case of d>100 in which violins are easy visible due to restricted widht of screen.
}
\usage{
MDstrips(Data, Ordering = "Default", Scaling = "None",
         QuantityThreshold = 50, UniqueValuesThreshold = 12, SampleSize = 5e+05,
         LabelThreshold = 100, LabelMax = 40, LabelEvery = NULL, LabelVariables = NULL,
         SizeOfJitteredPoints=1,
         palette = c("blue","green","yellow","orange","red"), ylab, main, BW = TRUE)
}
\arguments{
  \item{Data}{Numeric matrix containing the data. Each column is one variable.}
  \item{Ordering}{Optional:x String specifying ordering of variables on the axis. 
  Options are \code{"Default"}, \code{"Columnwise"}, \code{"AsIs"}, \code{"Alphabetical"}, 
  \code{"Average"}, \code{"Bimodal"}, \code{"Variance"}, \code{"Statistics"}.}
  \item{Scaling}{Optional, Data scaling method. Options: \code{"None"}, \code{"Percentalize"}, 
  \code{"CompleteRobust"}, \code{"Robust"}, \code{"Log"}.}
  \item{QuantityThreshold}{Optional: Minimum number of finite values required to estimate a distribution.}
  \item{UniqueValuesThreshold}{Optional: Minimum number of unique values required to estimate a distribution.}
  \item{SampleSize}{Optional: Maximum sample size. Larger datasets are subsampled for faster computation.}
  \item{LabelThreshold}{Optional: If number of variables exceeds this threshold, not all x-axis labels are shown.}
  \item{LabelMax}{Optional: Maximum number of x-axis labels shown when many variables are present.}
  \item{LabelEvery}{Optional: Integer. Show every \eqn{k}th variable label (overrides \code{LabelMax}).}
  \item{LabelVariables}{Optional: Character vector of variables to label explicitly (overrides other options).}
  \item{SizeOfJitteredPoints}{
	 Optional: scalar. If not enough unique values for density estimation are given, data points are jittered. This parameter defines the size of the points. 
  }
  \item{palette}{Optional: Vector of colors for density scale, from low to high density.}
  \item{ylab}{Optional: Label for the y-axis (range of values in which PDE is estimated).}
  \item{main}{Optional: Title of the plot.}
  \item{BW}{Optional: Logical. If \code{TRUE}, use black-and-white theme and remove grid lines.}
}
\details{
This function is intended for high-dimensional but univariate distribution visualization where MDplot, violin plots or boxplots 
become unreadable. By using density-colored strips, 
a large number of variables can be compared side-by-side. 

Pareto Density Estimation (PDE) is used for robustness of univariate density estimation.

If too few values are present for PDE estimation (less than \code{QuantityThreshold} or 
less than \code{UniqueValuesThreshold} unique values), jittered points are drawn instead.
}
\value{
Returns a \code{ggplot} object visualizing the density strips.
}
\author{
Michael Thrun
}
\seealso{
\code{\link{MDplot}} for Mirrored-density plot and the explaination of \code{Ordering} and \code{Scaling} in Detail.
}
\examples{
# Example with toy data
set.seed(123)
X <- matrix(rnorm(1000), ncol = 10)
colnames(X) <- paste0("V", 1:10)

# Default plot
MDstrips(X)

# With statistical ordering and black-and-white theme
MDstrips(X, Ordering = "Statistics", BW = TRUE)

# Show only every 2nd variable label
MDstrips(X, LabelEvery = 2)

# Show only specific variables
MDstrips(X, LabelVariables = c("V1","V5","V10"))
}

\name{ClassPDEplotMaxLikeli}
\alias{ClassPDEplotMaxLikeli}
\title{Unweighted Class-Conditional Pareto Densities on a Common Grid}
\description{
Estimates and plots one unweighted Pareto density \code{p(x | C = k)} for each
class. All classes are evaluated on kernels and with a Pareto radius estimated
from the pooled data.
}
\usage{
ClassPDEplotMaxLikeli(Data, Cls,
  ColorSequence,
  ClassNames = NULL, PlotLegend = TRUE, MinAnzKernels = 0,
  PlotNorm = 0, main = "Pareto Density Estimation (PDE)",
  xlab = "Data", ylab = "ParetoDensity", xlim = NULL,
  ylim = NULL, lwd = 1, \dots)
}
\arguments{
  \item{Data}{Numeric vector containing the observations to be plotted.}
  \item{Cls}{Vector of class identifiers with one entry per observation. The
  identifiers need not be consecutive or positive. Missing class assignments
  are not supported by the underlying class-validation routine.}
  \item{ColorSequence}{Vector of colors used for the class curves.Default is DataVisualizations::DefaultColorSequence[-2]}
  \item{ClassNames}{Optional vector containing exactly one displayed name per
  class. By default, the validated class identifiers are used.}
  \item{PlotLegend}{Logical or numeric flag controlling whether the ggplot
  legend is displayed. The default is \code{TRUE}.}
  \item{MinAnzKernels}{Minimum number of kernels used when the pooled grid is
  estimated. Values less than or equal to zero are replaced by \code{100}.}
  \item{PlotNorm}{Normal-reference overlay: \code{0} draws no reference,
  \code{1} uses the empirical mean and standard deviation of each class, and
  \code{2} uses a 10-percent trimmed mean and \code{Stdrobust}.}
  \item{main}{Plot title.}
  \item{xlab}{Label for the x axis.}
  \item{ylab}{Label for the y axis.}
  \item{xlim}{Optional numeric vector of length two defining the x-axis range.}
  \item{ylim}{Optional numeric vector of length two defining the y-axis range.}
  \item{lwd}{Numeric line width for the class-density and normal-reference
  curves.}
  \item{\dots}{Further arguments passed to the \code{geom_line} layer that
  draws the class Pareto densities.}
}
\details{
No empirical class-prior weights are applied. The curve for class \eqn{k}
therefore represents \code{p(x | C = k)}. Selecting the largest curve at a
value of x is a maximum-likelihood class decision. It is equivalent to a
maximum-a-posteriori decision only when all class priors are equal.

This is the central distinction from \code{ClassPDEplot}. That function
multiplies every class curve by its empirical prior and plots
\code{P(C = k) * p(x | C = k)}. Thus, imbalanced class sizes affect
\code{ClassPDEplot} but do not rescale the curves returned here.

The common kernels and Pareto radius in this function are estimated from all
finite observations pooled across classes. In contrast,
\code{ClassPDEplot(SameKernelsAndRadius = 1)} obtains its shared grid and radius
from the class with the largest number of unique finite observations.
}
\value{
An invisible list with the following components:
\item{Kernels}{Numeric vector containing the common evaluation kernels.}
\item{ClassParetoDensities}{Numeric matrix of unweighted class-conditional
Pareto densities, with one column per class.}
\item{ggobject}{A \code{ggplot2} plot object that can be printed or modified.}
\item{Dataframe}{Long-format data frame used for the class-density layers.}
}
\author{Felix Pape}
\seealso{\code{\link{ClassPDEplot}},
\code{\link{ParetoDensityEstimation}}}
\examples{
\dontshow{
data(ITS)
ind=sample(length(ITS),1000)
Classification=c(rep(1,8805),rep(2,2389))
result=DataVisualizations::ClassPDEplotMaxLikeli(ITS[ind],Classification[ind])
}

\donttest{
data(ITS)
Classification=c(rep(1,8805),rep(2,2389))
result=DataVisualizations::ClassPDEplotMaxLikeli(ITS,Classification)
result$ggobject
}
}
\references{
Aubert, A. H., Thrun, M. C., Breuer, L., and Ultsch, A. (2016).
Knowledge discovery from high-frequency stream nitrate concentrations:
hydrology and biology contributions. \emph{Scientific Reports}, 6, 31536.
\doi{10.1038/srep31536}
}

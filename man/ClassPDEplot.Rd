\name{ClassPDEplot}
\alias{ClassPDEplot}
\title{Prior-Weighted Pareto Density Curves for Classes}
\description{
Estimates a Pareto density for each class and multiplies every class-conditional
curve by its empirical class prior. The plotted quantity for class \eqn{k} is
proportional to \code{P(C = k) * p(x | C = k)}.
}
\usage{
ClassPDEplot(Data, Cls, ColorSequence, ColorSymbSequence,
  PlotLegend = 1, SameKernelsAndRadius = 0, xlim, ylim, \dots)
}
\arguments{
  \item{Data}{Numeric vector containing the observations to be plotted.}
  \item{Cls}{Vector of class identifiers with one entry per observation. The
  identifiers need not be consecutive or positive. Missing class assignments
  are not supported by the underlying class-validation routine.}
  \item{ColorSequence}{Optional vector of plotting colors. The default is
  \code{DataVisualizations::DefaultColorSequence} without yellow.}
  \item{ColorSymbSequence}{Retained for backward compatibility. Plot symbols
  are currently not used because the function draws lines.}
  \item{PlotLegend}{Logical or numeric flag. A legend is drawn when equal to
  \code{1}; the default is \code{1}.}
  \item{SameKernelsAndRadius}{Controls the evaluation grid. With \code{0},
  each class receives its own kernels and Pareto radius. With \code{1}, all
  classes use the kernels and radius estimated from the class having the
  largest number of unique finite observations.}
  \item{xlim}{Optional numeric vector of length two defining the x-axis range.
  By default, the range of the finite values in \code{Data} is used.}
  \item{ylim}{Optional numeric vector of length two defining the y-axis range.}
  \item{\dots}{Further arguments passed to the base \code{plot} calls.}
}
\details{
This function incorporates empirical class frequencies. If class \eqn{k}
contains \eqn{n_k} of the \eqn{n} observations, its Pareto density is multiplied
by \eqn{n_k/n}. Consequently, the curves are prior-weighted class-conditional
densities. They are the unnormalized numerators used by a
maximum-a-posteriori or Bayes decision rule. They are not posterior
probabilities unless the curves are additionally normalized across classes at
each x value.

The returned \code{ClassParetoDensities} use the same prior weighting in both
kernel modes. This differs from \code{ClassPDEplotMaxLikeli}, which leaves every
class density unweighted and therefore compares \code{p(x | C = k)} directly.

With \code{SameKernelsAndRadius = 0}, classes can have different kernel vectors
and Pareto radii. With \code{SameKernelsAndRadius = 1}, all returned density
columns share one kernel vector, which permits pointwise comparison.
}
\value{
An invisible list with the following components:
\item{Kernels}{When \code{SameKernelsAndRadius = 0}, a list containing one
kernel vector per class. When it is \code{1}, the common numeric kernel vector.}
\item{ClassParetoDensities}{Prior-weighted class density curves. This is a list
when \code{SameKernelsAndRadius = 0} and a numeric matrix with one column per
class when it is \code{1}.}
}
\author{Michael Thrun}
\seealso{\code{\link{ClassPDEplotMaxLikeli}},
\code{\link{ParetoDensityEstimation}}}
\examples{
\dontshow{
data(ITS)
ind=sample(length(ITS),1000)
Classification=c(rep(1,8805),rep(2,2389))
result=DataVisualizations::ClassPDEplot(ITS[ind],Classification[ind])
}

\donttest{
data(ITS)
Classification=c(rep(1,8805),rep(2,2389))
result=DataVisualizations::ClassPDEplot(ITS,Classification)
result$ClassParetoDensities
}
}

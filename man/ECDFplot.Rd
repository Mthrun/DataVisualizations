\name{ECDFplot}
\alias{ECDFplot}
\title{Compare Empirical Cumulative Distributions in One Plot}
\description{
Draws empirical cumulative distribution functions for user-supplied samples
as step curves in a single plot. The samples may have different lengths.
}
\usage{
ECDFplot(Curves, Names = NULL, xlab = "Value",
         ylab = "Cumulative probability",
         main = "Empirical cumulative distributions",
         col = c("#555555", "#2166ac", "#16845b", "#c36720"),
         lty = seq_len(6L), lwd = 2, LegendPosition = "bottomright",
         xlim = NULL, ReferenceLines = c(0.5, 0.8, 0.9),
         PlotIt = TRUE, ...)
}
\arguments{
  \item{Curves}{A numeric vector, a list of numeric vectors, or a numeric
    matrix or data frame with one curve per column. List entries may differ
    in length. Every curve must contain at least one finite observation.}
  \item{Names}{Optional legend labels, one per curve. Defaults to list or
    column names; unnamed entries receive labels such as \code{"Curve 1"}.}
  \item{xlab, ylab, main}{Horizontal axis label, vertical axis label and title.
    All three have general-purpose defaults and can be overridden.}
  \item{col, lty, lwd}{Curve colors, line types and line widths. Vectors are
    recycled to the number of curves.}
  \item{LegendPosition}{Position accepted by \code{\link[graphics]{legend}},
    or \code{NULL} to omit the legend.}
  \item{xlim}{Optional finite, increasing horizontal axis limits. By default,
    the pooled range is extended by a small margin. Constant samples are supported.}
  \item{ReferenceLines}{Probabilities for horizontal guide lines, between
    zero and one. Use \code{NULL} to omit them.}
  \item{PlotIt}{If \code{TRUE}, draw the plot. If \code{FALSE}, only compute
    and return the curve coordinates.}
  \item{...}{Additional arguments to \code{\link[graphics]{plot.default}},
    such as \code{cex.main}, \code{las} or \code{xaxs}. The arguments
    \code{type} and \code{ylim} are fixed by this function.}
}
\details{
For a sample of size \eqn{n}, the curve at \eqn{x} is the fraction of
observations less than or equal to \eqn{x}. Ties form a single jump. Curves
are right-continuous and are neither smoothed nor linearly interpolated.
Each curve uses its own sample size as its denominator.

Missing and infinite observations are omitted with a warning and counted
in \code{Removed}. The resulting ECDF is conditional on finite observations.
In benchmark applications, failed runs should therefore be reported separately;
encoding them as missing values does not penalize their curve.

For losses where smaller values are preferable, curves further to the left
indicate smaller losses. Crossing curves can favor different methods at
different quantiles; the plot alone does not establish an overall winner.
Existing multi-panel layouts and graphical settings are not changed.
}
\value{
Invisibly returns a list containing:
\item{Curves}{Named list of data frames with columns \code{X} (unique sorted
  observations) and \code{Y} (the ECDF at those observations).}
\item{N}{Named vector of the number of finite observations per curve.}
\item{Removed}{Named vector of the number of omitted observations per curve.}
\item{xlim}{The horizontal axis limits used for plotting.}
}
\seealso{\code{\link[stats]{ecdf}}, \code{\link{CCDFplot}}}
\examples{
Curves = list(Original = c(0.9, 0.3, 0.5, 0.5, 1.2),
              Alternative = c(0.2, 0.4, 0.4, 0.6))
ECDFplot(Curves)
ECDFplot(Curves, xlab = "Search loss (smaller is better)",
         ylab = "Fraction of runs with loss <= x",
         main = "Comparison of search methods")

Result = ECDFplot(Curves, PlotIt = FALSE)
Coordinates = Result$Curves
SampleSizes = Result$N
Coordinates
SampleSizes
}
\keyword{hplot}

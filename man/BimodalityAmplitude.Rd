\name{BimodalityAmplitude}
\alias{BimodalityAmplitude}
\alias{BimodalityAmplitudeCpp}
\title{Bimodality Amplitude}
\description{
Computes the bimodality amplitude after Zhang et al. (2003) from a
one-dimensional kernel density estimate.

\code{BimodalityAmplitude()} is the public compatibility wrapper.
\code{BimodalityAmplitudeCpp()} performs input validation, optional
reproducible subsampling, density estimation, C++-based peak and antimode
analysis, and optional plotting.
}
\usage{
BimodalityAmplitude(
  x,
  PlotIt = FALSE,
  SampleSize = Inf,
  SampleSeed = NULL,
  na.rm = FALSE,
  ...
)

BimodalityAmplitudeCpp(
  x,
  PlotIt = FALSE,
  SampleSize = Inf,
  SampleSeed = NULL,
  na.rm = FALSE,
  ...
)
}
\arguments{
  \item{x}{
    Numeric vector containing the observations for which the bimodality
    amplitude is calculated. Complex values, matrices, and arrays are not
    supported.
  }

  \item{PlotIt}{
    Logical scalar. If \code{TRUE}, plots the estimated density together with
    the two maxima and the selected antimode when these are available.
    Default is \code{FALSE}.
  }

  \item{SampleSize}{
    Integer scalar of at least three or \code{Inf}. If finite and smaller than
    \code{length(x)}, a sample of this size is drawn without replacement before
    density estimation. Default is \code{Inf}, which uses all observations.
  }

  \item{SampleSeed}{
    \code{NULL} or one non-negative integer. Optional seed used only for the
    subsampling step. The previous global random-number-generator state is
    restored after sampling. Default is \code{NULL}.
  }

  \item{na.rm}{
    Logical scalar. If \code{TRUE}, non-finite observations are removed before
    the calculation. If \code{FALSE}, any non-finite value causes an error.
    Default is \code{FALSE}.
  }

  \item{\dots}{
    Further arguments passed to \code{stats::density()}, for example
    bandwidth- or kernel-related arguments.
  }
}
\value{
Numeric scalar in the interval \code{[0, 1]} containing the estimated
bimodality amplitude.

A value of zero is returned when fewer than three usable observations remain,
when fewer than two distinct values remain, when no valid antimode/peak
configuration can be identified, or when the C++ core returns a non-positive
or non-finite amplitude.

The final positive amplitude is clipped to \code{[0,1]}.
}
\details{
The bimodality amplitude is intended to quantify the strength of a bimodal
structure. Values close to zero indicate little or no detectable bimodality,
whereas larger values indicate a more pronounced separation between two
adjacent density maxima around an antimode.

The public function \code{BimodalityAmplitude()} retains the historical
function name and delegates the computation to
\code{BimodalityAmplitudeCpp()}.

\strong{Density estimation}

Density estimation is performed with \code{stats::density(x, ...)}. The search
for adjacent density maxima, antimodes, and the maximum bimodality amplitude
is delegated to \code{BimodalityAmplitudeCoreCpp()}.

Historically, the implementation used a direct R-side search on the density
estimate. The current implementation keeps the density estimation in R while
performing the turning-point analysis in C++.

\strong{Subsampling}

For large vectors, runtime can be bounded with \code{SampleSize}. If
\code{SampleSize} is finite and smaller than the number of usable
observations, sampling is performed without replacement.

If \code{SampleSeed} is supplied, the helper
\code{.BimodalityAmplitudeLocalSeed()} temporarily sets the seed for this
sampling step and restores the previous \code{.Random.seed} afterwards.
Consequently, reproducible subsampling does not permanently change the
caller's random-number-generator state.

\strong{Handling of missing and non-finite values}

If \code{na.rm = TRUE}, all non-finite values are discarded with
\code{is.finite()}. If \code{na.rm = FALSE}, the presence of any non-finite
observation causes an error.

\strong{Peak and antimode logic}

The C++ core receives the x- and y-coordinates of the kernel density estimate.
If no suitable turning point can be identified, the result is zero. For a
candidate antimode, the method compares the nearest density maximum on the
lower-index side with the nearest density maximum on the higher-index side and
retains the corresponding amplitude.

\strong{Plotting}

If \code{PlotIt = TRUE} and the C++ core identifies exactly two maxima and one
antimode, the density curve is plotted using base graphics. Vertical lines mark
the two maxima in blue and the antimode in dark green. Text annotations mark
the two maxima and the antimode \code{"A"}.

The plotting step does not alter the returned numeric amplitude.
}
\note{
The implementation was rewritten after the flow of an earlier function by
Sathish Deevi because the original implementation was considered incorrect.

The public wrapper keeps the historical API while the computational work is
performed by \code{BimodalityAmplitudeCpp()}.
}
\references{
Zhang, C., Mapes, B., and Soden, B. (2003).
Bimodality in tropical water vapour.
\emph{Quarterly Journal of the Royal Meteorological Society},
\bold{129}(594), 2847--2866.
}
\examples{
# Example 1: two separated normal components
set.seed(1)
data <- c(rnorm(299, 0, 1), rnorm(299, 5, 1))
BimodalityAmplitude(data)

# Example 2: reproducible subsampling for a larger vector
set.seed(2)
dist1 <- rnorm(2100, 5, 2)
dist2 <- dist1 + 11
data <- c(dist1, dist2)

BimodalityAmplitude(
  data,
  SampleSize = 1000,
  SampleSeed = 123
)

# Example 3: remove non-finite observations
x <- c(rnorm(100), NA, Inf)
BimodalityAmplitude(x, na.rm = TRUE)

\dontrun{
# Plot the density, peaks, and antimode
BimodalityAmplitude(data, PlotIt = TRUE)
}
}
\seealso{
\code{\link[stats]{density}}
}
\author{
Michael Thrun
}
\keyword{distribution}
\keyword{nonparametric}
\keyword{Bimodality}
\keyword{antimode}
\keyword{peaks}

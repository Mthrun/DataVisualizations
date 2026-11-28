\name{ParetoDensityEstimation}
\alias{ParetoDensityEstimation}
\title{Pareto Density Estimation for One-Dimensional Data}
\usage{
ParetoDensityEstimation(
  Data,
  paretoRadius,
  kernels = NULL,
  MinAnzKernels = 100,
  PlotIt = FALSE,
  Compute = "Cpp",
  Silent = FALSE
)
}
\arguments{
  \item{Data}{
    [1:n] numeric vector of data. Matrices and arrays are
    flattened before processing. Values that can be converted to numeric are
    converted, and non-finite observations are removed. At least one finite
    numeric observation must remain.
  }

  \item{paretoRadius}{
    Optional positive finite numeric scalar defining the Pareto radius.
    If missing, \code{NULL}, \code{numeric(0)}, \code{NA}, \code{NaN}, or
    zero, the radius is estimated from \code{Data}. Negative, infinite,
    complex, or non-scalar values are rejected. If not given it is estimated internally. Please do not set manually, instead use \code{\link{ParetoRadius}}.
  }

  \item{kernels}{
    Optional [1:m] numeric vector of evaluation positions. \code{NULL},
    \code{numeric(0)}, or scalar zero requests an automatically generated
    kernel grid. User-supplied kernels should contain finite real values. Density values are
    interpolated to these positions in the supplied order.
  }

  \item{MinAnzKernels}{
    Optional, Minimum number of kernels used for an automatically generated grid.
    Use one finite non-negative real value. Zero restores the default of
    \code{100}. The internal grid may be refined to resolve the Pareto radius;
    widely separated observations use a grid at the count changes.
  }

  \item{PlotIt}{
    Logical value. If \code{TRUE}, a base R line plot of the
    returned density is produced. For publication-quality graphics, interfaces
    such as \code{\link{PDEplot}} or \code{\link{MDplot}} are generally more
    appropriate.
  }

  \item{Compute}{
    Character scalar selecting the interval-counting backend. Supported values
    are \code{"R"}, \code{"Cpp"}, and \code{"Cpp_exp"}, matched without
    regard to case. For estimated radii and at least 5000 observations,
    \code{"R"} uses \code{ParetoRadius}, whereas the C++ backends use
    \code{ParetoRadius_fast}. With a supplied radius, all backends use the
    same grid, boundary mirroring, interpolation, and normalization.
  }

  \item{Silent}{
    Logical value. If \code{TRUE}, routine diagnostic messages and warnings
    are suppressed. Numerical failures and errors are still reported.
  }

}
\value{
A list with the following components:
\describe{
  \item{\code{kernels}}{
    [1:m] Numeric vector of positions at which \code{paretoDensity} is returned.
    With user-supplied kernels, their positions and order are preserved.
  }
  \item{\code{paretoDensity}}{
    [1:m] Numeric vector containing the trapezoidally normalized Pareto-density
    values. For one or two distinct input values, this contains the numerical
    representation of the empirical Dirac measure or measures.
  }
  \item{\code{paretoRadius}}{
    Supplied or estimated positive Pareto radius. It is zero in the Dirac
    special case.
  }
    \item{\code{Fit}}{
     Minimal counting context for ParetoDensityEstimationFromFit(); NULL in the Dirac special case.
     It contains only Data, unnormalized paretoCounts, and countingKernels when the counting grid differs from the returned
     kernels. Quantities already returned above are not duplicated. Has the elements \code{Data},
    \code{paretoCounts}, \code{countingKernels} and \code{interpolationUsed}.
    For full-support estimates without boundary reflection, it additionally
    contains \code{boundaryReflection = FALSE}.
  }
  


}
}

\description{
This function estimates the Pareto Density for the distribution of one variable. 
In the default setting the functions estimates internally the appropriate number and position of kernels to estimate the density properly. However, the user can set the \code{kernels} manually. 
In this case the internally estimated density is linearly interpolated to these positions; the requested grid does not change the Pareto radius or the internal counts.
See example for details.
}

\details{
Pareto Density Estimation (PDE) is a method for the estimation of probability density functions using hyperspheres. The Pareto-radius of the hyperspheres is derived from the optimization of information for minimal set size. It is shown, that Pareto Density is the best estimate for clusters of Gaussian structure. The method is shown to be robust when cluster overlap and when the variances differ across clusters.
This is the best density estimation to judge Gaussian Mixtures of the data see [Ultsch 2005].

If input argument \code{kernels} is set manually the output arguments \code{paretoDensity_internal} and \code{kernels_internal} provide the internally estimated density and kernels. Otherwise these arguments are NULL. The function provides a message if range of kernels and range of data does not overlap completly.

Typically it is not advisable to set \code{paretoRadius} manually. However in specific cases, the function \code{\link{ParetoRadius}} is used prior to calling this function. In such cases the input argument can use a priorly estimated \code{paretoRadius}.

With user-supplied kernels, the density is first evaluated and normalized on
the automatic internal grid and is then linearly interpolated to
the requested kernels. Values outside the internal interpolation range are set
to zero. The internal grid and density are returned separately.

\strong{Special case: one or two distinct values}

If \code{Data} contains only one or two distinct finite values and no positive
\code{paretoRadius} is supplied, the function
returns a normalized piecewise-linear numerical representation of the
empirical Dirac measure or measures. Each support value is represented by a
triangular impulse whose area equals its empirical relative frequency.
The two impulses use the same nominal width, so their heights reflect the
relative frequencies, up to numerical rounding. The total trapezoidal area is one. In this case,
\code{kernels}, \code{MinAnzKernels}, and \code{Compute} are ignored, and the
returned Pareto radius is zero. A supplied positive radius instead produces
ordinary Pareto-window counts on their full support, without reflection.

For reflected estimates, the internal integration grid includes both data
extrema and is confined to their interval. This avoids losing probability
mass between a boundary observation and the nearest grid midpoint.

Warnings are issued, unless \code{Silent = TRUE}, when input is flattened,
non-numeric input is converted, non-finite observations are removed, very few
observations or distinct values are supplied, the kernel limit is applied, or
a fallback grid size is required.
}

\examples{
   \donttest{
   #kernels are estimated internally
   data = c(rnorm(1000),rnorm(2000)+2,rnorm(1000)*2-1)
   pdeVal        <- ParetoDensityEstimation(data)
   plot(pdeVal$kernels,pdeVal$paretoDensity,type='l',xaxs='i',
   yaxs='i',xlab='Data',ylab='PDE')
   
   ##data exist outside of the range kernels
   kernels=seq(from=-3,to=3,by=0.01) 
   pdeVal        <- ParetoDensityEstimation(data,  kernels=kernels)
   plot(pdeVal$kernels,pdeVal$paretoDensity,type='l',xaxs='i',
   yaxs='i',xlab='Data',ylab='PDE')
   
   #data exists in-between kernels that is not measured
   pdeVal$paretoRadius#0.42
   kernels=seq(from=-8,to=8,by=1)
   pdeVal        <- ParetoDensityEstimation(data,  kernels=kernels)
   plot(pdeVal$kernels,pdeVal$paretoDensity,type='l',xaxs='i',
   yaxs='i',xlab='Data',ylab='PDE')
   }
   \dontshow{
     data = rnorm(100)
   pdeVal        <- ParetoDensityEstimation(data)
   }
\donttest{
set.seed(1)
Data <- c(rnorm(1000), rnorm(2000) + 2, rnorm(1000) * 2 - 1)

# Pareto radius and kernels are estimated internally
pde <- ParetoDensityEstimation(Data)
plot(
  pde$kernels,
  pde$paretoDensity,
  type = "l",
  xaxs = "i",
  yaxs = "i",
  xlab = "Data",
  ylab = "PDE"
)

# Two distinct values produce numerical Dirac representations
pde_dirac <- ParetoDensityEstimation(c(1,10),PlotIt = TRUE)
}
}
\references{
[Stier et al., 2026]  Stier, Q., Hoffmann, J. and Thrun, M. C.: Classifying with the Fine
Structure of Distributions: Leveraging Distributional Information for Robust
and Plausible Naive Bayes. \emph{Machine Learning and Knowledge Extraction},
8(1), 13. DOI: 10.3390/make8010013, 2026.

[Thrun et al., 2020]  Thrun, M. C., Gehlert, T. and Ultsch, A.: Analyzing the Fine Structure
of Distributions. \emph{PLoS ONE}, 15(10), e0238835,
DOI: 10.1371/journal.pone.0238835,  2020.

[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for
knowledge discovery. In D. Baier and K. D. Wernecke (eds.),
\emph{Innovations in Classification, Data Science, and Information Systems},
pp. 91--100. Springer, Berlin, 2005.

}
\seealso{
\code{\link{ParetoRadius}},
\code{\link{ParetoRadius_fast}},
\code{\link{OptimalNoBins}},
\code{\link{PDEplot}},
\code{\link{MDplot}}
}
\author{
Michael Thrun
}
\keyword{distribution}
\keyword{nonparametric}
\keyword{pdf}
\concept{probability density function}
\concept{density estimation}
\concept{univariate density estimation}
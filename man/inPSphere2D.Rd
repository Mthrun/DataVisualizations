\name{inPSphere2D}
\alias{inPSphere2D}
\title{2D data points in Pareto Sphere}
\description{
  This function determines the 2D data points inside a ParetoSphere with ParetoRadius.
}
\usage{
inPSphere2D(data, paretoRadius=NULL)
}

\arguments{
  \item{data}{numeric matrix of data.}
  \item{paretoRadius}{numeric value. radius of P-spheres. If not given, calculate by the function 'paretoRad' }
}

\value{
   numeric vector with the number of data points inside a P-sphere with ParetoRadius.
}
\author{Felix Pape}

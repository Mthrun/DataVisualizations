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
\note{based on an earlier version of MT2016, who used the same name and InputParameters for a function (as Fredericke Matz in the dbt, AG Datenbionik)}
\author{Felix Pape}
\note{Faster computation than inPSphere.}

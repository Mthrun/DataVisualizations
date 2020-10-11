\name{world_country_polygons}
\alias{world_country_polygons}
\docType{data}
\title{
world_country_polygons
}
\description{
world_country_polygons shapefile
}
\usage{data("world_country_polygons")}

\format{
world_country_polygons  stores data objects using classes defined
in the sp package or inheriting from those classes updated to sp Y= 1.4 and
rgdal >= 1.5.

Since DataVisualization Version 1.2.1 it stores now a CRS objects with a comment containing an WKT2 CRS representation, thanks to a suggestion of Roger Bivand.
}
\details{
Note that the rebuilt CRS object contains
a revised version of the input Proj4 string as well as the WKT2 string,
and may be used with both older and newer versions of sp. See maptools package for further details.
}
\source{
maptools package
}
\references{
maptools package
}
\examples{
\donttest{
data(world_country_polygons)
str(world_country_polygons)
}
}
\author{Hamza Tayyab, Michael Thrun}
\keyword{datasets}

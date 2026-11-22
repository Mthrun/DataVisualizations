\name{GermanPostalCodesShapes}
\alias{GermanPostalCodesShapes}
\alias{GermanPostalCodesShapesWeb}
\docType{data}
\title{
GermanPostalCodesShapes
}
\description{
Historical polygons for German postal areas.
The data are separate GitHub files. Package builds do not include these files.
}

\format{
A table with 3341504 coordinate rows for 8178 postal codes.
Columns long and lat contain longitude and latitude.
Column order gives the point order in each ring.
Column hole identifies interior rings.
Columns piece, id, and group identify polygon parts.
Column region contains five-character postal codes.
}
\details{
GermanPostalCodesShapesWeb is a separate sf object with fewer vertices.
The reduction keeps all postal regions, separate polygon parts, and holes.

Supply either object as PostalCodesShapes to Choroplethmap.
The historical coordinates use longitude and latitude (\code{ShapeCRS = 4326}).
Postal boundaries differ from municipality boundaries.
The historical object does not record the release date or the original attribution.
Establish this information and the license before public use.
}
\source{
\url{https://github.com/Mthrun/DataVisualizations/tree/master/data}.
The historical analysis refers to postal-codes-de and suche-postleitzahl.org.
These references do not identify a specific release or license.
}

\examples{
\dontrun{
external <- new.env()
load("GermanPostalCodesShapes.rda", envir=external)
str(external$GermanPostalCodesShapes)
}
}
\keyword{datasets}

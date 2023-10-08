\name{Worldmap}
\alias{Worldmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plots a world map by country codes
}
\description{
The Worldmap function is used in [Thrun, 2018].
}
\usage{
Worldmap(CountryCodes, Cls, Colors, 

MissingCountryColor = grDevices::gray(0.8), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CountryCodes}{
  [1:n] vector of characters identifying countries by ISO 3166 codes (2 or 3 letters)
}
  \item{Cls}{
[1:n] numerical vector of classification
}
  \item{Colors}{
optional, vector of charcters specifying the used colors
}
  \item{MissingCountryColor}{
if not all countries are specified in \code{CountryCodes} then the color of non relevant countries can be changed here
}
  \item{\dots}{
Further arguments passed on to \code{plot}, see also \code{sp::SpatialPolygons-class}
}
}

\value{
List of
\item{Colors}{[1:m] colors used in map, m<=n}
\item{CountryCodeList}{ [1:m] countries found, m<=n}
\item{world_country_polygons}{\code{SpatialPolygonsDataFrame}}
}
\references{
Used in

[Thrun, 2018] Thrun, M. C. : Cluster Analysis of the World Gross-Domestic Product Based on Emergent Self-Organization of a Swarm, 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, Foundation of the Cracow University of Economics, Zakopane, Poland, accepted, 2018. 

Source for shapefile: - package maptoops and 


Originally 'mappinghacks.com/data/TM_WORLD_BORDERS_SIMPL-0.2.zip', now available from \url{https://github.com/nasa/World-Wind-Java/tree/master/WorldWind/testData/shapefiles}
}
\author{
Michae Thrun
}

\examples{
# data from [Thrun, 2018]
Cls=c(1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 
2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 
1L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 
2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 3L, 2L, 2L, 2L, 1L, 
2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 
2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 
2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 
2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 
2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L
)
Codes=c("AFG", "AGO", "ALB", "ARG", "ATG", "AUS", "AUT", "BDI", "BEL", 
"BEN", "BFA", "BGD", "BGR", "BHR", "BHS", "BLZ", "BMU", "BOL", 
"BRA", "BRB", "BRN", "BTN", "BWA", "CAF", "CAN", "CH2", "CHE", 
"CHL", "CHN", "CIV", "CMR", "COG", "COL", "COM", "CPV", "CRI", 
"CUB", "CYP", "DJI", "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", 
"ESP", "ETH", "FIN", "FJI", "FRA", "FSM", "GAB", "GBR", "GER", 
"GHA", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD", "GTM", "GUY", 
"HKG", "HND", "HTI", "HUN", "IDN", "IND", "IRL", "IRN", "IRQ", 
"ISL", "ISR", "ITA", "JAM", "JOR", "JPN", "KEN", "KHM", "KIR", 
"KNA", "KOR", "LAO", "LBN", "LBR", "LCA", "LKA", "LSO", "LUX", 
"MAC", "MAR", "MDG", "MDV", "MEX", "MHL", "MLI", "MLT", "MNG", 
"MOZ", "MRT", "MUS", "MWI", "MYS", "NAM", "NER", "NGA", "NIC", 
"NLD", "NOR", "NPL", "NZL", "OMN", "PAK", "PAN", "PER", "PHL", 
"PLW", "PNG", "POL", "PRI", "PRT", "PRY", "ROM", "RWA", "SDN", 
"SEN", "SGP", "SLB", "SLE", "SLV", "SOM", "STP", "SUR", "SWE", 
"SWZ", "SYC", "SYR", "TCD", "TGO", "THA", "TON", "TTO", "TUN", 
"TUR", "TWN", "TZA", "UGA", "URY", "USA", "VCT", "VEN", "VNM", 
"VUT", "WSM", "ZAF", "ZAR", "ZMB", "ZWE")
Worldmap(Codes,Cls)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{world map}% use one of  RShowDoc("KEYWORDS")
\keyword{classification world map}% __ONLY ONE__ keyword per line

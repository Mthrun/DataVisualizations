\name{Choroplethmap}
\alias{plotChoroplethMap}
\alias{Choroplethmap}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Static and Interactive Choropleth Maps
}
\description{
The function connects measurements or classes to polygon regions by postal code.
It returns a static map. It can also return an interactive Leaflet map.
The interactive map shows labels when you move the pointer over a region.
Click a region to show its information.
The function does not download data or map tiles.
}
\usage{
Choroplethmap(Counts, PostalCodes, NumberOfBins = 0,

 Breaks4Intervals, percentiles = c(0.5, 0.95), 
 
 digits = 0, PostalCodesShapes, PlotIt = TRUE,

 DiscreteColors, HighColorContinuous = "red",

 LowColorContinuous = "deepskyblue1", NAcolor = "grey",

 ReferenceMap = FALSE, main = "Political Map of Germany",

 legend = "Range of values", Silent = TRUE, Interactive = FALSE,

 Categorical = is.factor(Counts) || is.character(Counts),

 ClassLabels = NULL, Aggregate = NULL, Labels = NULL,

 ShapeCRS = 4326, SimplifyKeep = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Counts}{
A vector of measurements. To use class codes, set \code{Categorical = TRUE}.
The function keeps missing values.
}
  \item{PostalCodes}{
A vector with one postal code for each element of \code{Counts}.
The function keeps leading zeros.
It adds leading zeros to numeric codes and digit strings with fewer than five digits.
It keeps other character identifiers unchanged.
}
  \item{NumberOfBins}{
A non-negative integer. Values of zero or one select a continuous color scale.
Larger values set the maximum number of automatic bins.
The function does not use this argument for classes.
}
  \item{Breaks4Intervals}{
Optional numeric boundaries for bins.
The boundaries must be finite and strictly increasing.
Each interval includes its lower boundary and excludes its upper boundary.
The function includes values below the first boundary and above the last boundary.
Thus, k boundaries give k+1 bins.
}
  \item{percentiles}{
Two probabilities between zero and one. The first must not exceed the second.
The function uses these probabilities to calculate the automatic boundaries.
It removes duplicate boundaries.
Constant data or data with only missing values use one bin.
}
  \item{digits}{
The number of decimal places in bin labels.
This argument does not change the interval boundaries.
}
  \item{PostalCodesShapes}{
An \pkg{sf} polygon object with a \code{region} column and a coordinate reference system (CRS).
You can also supply the historical table with columns \code{long, lat, order, hole, id, group, region}.
The function keeps separate polygon parts and holes.
If you omit this argument, the function uses the installed GermanPostalCodesShapes data, if available.
Otherwise, the function stops with an error.
The German PostalCodesShapes can be downloaded from \url{https://github.com/Mthrun/DataVisualizations/tree/master/data}.
}
  \item{PlotIt}{
Set to TRUE to show the static map or the interactive map.
}
  \item{DiscreteColors}{
One color for each class or bin, including empty classes or bins.
For a named vector, the names must match the class codes or bin labels.
}
  \item{HighColorContinuous}{
The color for the highest value on a continuous scale.
}
  \item{LowColorContinuous}{
The color for the lowest value on a continuous scale.
}
  \item{NAcolor}{
The color for regions with missing values.
}
  \item{ReferenceMap}{
Keep FALSE. The old Google interface is no longer available.
If TRUE, the function stops with an error.
You can add a reference layer to the returned Leaflet Widget.
}
  \item{main}{
The plot title.
}
  \item{legend}{
The legend title.
}
  \item{Silent}{
This argument remains available for older calls.
It does not suppress warnings about invalid data.
}
  \item{Interactive}{
Set to TRUE to create a Leaflet map.
This option requires \pkg{leaflet} and \pkg{htmltools}.
}
  \item{Categorical}{
Set to TRUE to use class codes without numeric bins.
The default is TRUE for factors and character vectors.
}
  \item{ClassLabels}{
An optional named character vector.
The names are class codes. The values are unique labels.
The vector order sets the legend order, including classes that are absent from the data.
}
  \item{Aggregate}{
An optional function that combines numeric values for the same postal code.
For example, use \code{min} to select the smallest value.
The default accepts identical values but stops if values differ.
A missing value and an observed value count as different values.
The function does not combine class codes.
}
  \item{Labels}{
An optional character vector with one label for each postal code.
The function adds these labels to the interactive map.
It converts special characters to safe HTML text.
Duplicate postal codes must have identical labels.
}
  \item{ShapeCRS}{
The CRS of the historical coordinate table.
The default is EPSG:4326 for the supplied German data.
For an sf object, the function uses the CRS of that object.
}
  \item{SimplifyKeep}{
The fraction of vertices to keep. Use a value greater than zero and no greater than one.
Values below one require \pkg{rmapshaper}.
The function simplifies shared boundaries together and protects separate polygon parts and holes.
It stops if the result has invalid geometry or different part or ring counts.
If this occurs, increase SimplifyKeep. The default value of one keeps all vertices.
}
}
\details{
The function uses \pkg{sf} and \pkg{ggplot2}.
The interactive map also uses \pkg{leaflet}.
The function no longer uses the internal \pkg{choroplethr} interfaces.
It repairs invalid planar geometry with \code{sf::st_make_valid} and gives a warning.
It does not change global sf/s2 settings.

The German postal-area polygons and income-tax example data are distributed
as separate files in the GitHub repository.
They are excluded from CRAN builds and other package builds.
Download the required files explicitly from
\url{https://github.com/Mthrun/DataVisualizations/tree/master/data}.
Load the files into a separate environment and supply the shape object through
\code{PostalCodesShapes}. Supply the corresponding measurements or classes
through \code{Counts} and their postal codes through \code{PostalCodes}.
Installation from GitHub does not install these data files.
Package installation and checks do not download these files.

For the income-tax example, use \code{Categorical = TRUE} and named colors.
The classes are 0 = not classified, 1 = low quota, and 2 = high quota.
Class 3 is promoted (red). Class 4 is sponsors (magenta).
Historical postal areas approximate municipality areas.
}
\value{
An invisible list with:
\item{chorR6obj}{An environment for older calls.
Its \code{$render()} function returns Plot.
It is not a choroplethr R6 object.
Other historical R6 methods are not available.
The \code{$render_with_reference_map()} function stops with an error.}
\item{DataFrame}{One input row for each postal code after aggregation.
For classes and bins, valueReal contains the original values.}
\item{Plot}{A ggplot object.}
\item{Widget}{A Leaflet htmlwidget, or NULL.}
\item{MapData}{An sf object with one row for each region, its values, and optional labels.
Use \code{sf::st_write} to save this object.}
}
\references{
[Thrun/Ultsch, 2018] Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 
}
\author{
Michael Thrun
}
\note{
Since version 1.5.1, conflicting duplicate values require an explicit Aggregate function.
Use \code{Aggregate = min} for the previous minimum rule.
Classes use a separate mode. Bins include values outside the supplied boundaries.
The legend includes empty classes and bins.
Choroplethmap and plotChoroplethMap are aliases for the same function.
}

\seealso{
\code{\link{Worldmap}}
}
\examples{
if (requireNamespace("sf", quietly = TRUE)) {
  # Use two small polygons. This example does not need external data.
  square <- function(x) sf::st_polygon(list(matrix(
    c(x,0, x+1,0, x+1,1, x,1, x,0), ncol=2, byrow=TRUE)))
  shapes <- sf::st_sf(region=c("01067", "01069"),
    geometry=sf::st_sfc(square(0), square(1), crs=4326))
  out <- Choroplethmap(c(3,4), shapes$region,
    PostalCodesShapes=shapes, Categorical=TRUE,
    ClassLabels=c("3"="Promoted", "4"="Sponsors"),
    DiscreteColors=c("3"="red", "4"="magenta"), PlotIt=FALSE)
  out$chorR6obj$render()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{choropleth}% use one of  RShowDoc("KEYWORDS")
\keyword{choropleth map}% __ONLY ONE__ keyword per line
\keyword{political map}% __ONLY ONE__ keyword per line
\keyword{Germany}% __ONLY ONE__ keyword per line
\keyword{zip codes}% __ONLY ONE__ keyword per line
\keyword{postal codes}% __ONLY ONE__ keyword per line
\keyword{thematic map}%

\name{plotChoroplethMap}
\alias{plotChoroplethMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Choropleth Map
}
\description{
A thematic map with areas colored in proportion to the measurement of the statistical variable being displayed on the map. A political map geneated by this function was used in the conference talk of the publication [Thrun/Ultsch, 2018]}
\usage{
plotChoroplethMap(Counts, PostalCodes, NumberOfBins = 0,

 Breaks4Intervals, percentiles = c(0.5, 0.95), 
 
 digits = 0, PostalCodesShapes, PlotIt = TRUE,

 DiscreteColors, HighColorContinuous = "red",

 LowColorContinuous = "deepskyblue1", NAcolor = "grey",

 ReferenceMap = FALSE, main = "Political Map of Germany",

 legend = "Range of values", Silent = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Counts}{
vector [1:m], statistical variable being displayed
}
  \item{PostalCodes}{
vector[1:n], currently german postal codes (zip codes), if \code{PostalCodesShapes} is not changed manually, does not need to be unique
}
  \item{NumberOfBins}{
Default: 1; 1 or below continously changes the color as defined by the   package \code{choroplethr}.
A Number between 2 and 9 sets equally sized bins. Higher numbers are not allowed
}
  \item{Breaks4Intervals}{
If NumberOfBins>1 you can set here the intervals of the bins manually
}
  \item{percentiles}{
If NumberOfBins>1 and Breaks4Intervals not set, then the percentiles of min and max bin can be set here. See also \code{quantile}.
}
  \item{digits}{
number of digits for \code{round}
}
  \item{PostalCodesShapes}{
Specially prepared shape file with postal codes and geographic boundaries.
If you set this object, then you can use non german zip codes.
You can see the required structure in map.df,  github trulia choroplethr blob master r chloropleth. 

}
  \item{PlotIt}{
Either Plot the map directly or change the object manually before plotting it
}
  \item{DiscreteColors}{
Set the discrete colors manually if NumberOfBins>1, else it is ignored
}
  \item{HighColorContinuous}{
if NumberOfBins<=1: color of highest continuous value, else it is ignored
}
  \item{LowColorContinuous}{
if NumberOfBins<=1: color of lowest continuous value, else it is ignored
}
  \item{NAcolor}{
Color of NA values in the map (postal codes without any counts)
}
  \item{ReferenceMap}{
TRUE: With Google map, FALSE: without Google map
}
  \item{main}{
title of plot
}
  \item{legend}{
title of legend
}
  \item{Silent}{
TRUE: disable warnings of \code{choroplethr} package
FALSE: enable  warnings of \code{choroplethr} package
}
}
\details{
The \code{choroplethr} package is not documented well. This wrapper enables  to visualize an map much more easy in the case of german zip codes. Other postal codes are in principle usable.
}
\value{
List of
\item{chorR6obj}{An R6 object of the package \code{choroplethr}}
\item{DataFrame}{Transformed PostalCodes and Counts in a way that they can be used in the package \code{choroplethr}.}
}
\references{
[Thrun/Ultsch, 2018] Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 
}
\author{
Michael Thrun
}
\note{
You could read \url{https://www.r-bloggers.com/case-study-mapping-german-zip-codes-in-r/}, if you want to change the map.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Google \code{choroplethr} package.
}
\examples{
#Many postal codes are required to see a structure
#Exemplary two postal codes in the upper left corner of the map
out=plotChoroplethMap(c(4,8,5,4),

c('49838', '26817', '49838', '26817'),

NumberOfBins=2,PlotIt=FALSE)
\donttest{
out$chorR6obj$render()
}
#bins are only presented in the map if the have values within
out=plotChoroplethMap(c(4,8,5,4),c('49838', '26817',

 '49838', '26817'),NumberOfBins=5,
 
 Breaks4Intervals=c(1,2,3,5,10),PlotIt=FALSE)

\donttest{
out$chorR6obj$render()
}
# Result of [Thrun/Ultsch, 2018]
\donttest{
data('ChoroplethPostalCodesAndAGS_Germany')
res=plotChoroplethMap(as.numeric(ChoroplethPostalCodesAndAGS_Germany$Cls)+1,

ChoroplethPostalCodesAndAGS_Germany$PLZ,NumberOfBins = 2,

Breaks4Intervals = c(0,1,2,3,4,5,6),digits = 1,ReferenceMap = F,

DiscreteColors = c('white','green','blue','red','magenta'),

main = 'Classification of German Postal Codes based on Income Tax Share and Yield',

legend = 'ITS vs MTY Classification in 2010',NAcolor = 'black',PlotIt=FALSE)


#takes time to process
res$chorR6obj$render()
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

\name{ChoroplethPostalCodesAndAGS_Germany}
\alias{ChoroplethPostalCodesAndAGS_Germany}
\docType{data}
\title{
Postal Codes and AGS of Germany for a Choropleth Map
}
\description{
German postal codes, municipality identifiers (AGS), and income-tax classes.
Use these data with Choroplethmap.
The data are a separate GitHub file. Package builds do not include this file.
}
\format{
  A data frame with 8178 unique postal codes and the following variables.
  \describe{
    \item{\code{PLZ}}{Five-character German postal codes.}
    \item{\code{Cls}}{The displayed class: 0 not classified, 1 low quota, 2 high quota, 3 promoted, or 4 sponsors.}
    \item{\code{AGS}}{Eight-character municipality identifiers, or NA.}
    \item{\code{Names}}{The 2010 municipality name if the AGS matches.
Otherwise, this column contains the historical mapping name.}
    \item{\code{SourceCls}}{The 2010 class for the stored AGS.
This value is NA if that AGS does not match a 2010 municipality.}
    \item{\code{ClassSource}}{The source of the displayed class:
source_2010, manual_low_quota, or manual_high_quota.}
    \item{\code{HistoricalCls, HistoricalName}}{The previous class and name.
These columns retain the historical evidence.}
    \item{\code{MappingStatus}}{The key status:
matched_2010_key, missing_ags, or ags_not_in_2010.}
    \item{\code{ClassChanged}}{TRUE for the 54 differences between the historical class and the 2010 class for the stored AGS.}
    \item{\code{ITS, MTY, Ratio}}{Municipality values in EUR per taxpayer,
and 100*ITS/MTY in percent. Missing values remain NA.}
    \item{\code{Year}}{2010.}
    \item{\code{Geography}}{historical_postal_code_approximation.}
  }
}
\details{
The classes compare income tax yield (MTY) with the municipality share (ITS).
Class 3 is promoted (red). Class 4 is sponsors (magenta).
The current display contains the following classes.

\tabular{llllll}{
Class \tab not classified \tab low quota \tab high quota \tab promoted \tab sponsors \cr
Label \tab 0 \tab 1 \tab 2 \tab 3 \tab 4 \cr
CountPerClass \tab 12 \tab 1223 \tab 6850 \tab 9 \tab 84 \cr
}

Of the 8178 postal codes, 7952 have a stored AGS that matches the 2010 source.
Of these matches, 7940 have valid ITS and MTY values.
Twelve have no source measurements and keep class 0.
For 209 postal codes, the stored AGS is absent from the 2010 source.
For 17 postal codes, the AGS is missing.

The user assigned display classes to these 226 postal codes:
195 low quota and 31 high quota.
ClassSource identifies these manual assignments.
Their SourceCls and monetary values remain NA.
Their MappingStatus continues to identify the missing key match.

A missing AGS match does not mean that the historical postal class was missing.
The original preparation also used municipality names and explicit additions.
HistoricalCls retains those classes.
Preparation and audit files remain internal to the dashboard project.

A matching AGS does not confirm the geographical postal-to-municipality assignment.
ITS and MTY describe the associated municipality.
They are not measurements for the entire postal area.
Historical postal areas approximate municipality areas.
}
\source{
Generated for [Thrun/Ultsch, 2018] using the approach of [Ultsch/Behnisch, 2017].
The current table also uses the original 2010 study files.
Download the data from \url{https://github.com/Mthrun/DataVisualizations/tree/master/data}.
Preparation scripts are internal. The public repository and package do not include these scripts.
Package checks do not download data.
}
\references{
[Thrun/Ultsch, 2018] Thrun, M. C., & Ultsch, A. : Effects of the payout system of income taxes to municipalities in Germany, in Papiez, M. & Smiech,, S. (eds.), Proc. 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, pp. 533-542, Cracow: Foundation of the Cracow University of Economics, Cracow, Poland, 2018. 

[Ultsch/Behnisch, 2017]  Ultsch, A., Behnisch, M.: Effects of the payout system of income taxes to municipalities in Germany, Applied Geography, Vol. 81, pp. 21-31, 2017.
}
\examples{
\dontrun{
external <- new.env()
load("ChoroplethPostalCodesAndAGS_Germany.rda", envir=external)
str(external$ChoroplethPostalCodesAndAGS_Germany)
}
}
\keyword{datasets}

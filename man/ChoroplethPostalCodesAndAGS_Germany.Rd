\name{ChoroplethPostalCodesAndAGS_Germany}
\alias{ChoroplethPostalCodesAndAGS_Germany}
\docType{data}
\title{
Postal Codes and AGS of Germany for a Choropleth Map
}
\description{
Zip Codes and Community Identification Number of Germany which can be used in a Choropleth Map.
}
\usage{data("ChoroplethPostalCodesAndAGS_Germany")}
\format{
  A data frame with 8702 observations on the following 4 variables.
  \describe{
    \item{\code{PLZ}}{German postal codes/zip codes}
    \item{\code{Cls}}{Clustering aggregated of germany postal codes by \code{MTY} and \code{ITS} features}
    \item{\code{AGS}}{It is the 'Amtlicher Gemeindeschluessel' (Community Identification Number) of German municipalities }
    \item{\code{Names}}{Names of municipalities}
  }
}
\details{
CLS are the the labels of a MTS versus ITS Bayesian classification showing two main groups of low quota ('1') and high quota ('2') municipalities. Additionally, outliers are manually classified into two separated groups called sponsors ('3') and promoted ('4').
In the Bayesian Classification non classified data have the label '0'. If a 'AGS' code of a 'PLZ' was unclear than the label is 'NaN'.

\tabular{llllllll}{
Class \tab 0 \tab low quota \tab  high quota \tab sponsors \tab promoted \tab non classified  \tab unclear  mapping \cr
Labels \tab 0 \tab 1 \tab 2 \tab 3 \tab 4 \tab 5 \tab NaN \cr
CountPerClass \tab 31 \tab 1325 \tab 7239 \tab  10 \tab 95 \tab 5 \tab 2 \cr
}
}
\source{
Generated for [Thrun/Ultsch, 2018] using the approach of [Ultsch/Behnisch, 2017].
}
\references{
[Thrun/Ultsch, 2018]  Thrun, M. C. & Ultsch A. : Effects of the payout system of income taxes to municipalities in Germany, 12th Professor Aleksander Zelias International Conference on Modelling and Forecasting of Socio-Economic Phenomena, Foundation of the Cracow University of Economics, Zakopane, Poland, 2018. 

[Ultsch/Behnisch, 2017]  Ultsch, A., Behnisch, M.: Effects of the payout system of income taxes to municipalities in Germany, Applied Geography, Vol. 81, pp. 21-31, 2017.
}
\examples{
data(ChoroplethPostalCodesAndAGS_Germany)
str(ChoroplethPostalCodesAndAGS_Germany)
}
\keyword{datasets}

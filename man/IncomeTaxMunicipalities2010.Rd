\name{IncomeTaxMunicipalities2010}
\alias{IncomeTaxMunicipalities2010}
\title{External Keyed Income-Tax Data for German Municipalities in 2010}
\description{
The table contains the 2010 data from the original LRN, NAMES, and CLS study files.
It is a separate GitHub file. Package builds do not include this file.
}
\format{
A table with 11441 rows.
AGS is the eight-character municipality identifier.
Names contains the municipality name in UTF-8.
ITS and MTY contain EUR per taxpayer.
Cls contains 0 not classified, 1 low quota, 2 high quota, 3 promoted, or 4 sponsors.
Ratio contains 100*ITS/MTY in percent. Year contains 2010.
}
\details{
The 11194 rows with positive ITS and MTY values exactly match the package vectors ITS and MTY in the same order.
The other 247 rows have missing source measurements and class 0.
The table keeps the original classes.
It does not calculate new classes from simplified ratio thresholds.
}
\source{
\url{https://github.com/Mthrun/DataVisualizations/tree/master/data}.
The data come from the original study files.
Preparation scripts are internal. The public repository and package do not include these scripts.
}
\references{Thrun and Ultsch (2018), DOI: 10.14659/SEMF.2018.01.54; Ultsch and Behnisch (2017), Applied Geography 81, 21--31.}
\examples{
\dontrun{
  external <- new.env()
  load("IncomeTaxMunicipalities2010.rda", envir=external)
  str(external$IncomeTaxMunicipalities2010)
}
}
\keyword{datasets}

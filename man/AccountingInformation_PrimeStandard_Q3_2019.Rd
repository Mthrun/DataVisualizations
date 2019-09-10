\name{AccountingInformation_PrimeStandard_Q3_2019}
\alias{AccountingInformation_PrimeStandard_Q3_2019}
\docType{data}
\title{
Accounting Information in the Prime Standard in Q3 in 2019 (AI_PS_Q3_2019)
}
\description{
Accounting Information of 261 companies traded in the Frankfurt stock exchange in the German Prime standard.
}
\usage{data("AccountingInformation_PrimeStandard_Q3_2019")}
\format{
  A list with of three objects
  \describe{
    \item{\code{Key}}{[1:n] Key of the 261 obeservations }
    \item{\code{Data}}{[1:n,1:d] numeric matrix of 261 observations on the 45 variables describing the accounting information}
    \item{\code{Cls}}{[1:n] a numeric vector of k clusters of the clustering performend in [Thrun/Ultsch, 2019]}
  }
}
\details{
Detailed data description can be found in [Thrun/Ultsch, 2019].
}
\source{
Yahoo Finance 
}
\references{
[Thrun/Ultsch, 2019]  Thrun, M. C., & Ultsch, A.: Stock Selection via Knowledge Discovery using Swarm Intelligence with Emergence, IEEE Intelligent Systems, Vol. under review, pp., 2019.


}
\examples{
data(AccountingInformation_PrimeStandard_Q3_2019)

str(AI_PS_Q3_2019)
dim(AI_PS_Q3_2019$Data)
}
\keyword{datasets}

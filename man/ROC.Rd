\name{ROC}
\alias{ROC}
\title{ROC plot}
\description{
Receiver operating characteristic curve
}
\usage{
ROC(Data, Cls, Names, Colors)
}
\arguments{
\item{Data}{[1:n, 1:d] numeric vector or matrix of scores to be evaluated with
ROC.}
\item{Cls}{[1:n] numeric vector with true classes.}
\item{Names}{[1:d] character vector with names for scores.}
\item{Colors}{[1:d] character vector with colores for scores.}
}
\value{
\item{ROCit}{List of ROCit results for each score column in Data.}
\item{Plot}{Plotly object.}
}
\author{Quirin Stier}
\examples{

Data = runif(1000,0,1)
Cls  = sample(c(0,1), 1000, replace = TRUE)
ROC(Data, Cls)

}
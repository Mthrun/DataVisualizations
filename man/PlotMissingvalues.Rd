\name{PlotMissingvalues}
\alias{PlotMissingvalues}
\title{Plot of the Amount Of Missing Values}
\usage{
PlotMissingvalues(Data,Names,

WhichDefineMissing=c('NA','NaN','DUMMY','.',' '),

PlotIt=TRUE,

xlab='Amount Of Missing Values in Percent',

xlim=c(0,100),...)
}
\arguments{
\item{Data}{[1:n,1:d] data cases in rows, variables/features in columns}
\item{Names}{[1:d] optional vector of string describing the names of the features}
\item{WhichDefineMissing}{[1:d] optional vector of string describing missing values, usefull for character features. Currently up to five different options are possible.}
\item{PlotIt}{If FALES: Does not plot}
\item{xlab}{x label of bar plot}
\item{xlim}{x axis limits in percent}
\item{\dots}{Further arguments passed on to \code{barplot}, such as \code{main} for title}
}
\description{
Percentage of missing values per feature are visualized as a bar plot.
}
\value{
plots not finite and missing values as a bar plot for each feature d
and returns with \code{invisible} the amount of missing values as a vector. Works even with character variables, but WhichDefineMissing cannot be changed at the current version. Please make a suggestion on GitHub how to improve this.
}
\author{
Michael Thrun
}
\note{
 Does not work with the tibble format, in such a case please call \code{as.data.frame(as.matrix(Data))}
}
\examples{
data("ITS")
data("MTY")

PlotMissingvalues(cbind(ITS,MTY),Names=c('ITS','MTY'))

}
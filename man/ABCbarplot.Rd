\name{ABCbarplot}
\alias{ABCbarplot}
\alias{ABC_screeplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Barplot with Sorted Data Colored by ABCanalysis
}
\description{
This plot can be read like a scree plot for PCA. It allowed to select the most important values visually.
}
\usage{
ABCbarplot(Data,

Colors=DataVisualizations::DefaultColorSequence[1:3],

main,xlab="Fraction of Data in \%",ylab="Value")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n] vector of Data, e.g. eigenvalues of PCA}
\item{Colors}{three colors for A, B and C}
\item{main}{title of plot}
\item{xlab}{xlabel}
\item{ylab}{ylabel}
}
\details{
ABC analysis is explained in \pkg{ABCanalysis}. The visualization is based on \pkg{ggplot2}.
}
\value{
List V of
    \item{ABCanalysis}{output of \pkg{ABCanalysis}}
        \item{ggobject}{object of \pkg{ggplot2} plotted}
          \item{DF}{Data frame if another plot should be done manually}
}
\references{
Ultsch. A ., Lotsch J.: Computed ABC Analysis for Rational Selection of Most Informative Variables in Multivariate Data, PloS one, Vol. 10(6), pp. e0129767. doi 10.1371/journal.pone.0129767, 2015.

}
\author{
Michael Thrun
}


\seealso{
\code{\link[ABCanalysis]{ABCanalysis}}
}
\examples{

data('FundamentalData_Q1_2018')
Data=as.matrix(FundamentalData_Q1_2018$Data)
Data[!is.finite(Data)]=0
results=prcomp(Data)
main="Scree plot with Class A of the Most-Important Eigenvalues"
plotlist = ABCbarplot(results$sdev,ylab='Eigenvalues',main=main)
\donttest{plotlist$ggobject}
}

\keyword{scree}
\keyword{scree plot}
\keyword{bar}
\keyword{barplot}
\keyword{bar plot}
\keyword{geom_bar}
\keyword{ABCbarplot}
\keyword{ABC_screeplot}
\keyword{ABC barplot}
\keyword{ABC screeplot}

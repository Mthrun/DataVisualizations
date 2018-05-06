\name{MAplot}
\alias{MAplot}
\title{
Minus versus Add plot
}
\description{
Bland-Altman plot [Altman/Bland, 1983].
}
\usage{
MAplot(x,y,islog=TRUE,densityplot=FALSE,

main='MA-plot',xlab,ylab,Cls)
}
\arguments{
  \item{x}{[1:n] numerical vector of a feature/variable
}
  \item{y}{[1:n] another numerical vector of a feature/variable
}
  \item{islog}{ TRUE: MAplot, FALSE: M=x-y versus a=0.5(x+y)
}
  \item{densityplot}{ FALSE: Scatterplot, TRUE: density scatter plot with PDE
}
  \item{main}{see \code{plot}
}  
  \item{xlab}{  see \code{plot}
}
  \item{ylab}{
  see \code{plot}
}
  \item{Cls}{
	prior Classification as a numeric vector.
}
}
\details{
Bland-Altman plot [Altman/Bland, 1983] for visual representation of genomic data or in order to decorrelate data.
}
\value{
  \item{MA}{[1:n,2] Matrix of Minus component of two features and Add component of two features
}
  \item{ggplot}{see \code{ggplot2} output, if densityplot=TRUE, else NULL
}
}
\references{
[Altman/Bland, 1983]  Altman D.G., Bland J.M.: Measurement in medicine: the analysis of method comparison studies, The Statistician, Vol. 32, p. 307-317, doi:10.2307/2987937, 1983.

[Ultsch, 2005]  Ultsch, A.: Pareto Density Estimation: A Density Estimation for Knowledge Discovery, Baier D., Wernecke K.D. (Eds), In Innovations in Classification, Data Science, and Information Systems - Proceedings 27th Annual Conference of the German Classification Society (GfKL) 2003, Berlin, Heidelberg, Springer, pp, 91-100, 2005.
}
\author{
Michael Thrun
}
\examples{
#taken from [Thrun/Ultsch, 2018]
data("ITS")
data("MTY")
MAlist=MAplot(ITS,MTY)
}
\keyword{MAplot}
\keyword{MA}
\keyword{MA plot}
\keyword{Bland-Altman plot}
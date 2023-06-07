\name{Pixelmatrix}
\alias{Pixelmatrix}
\alias{PlotPixMatrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot of a Pixel Matrix }
\description{
  Plots Data matrix as a pixel coulour image.
}
\usage{
Pixelmatrix(Data, XNames, LowLim, HiLim,

YNames, main,FillNotFiniteWithHighestValue=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ [1:n,1:d] Data cases in rows (n), variables in columns (d) }
  \item{LowLim}{ Optional: limits for the color axis }
  \item{HiLim}{ Optional: limits for the color axis }
  \item{XNames}{ Optional: Vector - names for the X-ticks, NULL: no ticks at all }
  \item{YNames}{ Optional: Vector - names for the Y-ticks, NULL: no ticks at all }
  \item{main}{Optoinal: String - Title of the plot}
    \item{FillNotFiniteWithHighestValue}{Optional: TRUE: fills not finite values with same color as the highest value}
}
\details{
Low values are shown in blue and green, middle values in yellow and high values in orange and red.
}
\author{Michael Thrun, Felix Pape}
\examples{
data("Lsun3D")
Data=Lsun3D$Data
\donttest{
Pixelmatrix(Data)
}
\dontshow{
Pixelmatrix(Data[1:100,])
}
}
\keyword{Pixelmatrix}
\keyword{PixelMatrixPlot}

\keyword{heatmap}
\keyword{pixel matrix}
\keyword{image}
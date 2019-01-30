\name{Pixelmatrix}
\alias{Pixelmatrix}
\alias{PlotPixMatrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot of a Pixel Matrix }
\description{
  Plots Data matrix as a pixel coulour image.
}
\usage{
Pixelmatrix(Data, XNames = NULL, LowLim, HiLim,

YNames = NULL, main = '',FillNotFiniteWithHighestValue=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ [1:n,1:d] Data cases in rows (n), variables in columns (d) }
  \item{LowLim}{ Optional: limits for the color axis }
  \item{HiLim}{ Optional: limits for the color axis }
  \item{XNames}{ Optional: Vector - names for the X-ticks }
  \item{YNames}{ Optional: Vector - names for the Y-ticks }
  \item{main}{Optinal: String - Title of the plot}
   \item{FillNotFiniteWithHighestValue}{Optinal, Default FALSE = Non finite values are shown in black, TRUE=non finite values are transformed to a value higher than the highest value and shown in this color}
  
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
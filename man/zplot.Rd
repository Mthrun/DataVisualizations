\name{zplot}
\alias{zplot}
\title{
Plotting for 3 dimensional data
}
\description{
Plots z above xy plane as 3D mountain or 2D contourlines
}
\usage{
zplot(x, y, z, DrawTopView = TRUE, NrOfContourLines = 20,

                 TwoDplotter = "native", xlim, ylim)
}
\arguments{
  \item{x}{
    Vector of x-coordinates of the data. If y and z are missing: Matrix containing 3 rows, one for each coordinate
  }
  \item{y}{
    Vector of y-coordinates of the data.
  }
  \item{z}{
    Vector of z-coordinates of the data.
  }
  \item{DrawTopView}{
    Optional: Boolean, if true plot contours otherwise a 3D plot. Default: True
  }
  \item{NrOfContourLines}{
    Optional: Numeric. Only used when DrawTopView == True. Number of lines to be drawn in 2D contour plots. Default: 20
  }
  \item{TwoDplotter}{
    Optional: String indicating which backend to use for plotting. Possible Values: 'ggplot', 'native', 'plotly'
  }
    \item{xlim}{
  [1:2] scalar vector setting the limits of x-axis
  }
    \item{ylim}{
   [1:2] scalar vector setting the limits of y-axis
  }
}
\value{
If the plotting backend does support it, this will return a handle for the generated plot. 
}
\examples{

\dontshow{
data("Lsun3D")
Data=Lsun3D$Data
zplot(Data[,1],Data[,2],Data[,3])
}
}
\author{
Felix pape
}
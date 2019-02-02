\name{GoogleMapsCoordinates}
\alias{GoogleMapsCoordinates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Google Maps with marked coordinates 
}
\description{
Google Maps with marked coordinates.
}
\usage{
GoogleMapsCoordinates(Longitude,Latitude,Cls=rep(1,length(Longitude)),
zoom=3,location= c(mean(Longitude),mean(Latitude)),stroke=1.7,size=6,sequence)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Longitude}{ sphaerischer winkel der Kugeloberflaeche, coord 1
}
  \item{Latitude}{sphaerischer winkel der Kugeloberflaeche,  coord 2
}
  \item{Cls}{Vorklassification/Clusterung
}
  \item{zoom}{
   map zoom, an integer from 3 (continent) to 21 (building), default value 10 (city). 
                           openstreetmaps limits a zoom of 18, and the limit on stamen maps depends on the maptype. 
                         "auto" automatically determines the zoom for bounding box specifications, and is defaulted to 10
                           with center/zoom specifications. maps of the whole world currently not supported
}
  \item{location}{Optional, 
  default: c(mean(Longitude),mean(Latitude); an address, longitude/latitude pair (in that order), 
                          or left/bottom/right/top bounding box
}
  \item{stroke}{Optional,  plotting parameter, dicke der linien der coordiantensymbole
}
  \item{size}{Optional, plotting parameter, groesse der koordinatensymbole
}
  \item{sequence}{Optional, vector of length of number of clusers with numbers indicating the plotting symbols and colors to use
}
}

\value{
ggobject()
}
\details{
This plot was used in [Thrun, 2018, p. 135].
}
\note{
requires an Internet connection,
requires an API key of Google.
See \code{?ggmap::register_google} for details.
}
\author{
Michael Thrun
}
\references{
[Thrun, 2018] Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, ISBN: 978-3-658-20539-3, Heidelberg, 2018.
}
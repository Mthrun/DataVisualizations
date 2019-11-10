\name{JitterUniqueValues}
\alias{JitterUniqueValues}
\title{
Jitters Unique Values
}
\description{
Jitters Unique Values for Visualizations
}
\usage{
JitterUniqueValues(Data, Npoints = 20,

min = 0.99999, max = 1.00001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n] vector of data
}
  \item{Npoints}{
number of jittered points generated from the m unique values of the datavector Data
}
  \item{min}{
minimum value of jittering
}
  \item{max}{
maximum value of jittering
}
}
\details{
min and max are either multiplied or added to data depending on the range of values. If Npoints==2, then only two  values per unique of Data is jittered otherwise additional values are generated.Npoints==1 does not jitter the values but gives the unique values back.
}
\value{
vector of DataJitter[1:(m+Npoints-1)] jittered values
}

\author{
Michael Thrun
}


\seealso{
used for example in \code{\link{MDplot}}
}
\examples{
data=c(rep(1,10),rep(0,10),rep(100,10))

JitterUniqueValues(data,Npoints=1)

JitterUniqueValues(data,Npoints=2)

DataJitter=JitterUniqueValues(data,Npoints=20)
}
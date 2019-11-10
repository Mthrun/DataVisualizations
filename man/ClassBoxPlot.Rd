\name{ClassBoxplot}
\alias{ClassBoxplot}
\title{
 Creates Boxplot plot for all classes
}
\description{
 Boxplot the data for all classes
}
\usage{
ClassBoxplot(Data, Cls,  ColorSequence = DataVisualizations::DefaultColorSequence,

 ClassNames = NULL,All=FALSE, PlotLegend = TRUE,

 main = 'Boxplot per Class', xlab = 'Classes', ylab = 'Range of Data')
}
\arguments{
  \item{Data}{
  Vector of the data to be plotted
  }
  \item{Cls}{
  Vector of class identifiers.
  }
  \item{ColorSequence}{
  Optional: The sequence of colors used, Default: DefaultColorSequence()
  }
  \item{ClassNames}{
  Optional: The names of the classes. Default: C1 - C(Number of Classes)
  }
   \item{All}{
	Optional: adds full data vector for comparison against classes
  }
  \item{PlotLegend}{
  Optional: Add a legent to plot. Default: TRUE)
  }
  \item{main}{
  Optional: Title of the plot. Default: "ClassBoxPlot""
  }
  \item{xlab}{
  Optional: Title of the x axis. Default: "Classes"
  }
  \item{ylab}{
  Optional: Title of the y axis. Default: "Data"
  }
}
\value{
  A List of
  \item{ClassData}{The DataFrame used to plot}
  \item{ggobject}{The ggplot2 plot object}
    in mode \code{invisible}
}
\author{
  Michael Thrun, Felix Pape
}
\examples{
\dontshow{
data(ITS)
#clustering only for testing
Classification=c(rep(1,8805),rep(2,2389))
DataVisualizations::ClassBoxplot(ITS,Classification)
}

\donttest{
data(ITS)
#please download package from cran
#model=AdaptGauss::AdaptGauss(ITS)
#Classification=AdaptGauss::ClassifyByDecisionBoundaries(ITS,

#DecisionBoundaries = AdaptGauss::BayesDecisionBoundaries(model$Means,model$SDs,model$Weights))

DataVisualizations::ClassBoxplot(ITS,Classification)$ggobject
}
}
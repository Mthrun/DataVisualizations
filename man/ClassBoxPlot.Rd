\name{ClassBoxPlot}
\alias{ClassBoxPlot}
\title{
 Creates BoxPlot plot for all classes
}
\description{
 BoxPlot the data for all classes
}
\usage{
ClassBoxPlot(Data, Cls,  ColorSequence = DataVisualizations::DefaultColorSequence,

 ClassNames = NULL, PlotLegend = TRUE,

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
}
\author{
  Michael Thrun, Felix Pape
}
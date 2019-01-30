\name{ClassMDplot}
\alias{ClassMDplot}
\title{
Class MD-plot for Data w.r.t. all classes
}
\description{
  Creates PDE optimized Violin plot for all classes 
}
\usage{
ClassMDplot(Data, Cls, ColorSequence = DataVisualizations::DefaultColorSequence,

 ClassNames = NULL,

 PlotLegend = TRUE,

 main = 'PDE Violin Plot for each Class',

 xlab = 'Classes',

 ylab = 'PDE of Data per Class')
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
  Optional: Title of the plot. Default: "ClassViolinPlot""
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
\references{
Thrun, M. C., Pape, F., & Ultsch, A. : Benchmarking Cluster Analysis Methods using PDE-Optimized Violin Plots, Proc. European Conference on Data Analysis (ECDA), accepted, Paderborn, Germany, 2018. 

Thrun, M. C., Breuer, L., & Ultsch, A. : Knowledge discovery from low-frequency stream nitrate concentrations: hydrology and biology contributions, Proc. European Conference on Data Analysis (ECDA), accepted, Paderborn, Germany, 2018. 
}
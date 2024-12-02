\name{ClassBarPlot}
\alias{ClassBarPlot}
\title{ClassBarPlot}
\description{
Represent values for each class and instance as bar plot with optional error
deviation, e.g., mean values of features depending on class with standard
deviation.
}
\usage{
ClassBarPlot(Values, Cls, Deviation, Names, ylab = "Values",
xlab = "Instances", PlotIt = TRUE)
}
\arguments{
\item{Values}{[1:n] Numeric vector with values (y-axis) in matching order to 
Cls, Deviation and Names.}
\item{Cls}{[1:n] Numeric vector of classes in matching order to Values and
Deviation and Names.}
\item{Deviation}{[1:n] Numeric vector with deviation in matching order to Values
and Cls and Names.}
\item{Names}{[1:n] Character or numeric vector of instances (x-axis) in matching
order to Values and Cls and Deviation.}
\item{ClassColors}{Character vector of color names stating either the colors
for each class or defining colors matching the class vector cls.}
\item{ylab}{Character stating y label.}
\item{xlab}{Character stating x label.}
\item{PlotIt}{Logical value indicating visual output TRUE => create visual
output FALSE => do not create visual output (Default: Boolean=TRUE).}
}
\value{
ggplot2 object
}
\author{
Quirin Stier
}
\examples{

library(ggplot2)

if(require(dplyr)){
  tmpVar1 = iris %>% group_by(Species) %>% summarise(mean = mean(.data[["Sepal.Length"]]), n = n())
  tmpVar2 = iris %>% group_by(Species) %>% summarise(mean = mean(.data[["Sepal.Width"]]), n = n())
  tmpVar3 = iris %>% group_by(Species) %>% summarise(mean = mean(.data[["Petal.Length"]]), n = n())
  tmpVar4 = iris %>% group_by(Species) %>% summarise(mean = mean(.data[["Petal.Width"]]), n = n())
  
  tmpVar5 = iris %>% group_by(Species) %>% summarise(sd = sd(.data[["Sepal.Length"]]), n = n())
  tmpVar6 = iris %>% group_by(Species) %>% summarise(sd = sd(.data[["Sepal.Width"]]), n = n())
  tmpVar7 = iris %>% group_by(Species) %>% summarise(sd = sd(.data[["Petal.Length"]]), n = n())
  tmpVar8 = iris %>% group_by(Species) %>% summarise(sd = sd(.data[["Petal.Width"]]), n = n())
  
  Values    = c(tmpVar1$mean, tmpVar2$mean, tmpVar3$mean, tmpVar4$mean)
  Class     = rep(1:3, 4)
  Deviation = c(tmpVar5$sd, tmpVar6$sd, tmpVar7$sd, tmpVar8$sd)
  
  if(length(Values) == length(Class)){
    ClassBarPlot(Values = Values, Cls = Class, Deviation = Deviation)
  }
  
}

}
\keyword{ClassBarPlot}% use one of  RShowDoc("KEYWORDS")
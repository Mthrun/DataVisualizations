\name{ClassBarPlot}
\alias{ClassBarPlot}
\title{ClassBarPlot}
\description{
Represent values for each class and instance as bar plot with optional error
deviation, e.g., mean values of features depending on class with standard
deviation.
}
\usage{
ClassBarPlot(Values, Class, Deviation, Names, ylab = "Values",
xlab = "Instances", PlotIt = TRUE)
}
\arguments{
\item{Values}{[1:n] Numeric vector with values (y-axis) in matching order to 
Class, Deviation and Names.}
\item{Class}{[1:n] Numeric vector of classes in matching order to Values and
Deviation and Names.}
\item{Deviation}{[1:n] Numeric vector with deviation in matching order to Values
and Class and Names.}
\item{Names}{[1:n] Character or numeric vector of instances (x-axis) in matching
order to Values and Class and Deviation.}
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

library(dplyr)
library(ggplot2)

tmpVar1 = iris %>% group_by(Species) %>% summarise(mean = mean(.data[["Sepal.Length"]]), n = n())
tmpVar2 = iris %>% group_by(Species) %>% summarise(mean = mean(.data[["Sepal.Width"]]), n = n())
tmpVar3 = iris %>% group_by(Species) %>% summarise(mean = mean(.data[["Petal.Length"]]), n = n())
tmpVar4 = iris %>% group_by(Species) %>% summarise(mean = mean(.data[["Petal.Width"]]), n = n())

tmpVar5 = iris %>% group_by(Species) %>% summarise(sd = sd(.data[["Sepal.Length"]]), n = n())
tmpVar6 = iris %>% group_by(Species) %>% summarise(sd = sd(.data[["Sepal.Width"]]), n = n())
tmpVar7 = iris %>% group_by(Species) %>% summarise(sd = sd(.data[["Petal.Length"]]), n = n())
tmpVar8 = iris %>% group_by(Species) %>% summarise(sd = sd(.data[["Petal.Width"]]), n = n())

Values = c(tmpVar1$mean, tmpVar2$mean, tmpVar3$mean, tmpVar4$mean)
Class = rep(1:3, 4)
Deviation = c(tmpVar5$sd, tmpVar6$sd, tmpVar7$sd, tmpVar8$sd)

ClassBarPlot(Values = Values, Class = Class, Deviation = Deviation)

}
\keyword{ClassBarPlot}% use one of  RShowDoc("KEYWORDS")